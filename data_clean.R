# Data Analysis -----------------------------------------------------------
library(tidyverse)
library(rio)
library(openxlsx)
library(ggplot2)
library(mice)
library(gridExtra)
library(readr)
## ----Get cohorts---------------------------------------------------------------------------------------------
cohort <- import("data/Subject IDs by MAD cohort 2024-05-23.xlsx")


## ----First dose datetime, take baseline labs as last value prior to first dose-------------------------------
#names(ex_edc); unique(ds_edc$`Cohort: (display value) (COHORT_DEC)`)
ex_edc <- import("data/ex.xlsx") |>
  filter(str_detect(`Visit Name (VISNAME)`,"Day 1")) |>
  filter(!str_detect(`Visit Name (VISNAME)`,"Day 14")) |>
  filter(`Was dose taken? (EXYN)` == "Y") |>
  left_join(cohort, by=c("Subject Identifier (SUBNUM)"="Subject ID Assigned")) |>
  select(-c(`Site Number (SITENUM)`, `Internal Subject Id (SUBID)`,`Visit Id (VISITID)`:`Page Not Done (EXND)`,
            `Was dose taken? (display value) (EXYN_DEC)`,
            `If No, reason missed: (EXREASND)`:`Marked Deleted (DELETED)`)) |>
  unique() |>
  arrange(`Strata Group`, `Subject Identifier (SUBNUM)`) |>
  mutate(doublet_ind = case_when(
    `Subject Identifier (SUBNUM)` == lag(`Subject Identifier (SUBNUM)`) ~ 1,
    TRUE ~ 0
  )) |>
  filter(doublet_ind == 0) |>
  select(`Subject Identifier (SUBNUM)`, `Date: (EXSTDAT)`, `Time: (EXSTTIM)`)

# names(ex_edc_db2); unique(ds_edc$`Cohort: (display value) (COHORT_DEC)`)
ex_edc_db2 <- import("data/ex DB2.xlsx") |>
  filter(str_detect(`Visit Name (VISNAME)`,"Day 1")) |>
  filter(!str_detect(`Visit Name (VISNAME)`,"Day 14")) |>
  filter(`Was dose taken? (EXYN)` == "Y")|>
  select(`Subject Identifier (SUBNUM)`, `Date: (EXSTDAT)`, `Time: (EXSTTIM)`)

first_dose <- bind_rows(ex_edc, ex_edc_db2) |>
  right_join(cohort, by=c("Subject Identifier (SUBNUM)"="Subject ID Assigned"))


## ----OGTT start times----------------------------------------------------------------------------------------

OGTT_times_db1 <- import("data/ag.xlsx") |>
  inner_join(cohort, by=c("Subject Identifier (SUBNUM)"="Subject ID Assigned")) |>
  select(`Subject Identifier (SUBNUM)`,
         `Visit Name (VISNAME)`,
         `OGTT Procedure Date: (AGSTDAT)`,
         `Glucose Consumption Start Time: (AGSTTIM)`) |>
  filter(!is.na(`Glucose Consumption Start Time: (AGSTTIM)`)) |>
  mutate(Visit = ifelse(str_detect(`Visit Name (VISNAME)`, "4 \\(M"), "Day 14", `Visit Name (VISNAME)`))

OGTT_times_db2 <- import("data/ag DB2.xlsx") |>
  inner_join(cohort, by=c("Subject Identifier (SUBNUM)"="Subject ID Assigned")) |>
  select(`Subject Identifier (SUBNUM)`,
         `Visit Name (VISNAME)`,
         `OGTT Procedure Date: (AGSTDAT)`,
         `Glucose Consumption Start Time: (AGSTTIM)`) |>
  filter(!is.na(`Glucose Consumption Start Time: (AGSTTIM)`)) |>
  mutate(Visit = ifelse(str_detect(`Visit Name (VISNAME)`, "4 \\(M"), "Day 14", `Visit Name (VISNAME)`))

OGTT_times <- bind_rows(OGTT_times_db1, OGTT_times_db2) |>
  select(-`Visit Name (VISNAME)`)

# rm(OGTT_times_db1, OGTT_times_db2)


## ----PPD CL data---------------------------------------------------------------------------------------------
PPD_lb_tnsf_gluc_insulin <- import("data/glucose insulin cpeptide all MAD cohorts 2024-05-23.xlsx") |>
  rename(`Subject level ID or Number (SUBJID)`=`Subject ID or Number (SUBJID)`,
         `lab date (lbdt)`=`LBDT (LBDT)`,
         `lab time (lbtm)`=`LBTM (LBTM)`,
         `Received Date (RCVDTM)`=`Received Date and Time (RCVDTM)`) |>
  select(`Study ID or Number (STUDYID)`,
         `Subject level ID or Number (SUBJID)`,
         `Subject Sex (SEX)`,
         `Subject Age at Collection (AGEATCOL)`,
         `Subject Race (RACE)`,
         `Visit Name (VISIT)`,
         `Accession ID or Number (ACCSNNUM)`,
         `Specimen ID or Number (SPECNUM)`,
         `lab date (lbdt)`,
         `lab time (lbtm)`,
         `Received Date (RCVDTM)`,
         `Specimen Condition (SPECCND)`,
         `Lab - Specimen Comments (SPECCOM)`,
         `Specimen Material Name (LBSPEC)`,
         `Fasting Status (FASTSTAT)`,
         `Battery ID (BATTRID)`,
         `Lab Test ID (LBTESTCD)`,
         `Lab Test Name (LBTEST)`,
         `Test ID (TSTCD)`,
         `Test Name ID (TSTNAM)`:`Transaction Type (TRNSTYP)`
  ) |>
  mutate(`lab date` = ymd(`lab date (lbdt)`))

# unique(PPD_lb_tnsf_gluc_insulin$`Lab Test Name (LBTEST)`)


## ----Begin Cpeptide work filter PPD CL to cpeptide, include=FALSE--------------------------------------------
PPD_lb_tnsf_cpeptide <- PPD_lb_tnsf_gluc_insulin |> # names(PPD_lb_tnsf_gluc_insulin)
  filter(`Lab Test Name (LBTEST)` == "C Peptide") |>
  select(`Subject level ID or Number (SUBJID)`,
         `Visit Name (VISIT)`,
         `Accession ID or Number (ACCSNNUM)`,
         `Specimen ID or Number (SPECNUM)`,
         `lab date (lbdt)`,
         `lab time (lbtm)`,
         `Received Date (RCVDTM)`,
         `Specimen Condition (SPECCND)`,
         `Lab - Specimen Comments (SPECCOM)`,
         `Specimen Material Name (LBSPEC)`,
         `Lab Test Name (LBTEST)`,
         `Test Name ID (TSTNAM)`,
         `Additional Test Description (TSTDESC)`,
         `Reported Numeric Result (RPTRESN)`,
         `Reported Units (RPTU)`,
         `lab date`)
# head(PPD_lb_tnsf_cpeptide)


## ----All cpeptide around the OGTT test ,include=FALSE--------------------------------------------------------
all_dat_cpep <- OGTT_times |> # filtering cpeptide data to OGTT collection times
  left_join(PPD_lb_tnsf_cpeptide, by=c("Subject Identifier (SUBNUM)"="Subject level ID or Number (SUBJID)",
                                       "OGTT Procedure Date: (AGSTDAT)"="lab date")
  ) |>
  mutate(`OGTT datetime` = ymd_hms(paste0(as.character(`OGTT Procedure Date: (AGSTDAT)`)," ",`Glucose Consumption Start Time: (AGSTTIM)`)),
         `Lab datetime` = ymd_hm(paste0(as.character(`lab date (lbdt)`)," ",`lab time (lbtm)`)),
         `Lab time rel OGTT` = difftime(`Lab datetime`,  `OGTT datetime`, units = "mins"),
         `pre-post OGTT` = ifelse(`Lab time rel OGTT`<=0,
                                  "Pre OGTT start",
                                  "Post OGTT start"),
         `Visit label` = factor(Visit,
                                levels = c("Day -1", "Day 7", "Day 14", "Day 21", "Day 28", "Week 8",
                                           "Week 12", "Week 16", "Week 20", "Week 26", "Unscheduled")
         )
  ) |>
  relocate(c(`OGTT datetime`, `pre-post OGTT`), .after = `Visit Name (VISIT)`) |>
  filter(!is.na(`Reported Numeric Result (RPTRESN)`)) |>
  arrange(`Subject Identifier (SUBNUM)`,
          `Visit label`,
          `Additional Test Description (TSTDESC)`) |>
  filter(!`Additional Test Description (TSTDESC)` %in% c("OGTT C Peptide 10hr","OGTT C Peptide 6hr",
                                                         "C Peptide","C Peptide 6hr","C Peptide 8hr","C Peptide 10hr"))
all_dat_cpep <- all_dat_cpep %>%
  mutate(`Lab time rel OGTT` = as.numeric(`Lab time rel OGTT`, units = "mins"))
## ----Begin glucose work filter PPD CL to glucose , include=FALSE---------------------------------------------
PPD_lb_tnsf_glucose <- PPD_lb_tnsf_gluc_insulin |> # names(PPD_lb_tnsf_gluc_insulin)
  filter(`Lab Test Name (LBTEST)` %in% c("Glucose", "Glucose, Random")) |>
  filter(!`Additional Test Description (TSTDESC)` == "Chemistry") |>
  select(`Subject level ID or Number (SUBJID)`,
         `Visit Name (VISIT)`,
         `Accession ID or Number (ACCSNNUM)`,
         `Specimen ID or Number (SPECNUM)`,
         `lab date (lbdt)`,
         `lab time (lbtm)`,
         `Received Date (RCVDTM)`,
         `Specimen Condition (SPECCND)`,
         `Lab - Specimen Comments (SPECCOM)`,
         `Specimen Material Name (LBSPEC)`,
         `Lab Test Name (LBTEST)`,
         `Test Name ID (TSTNAM)`,
         `Additional Test Description (TSTDESC)`,
         `Reported Numeric Result (RPTRESN)`,
         `Reported Units (RPTU)`,
         `lab date`)

## ----All cpeptide around the OGTT test, include=FALSE--------------------------------------------------------
all_dat_gluc <- OGTT_times |> # filtering cpeptide data to OGTT collection times
  left_join(PPD_lb_tnsf_glucose, by=c("Subject Identifier (SUBNUM)"="Subject level ID or Number (SUBJID)",
                                      "OGTT Procedure Date: (AGSTDAT)"="lab date")
  ) |>
  mutate(`OGTT datetime` = ymd_hms(paste0(as.character(`OGTT Procedure Date: (AGSTDAT)`)," ",`Glucose Consumption Start Time: (AGSTTIM)`)),
         `Lab datetime` = ymd_hm(paste0(as.character(`lab date (lbdt)`)," ",`lab time (lbtm)`)),
         `Lab time rel OGTT` = difftime(`Lab datetime`,  `OGTT datetime`, units = "mins"),
         `pre-post OGTT` = ifelse(`Lab time rel OGTT`<=0,
                                  "Pre OGTT start",
                                  "Post OGTT start"),
         `Visit label` = factor(ifelse(str_detect(`Visit`, "MAD2"),
                                       "Day 14",
                                       `Visit`),
                                levels = c("Day -1", "Day 7", "Day 14", "Day 21", "Day 28",  "Week 8",
                                           "Week 12", "Week 16", "Week 20", "Week 26")
         )
  ) |>
  relocate(c(`OGTT datetime`, `pre-post OGTT`), .after = `Visit Name (VISIT)`) |>
  filter(!is.na(`Reported Numeric Result (RPTRESN)`)) |>
  arrange(`Subject Identifier (SUBNUM)`,
          `Visit label`,
          `Additional Test Description (TSTDESC)`) |>
  filter(!`Additional Test Description (TSTDESC)` %in% c("Plasma Glucose 6hr","Plasma Glucose 8hr","Plasma Glucose 10hr",
                                                         "Plasma Glucose 12hr","Plasma Glucose 14hr","Plasma Glucose 16hr","Plasma Glucose 26hr",
                                                         "OGTT Plasma Glucose 6hr","OGTT Plasma Glucose 10hr","C Peptide 8hr","C Peptide 10hr"))
all_dat_gluc <- all_dat_gluc %>%
  mutate(`Lab time rel OGTT` = as.numeric(`Lab time rel OGTT`, units = "mins"))

## ----include=FALSE-------------------------------------------------------------------------------------------
data_cpep = data.frame(
  subject = all_dat_cpep$`Subject Identifier (SUBNUM)`,
  visit = all_dat_cpep$Visit,
  time = all_dat_cpep$`Additional Test Description (TSTDESC)`,
  treatment = all_dat_cpep$`pre-post OGTT`,
  followup = all_dat_cpep$`Lab time rel OGTT`,
  result = all_dat_cpep$`Reported Numeric Result (RPTRESN)`
)

## ----sorting C-Peptide data for Imputation-------------------------------------------------------------------
data_cpep$time = case_when(
  data_cpep$time == "C Peptide 2hr" ~ "OGTT C Peptide 0 Min",
  data_cpep$time == "C Peptide 2.5hr" ~ "OGTT C Peptide 30 Min",
  data_cpep$time == "C Peptide 3hr" ~ "OGTT C Peptide 1hr",
  data_cpep$time == "C Peptide 3.5hr" ~ "OGTT C Peptide 1.5hr",
  data_cpep$time == "C Peptide 4hr" ~ "OGTT C Peptide 2hr",
  TRUE ~ data_cpep$time
)
data_cpep = data_cpep |>
  filter(time %in% c("OGTT C Peptide 0 Min", "OGTT C Peptide 30 Min", "OGTT C Peptide 1hr", "OGTT C Peptide 1.5hr", "OGTT C Peptide 2hr")) |>
  filter(!visit %in% c("Day 7", "Day 21", "Unscheduled"))
data_cpep$time = case_when(
  data_cpep$time == "OGTT C Peptide 0 Min" ~ "0hr",
  data_cpep$time == "OGTT C Peptide 30 Min" ~ "0.5hr",
  data_cpep$time == "OGTT C Peptide 1hr" ~ "1hr",
  data_cpep$time == "OGTT C Peptide 1.5hr" ~ "1.5hr",
  data_cpep$time == "OGTT C Peptide 2hr" ~ "2hr",
  TRUE ~ data_cpep$time
)
data_cpep$visit =  case_when(
  data_cpep$visit == "Day -1" ~ "0",
  data_cpep$visit == "Day 14" ~ "2",
  data_cpep$visit == "Day 28" ~ "4",
  data_cpep$visit == "Week 8" ~ "8",
  data_cpep$visit == "Week 12" ~ "12",
  data_cpep$visit == "Week 16" ~ "16",
  data_cpep$visit == "Week 20" ~ "20",
  data_cpep$visit == "Week 26" ~ "26",
  TRUE ~ data_cpep$visit
)
# Ensure that time and visit are factors with levels set to chronological order
data_cpep$time <- factor(data_cpep$time, levels = c("0hr", "0.5hr", "1hr", "1.5hr", "2hr"))
data_cpep$visit <- factor(data_cpep$visit, levels = c("0", "2", "4", "8", "12", "16", "20", "26"))

## ----include=FALSE-------------------------------------------------------------------------------------------
data_gluc = data.frame(
  subject = all_dat_gluc$`Subject Identifier (SUBNUM)`,
  visit = all_dat_gluc$Visit,
  time = all_dat_gluc$`Additional Test Description (TSTDESC)`,
  treatment = all_dat_gluc$`pre-post OGTT`,
  followup = all_dat_gluc$`Lab time rel OGTT`,
  result = all_dat_gluc$`Reported Numeric Result (RPTRESN)`)

## ----sorting Glucose Data for Imputation---------------------------------------------------------------------
data_gluc$time = case_when(
  data_gluc$time == "Plasma Glucose 2hr" ~ "OGTT Plasma Glucose 0 Min",
  data_gluc$time == "Plasma Glucose 2.5hr" ~ "OGTT Plasma Glucose 30 Min",
  data_gluc$time == "Plasma Glucose 3hr" ~ "OGTT Plasma Glucose 1hr",
  data_gluc$time == "Plasma Glucose 3.5hr" ~ "OGTT Plasma Glucose 1.5hr",
  data_gluc$time == "Plasma Glucose 4hr" ~ "OGTT Plasma Glucose 2hr",
  TRUE ~ data_gluc$time
)
data_gluc = data_gluc |>
  filter(time %in% c("OGTT Plasma Glucose 0 Min", "OGTT Plasma Glucose 30 Min", "OGTT Plasma Glucose 1hr", "OGTT Plasma Glucose 1.5hr", "OGTT Plasma Glucose 2hr"))|>
  filter(!visit %in% c("Day 7", "Day 21", "Unscheduled"))

data_gluc$time = case_when(
  data_gluc$time == "OGTT Plasma Glucose 0 Min"~ "0hr",
  data_gluc$time == "OGTT Plasma Glucose 30 Min" ~ "0.5hr",
  data_gluc$time == "OGTT Plasma Glucose 1hr" ~ "1hr",
  data_gluc$time == "OGTT Plasma Glucose 1.5hr" ~ "1.5hr",
  data_gluc$time == "OGTT Plasma Glucose 2hr" ~ "2hr",
  TRUE ~ data_gluc$time
)
data_gluc$visit =  case_when(
  data_gluc$visit == "Day -1" ~ "0",
  data_gluc$visit == "Day 14" ~ "2",
  data_gluc$visit == "Day 28" ~ "4",
  data_gluc$visit == "Week 8" ~ "8",
  data_gluc$visit == "Week 12" ~ "12",
  data_gluc$visit == "Week 16" ~ "16",
  data_gluc$visit == "Week 20" ~ "20",
  data_gluc$visit == "Week 26" ~ "26",
  TRUE ~ data_gluc$visit
)
# Ensure that time and visit are factors with levels set to chronological order
data_gluc$time <- factor(data_gluc$time, levels = c("0hr", "0.5hr", "1hr", "1.5hr", "2hr"))
data_gluc$visit <- factor(data_gluc$visit, levels = c("0", "2", "4", "8", "12", "16", "20", "26"))
# remove everything from the environment except  two data frames
rm(list=setdiff(ls(), c("data_cpep", "data_gluc")))


# Helper functions --------------------------------------------------------

# Function to calculate AUC using the trapezoidal rule
trapezoidal_auc <- function(time, value) {
  n <- length(time)
  auc <- sum(diff(time) * (value[-1] + value[-n]) / 2)
  return(abs(auc))
}

# Function to convert "hr" strings to minutes
convert_to_minutes <- function(time_str) {
  hours <- as.numeric(sub("hr", "", time_str))
  return(hours * 60)
}

# Function to calculate the Index
calculate_index <- function(data_cpep, data_gluc) {
  data_cpep$time <- sapply(data_cpep$time, convert_to_minutes)
  data_gluc$time <- sapply(data_gluc$time, convert_to_minutes)

  auc_cpep <- data_cpep %>%
    group_by(subject, visit) %>%
    summarise(auc_cpep = trapezoidal_auc(as.numeric(time), result)) %>%
    ungroup()

  auc_gluc <- data_gluc %>%
    group_by(subject, visit) %>%
    summarise(auc_gluc = trapezoidal_auc(as.numeric(time), result)) %>%
    ungroup()

  index_data <- auc_cpep %>%
    inner_join(auc_gluc, by = c("subject", "visit")) %>%
    mutate(index = (auc_cpep / auc_gluc) * 100)

  return(index_data)
}
