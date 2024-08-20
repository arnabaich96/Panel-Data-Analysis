

library(tidyverse)
library(rio)
library(openxlsx)


################################################################################################################################
################################################################################################################################
# Get cohorts

cohort <- import("Subject IDs by MAD cohort 2024-05-23.xlsx")

################################################################################################################################
#######################################################################################
# get first dose datetime, take baseline labs as last value prior to first dose 

#names(ex_edc); unique(ds_edc$`Cohort: (display value) (COHORT_DEC)`)
ex_edc <- import("ex.xlsx") |>
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
ex_edc_db2 <- import("ex DB2.xlsx") |>
  filter(str_detect(`Visit Name (VISNAME)`,"Day 1")) |>
  filter(!str_detect(`Visit Name (VISNAME)`,"Day 14")) |> 
  filter(`Was dose taken? (EXYN)` == "Y")|>
  select(`Subject Identifier (SUBNUM)`, `Date: (EXSTDAT)`, `Time: (EXSTTIM)`)

first_dose <- bind_rows(ex_edc, ex_edc_db2) |>
  right_join(cohort, by=c("Subject Identifier (SUBNUM)"="Subject ID Assigned"))


#######################################################################################
#######################################################################################
# get OGTT start times

OGTT_times_db1 <- import("ag.xlsx") |>
  inner_join(cohort, by=c("Subject Identifier (SUBNUM)"="Subject ID Assigned")) |>
  select(`Subject Identifier (SUBNUM)`,
         `Visit Name (VISNAME)`,
         `OGTT Procedure Date: (AGSTDAT)`,
         `Glucose Consumption Start Time: (AGSTTIM)`) |>
  filter(!is.na(`Glucose Consumption Start Time: (AGSTTIM)`)) |>
  mutate(Visit = ifelse(str_detect(`Visit Name (VISNAME)`, "4 \\(M"), "Day 14", `Visit Name (VISNAME)`)) 

OGTT_times_db2 <- import("ag DB2.xlsx") |>
  inner_join(cohort, by=c("Subject Identifier (SUBNUM)"="Subject ID Assigned")) |>
  select(`Subject Identifier (SUBNUM)`,
         `Visit Name (VISNAME)`,
         `OGTT Procedure Date: (AGSTDAT)`,
         `Glucose Consumption Start Time: (AGSTTIM)`) |>
  filter(!is.na(`Glucose Consumption Start Time: (AGSTTIM)`)) |>
  mutate(Visit = ifelse(str_detect(`Visit Name (VISNAME)`, "4 \\(M"), "Day 14", `Visit Name (VISNAME)`)) 

OGTT_times <- bind_rows(OGTT_times_db1, OGTT_times_db2) |>
  select(-`Visit Name (VISNAME)`)

rm(OGTT_times_db1, OGTT_times_db2)


# unique(OGTT_all$`Strata Group`)
################################################################################################################################
################################################################################################################################
# get PPD CL data

PPD_lb_tnsf_gluc_insulin <- import("glucose insulin cpeptide all MAD cohorts 2024-05-23.xlsx") |>
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

unique(PPD_lb_tnsf_gluc_insulin$`Lab Test Name (LBTEST)`)

################################################################################################################################
# Begin Cpeptide work: filter PPD CL to cpeptide
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


# here get all cpeptide around the OGTT test
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
         `Visit label` = factor(ifelse(str_detect(`Visit Name (VISNAME)`, "MAD2"),
                                       "Day 14",
                                       `Visit Name (VISNAME)`),
                                levels = c("Day -1", "Day 7", "Day 14", "Day 21", "Day 28",  "Week 8", 
                                           "Week 12", "Week 16", "Week 20", "Week 26")
         )
  ) |>
  relocate(c(`OGTT datetime`, `pre-post OGTT`), .after = `Visit Name (VISIT)`) |>
  filter(!is.na(`Reported Numeric Result (RPTRESN)`)) |>
  arrange(`Subject Identifier (SUBNUM)`,
          `Visit label`,
          `Additional Test Description (TSTDESC)`) 


################################################################################################################################
# Begin glucose work: filter PPD CL to glucose
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


# here get all cpeptide around the OGTT test
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
         `Visit label` = factor(ifelse(str_detect(`Visit Name (VISNAME)`, "MAD2"),
                                       "Day 14",
                                       `Visit Name (VISNAME)`),
                                levels = c("Day -1", "Day 7", "Day 14", "Day 21", "Day 28",  "Week 8", 
                                           "Week 12", "Week 16", "Week 20", "Week 26")
         )
  ) |>
  relocate(c(`OGTT datetime`, `pre-post OGTT`), .after = `Visit Name (VISIT)`) |>
  filter(!is.na(`Reported Numeric Result (RPTRESN)`)) |>
  arrange(`Subject Identifier (SUBNUM)`,
          `Visit label`,
          `Additional Test Description (TSTDESC)`) 

