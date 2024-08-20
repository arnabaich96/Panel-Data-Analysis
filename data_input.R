library(readr)
library(dplyr)
data_cpep <- read_csv("data/data_cpep_mice.csv")

# rename variables for requirement of the app
data_cpep <- data_cpep %>% rename(
"ID" = "subject",
"Visit" = "visit",
"Time" = "time",
"Measurement" = "result"
)

data_cpep <- data_cpep %>% mutate(ID = as.character(ID)
                                  , Visit = as.factor(Visit)
                                  , Time = as.factor(Time)
                                  )


# if treatment variable is NA replace it with the treatment of same ID
 data_cpep <- data_cpep %>% group_by(ID) %>% mutate(treatment = ifelse(is.na(treatment), treatment[!is.na(treatment)], treatment))
 data_cpep <- data_cpep %>% mutate(treatment = ifelse(treatment == "Pre OGTT start", "0", "1"))
unique(data_cpep$Visit)
# reaname visit from 1 to 8
mapping <- c("0" = "1", "2" = "2", "4" = "3", "8" = "4", "12" = "5", "16" = "6", "20" = "7", "26" = "8")
data_cpep$Visit <- mapping[data_cpep$Visit]
         
table(data_cpep$treatment,data_cpep$ID)
# remove first two columns
data_cpep <- data_cpep[,-c(1,2)]
# write the data to a csv file
# 
write.csv(data_cpep, "data/data_cpep.csv", row.names = FALSE)
unique(data_cpep$treatment)
