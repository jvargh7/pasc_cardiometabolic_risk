library(tidyverse)
library(lubridate)
path_pasc_proposal_folder <- "C:/Cloud/OneDrive - Emory University/Proposals/R01S COVID x Post Acute Diabetes Diagnosis"
path_pasc_cmr_folder <- "C:/Cloud/OneDrive - Emory University/Papers/PASC Cardiometabolic Risk Factors"
path_pasc_cmr_repo <- "C:/code/external/pasc_cardiometabolic_risk"

version = "chakkalakal_v1"

pandemic_start = "2020-03-01" # March 1, 2020
exposed_identification_start = ymd(pandemic_start)
exposed_identification_stop = ymd("2022-01-29")
exposed_followup_start = exposed_identification_start + days(30)
exposed_followup_stop = exposed_identification_stop + days(30)

historical_identification_start = ymd("2018-03-02")
historical_identification_stop = ymd("2020-01-30")
historical_followup_start = historical_identification_start + days(30)
historical_followup_stop = historical_identification_stop + days(30)

admissible_encounter_types = c('AV', 'IP', 'OS','ED','EI','IC','IS','TH')
