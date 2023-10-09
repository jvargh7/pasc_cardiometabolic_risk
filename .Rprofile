library(tidyverse)
library(lubridate)
library(arrow)
path_pasc_proposal_folder <- "C:/Cloud/OneDrive - Emory University/Proposals/R01S COVID x Post Acute Diabetes Diagnosis"
path_pasc_cmr_folder <- "C:/Cloud/OneDrive - Emory University/Papers/PASC Cardiometabolic Risk Factors"
path_pasc_cmr_repo <- "C:/code/external/pasc_cardiometabolic_risk"

path_community_profile_reports <- "C:/Cloud/Emory University/Patel, Shivani A - CovidHealthEquityDashboard/Data/Processed/Community Profile Reports/2022-05-02"

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

# Could alternatively use readxl::read_excel("data/PASC CMR Variable List.xlsx",sheet="labs") %>% dplyr::filter(search == "Glucose",include == "Yes")
glucose_loinc <- c("2345-7","2339-0","41653-7",
                   "2340-8","27353-2",
                   "1547-9","1558-6")

# Could alternatively use readxl::read_excel("data/PASC CMR Variable List.xlsx",sheet="labs") %>% dplyr::filter(search == "HbA1c",include == "Yes")
hba1c_loinc <- c("4548-4","41995-2","55454-3",
                 "71875-9","549-2","17856-6",
                 "59261-6","62388-4","17855-8",
                 #10839-9 was not included in Weise 2018
                 "10839-9")

# Used for exact matching --> DX %in% list_of_codes
icd10_dm_qualifying <- c("E11.00", "E11.01", "E11.21", "E11.22", "E11.29", "E11.311", "E11.319", "E11.321",
                         "E11.329", "E11.331", "E11.339", "E11.341", "E11.349", "E11.351", "E11.359", "E11.36",
                         "E11.39", "E11.40", "E11.41", "E11.42", "E11.43", "E11.44", "E11.49", "E11.51", "E11.52",
                         "E11.59", "E11.610", "E11.618", "E11.620", "E11.621", "E11.622", "E11.628", "E11.630", 
                         "E11.638", "E11.641", "E11.649", "E11.65", "E11.69", "E11.8", "E11.9")

# Used for paste0() --> str_detect()
# Any code ending in \\. has a wildcard '*'
icd10_otherdm_excluding <- c("R73\\.01", "R73,02", "R73\\.0", "R81\\.", "E88\\.81", "Z13\\.1", "E13\\.", "E08\\.", "E09\\.")
icd10_t1dm <- c("E10\\.")
icd10_gdm <- c("O24\\.")
icd10_covid <- c("U07.1","U07.2",
                 "J12.81","J12.82",
                 "B34.2","B97.2",
                 "B97.21","B97.29",
                 "U04","U04.9")

# pcrpre302_high dimensional procedures
n_hd_pro_min = 1000
fdr_hd_pvalue = 0.05

sbp_max_possible = 350
sbp_min_possible = 50
dbp_max_possible = 300
dbp_min_possible = 30

wt_max_possible = 500
wt_min_possible = 50
ht_max_possible = 7.5*12
ht_min_possible = 4*12

bmi_min_possible = 12
bmi_max_possible = 60

HT_LOINCS <- c("3137-7", "8302-2", "8308-9","3138-5")
WT_LOINCS <- c("3141-9","29463-7","8335-2","3142-7","8341-0","8340-2")
BMI_LOINCS <- c("39156-5","89270-3")
covid_lab_loincs <- c("94500-6","94309-2",
                      "94558-4","94534-5",
                      "94759-8","95209-3",
                      "95608-6","95380-2",
                      "94316-7","95422-2",
                      "94533-7","94565-9",
                      "94756-4","94642-6",
                      "94306-8","95406-5",
                      "95425-5","87635")
permissible_enc_type <- c("AV","IP","EI","TH")
