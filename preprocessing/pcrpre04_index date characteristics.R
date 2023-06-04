rm(list=ls());gc();source(".Rprofile")
source("functions/month_replace.R")

index_date <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/index date.RDS"))

# Facility of diagnosis -----------

index_encounters = index_date$ENCOUNTERID

facility <- open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/encounter_",version,".parquet")) %>% 
  dplyr::select(ID,ENCOUNTERID,FACILITY_LOCATION,ENC_TYPE,FACILITYID,FACILITY_TYPE,PAYER_TYPE_PRIMARY,PAYER_TYPE_SECONDARY,site) %>% 
  right_join(index_date %>% 
               dplyr::select(ID,ENCOUNTERID,index_date,COHORT),
             by = c("ID","ENCOUNTERID")) %>% 
  # Insurance Type (No Insurance, Medicare, Medicaid, Private) -----------
  mutate(across(one_of("PAYER_TYPE_PRIMARY","PAYER_TYPE_SECONDARY"), ~case_when(. %in% c("NI","UN","") ~ "No Information",
                                                                                           . %in% c("1","11","111","119") ~ "Medicare",
                                                                                           . %in% c("2","21","29") ~ "Medicaid",
                                                                                           . %in% c("3","311","32","349","382") ~ "Government",
                                                                                           . %in% c("6","623") ~ "Bluecross",
                                                                                           . %in% c("8","81","82","821","822") ~ "No Insurance",
                                                                                           . %in% c("5","51","511","512","52","521",
                                                                                                    "529") ~ "Private or Other",
                                                                                           TRUE ~ "Private or Other"))) %>% 
  rename(payer_type_primary = PAYER_TYPE_PRIMARY,
         payer_type_secondary = PAYER_TYPE_SECONDARY) %>% 
  collect()

table(is.na(facility$site))
# FALSE   TRUE 
# 383740   4578 

table(facility$payer_type_primary)
# Bluecross       Government         Medicaid         Medicare   No Information     No Insurance Private or Other 
# 36091             9382            26902            66985           103574             8729           136655 

table(facility$payer_type_secondary)
# Bluecross       Government         Medicaid         Medicare   No Information     No Insurance Private or Other 
# 4332             4001            14523             3125           277699            21181            63457


# Hospitalization (Not hospitalized, Hospitalized, ICU admission) within 30 days of infection ------------
# Hospitalized --> ENC_TYPE: EI (ED to IP), IP (IP), IS (Non-acute institutional stay), OS (Observation Stay)
# Not Hospitalized --> ENC_TYPE: ED (Emergency Department), IC (Consult), TH (Telehealth), OA (Other Ambulatory), NI, UN, OT


# The below code doesn't return any observations
        # icu_labs = open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/lab_",version,".parquet")) %>% 
        #   dplyr::select(ID,SPECIMEN_SOURCE,SPECIMEN_DATE) %>% 
        #   mutate(ID = as.character(ID)) %>% 
        #   right_join(index_date %>% 
        #                dplyr::select(ID,index_date) ,
        #              by = c("ID")) %>% 
        #   dplyr::filter(SPECIMEN_SOURCE == "INTENSIVE_CARE_UNIT") %>% 
        #   collect() %>% 
        #   mutate(across(one_of("SPECIMEN_DATE"), ~ymd(month_replace(.)))) %>% 
        #   rename(specimen_date = SPECIMEN_DATE)  %>% 
        #   dplyr::filter(index_date >= specimen_date, index_date < (specimen_date + days(30))) 
  

hospitalization <- open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/encounter_",version,".parquet")) %>% 
  dplyr::select(ID,ENCOUNTERID,FACILITY_LOCATION,ENC_TYPE,ADMIT_DATE,DISCHARGE_DATE) %>% 
  right_join(index_date %>% 
               dplyr::select(ID,index_date,origin_date),
             by = c("ID")) %>% 
  mutate(hospitalized = case_when(ENC_TYPE %in% c("EI","IP","IS","OS") ~ 1,
                                     TRUE ~ 0),
         not_hospitalized = case_when(ENC_TYPE %in% c("ED","IC","TH","OA","NI","UN","OT") ~ 1,
                                      TRUE ~ 0)) %>% 
  dplyr::filter(ADMIT_DATE >= index_date, ADMIT_DATE < origin_date)  %>% 
  collect() %>% 
  # USED group_by --> summarize ---------
  group_by(ID) %>% 
  # Counts of encounters when hospitalized
  summarize(n_hospitalized = sum(hospitalized),
  # Counts of encounters when not hospitalized
            n_not_hospitalized = sum(not_hospitalized)) %>% 
  mutate(hospitalization = case_when(n_hospitalized >= 1 ~ 1,
                                     n_hospitalized == 0 & n_not_hospitalized >=1 ~ 0,
                                     TRUE ~ NA_real_)) 

table(hospitalization$hospitalization,useNA = "always")
# 
#     1      2   <NA> 
#   91031 162085 122027


# Severity of hyperglycemia between index date and origin date (% measures: <25, 25-50, >50) -------------
glucose <- open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/lab_",version,".parquet")) %>% 
  mutate(ID = as.character(ID)) %>% 
  dplyr::filter(str_detect(RAW_LAB_NAME,"(GLUCOSE|Glucose)"),!str_detect(RAW_LAB_NAME,"(URINE|Urine|UA\\s|Ur\\s)")) %>% 
  dplyr::select(ID,ENCOUNTERID,RAW_LAB_NAME,LAB_LOINC,
                LAB_ORDER_DATE,SPECIMEN_DATE, 
                RESULT_NUM,RESULT_QUAL,RESULT_UNIT,
                NORM_MODIFIER_LOW,NORM_RANGE_LOW,NORM_MODIFIER_HIGH,NORM_RANGE_HIGH,
                RAW_RESULT) %>% 
  right_join(index_date %>% 
               dplyr::select(ID,index_date,origin_date),
             by = c("ID")) %>% 
  dplyr::filter(LAB_LOINC %in% glucose_loinc) %>% 
  # NI: No information
  mutate(hyperglycemia = case_when(RESULT_QUAL %in% c("HIGH") ~ 1,
                                   RESULT_QUAL %in% c("LOW","NEGATIVE","NI","OT","UNDETECTABLE") ~ 0,
                                   TRUE ~ NA_real_)) %>% 
  dplyr::filter(SPECIMEN_DATE >= index_date, SPECIMEN_DATE < origin_date)   %>% 
  # USED group_by --> summarize ---------
  group_by(ID) %>% 
  summarize(n_hyperglycemia = sum(hyperglycemia,na.rm=TRUE),
            n_total = sum(!is.na(hyperglycemia)),
            p_hyperglycemia = mean(hyperglycemia,na.rm=TRUE)) %>% 
  collect()

# table(glucose$RESULT_QUAL,useNA = "always")
# HIGH          LOW     NEGATIVE           NI           OT           UN UNDETECTABLE         <NA> 
#   26716          494           20       328131        31416         5812            7            0


(index_date_characteristics <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/demographic.RDS")) %>% 
  dplyr::select(ID, COHORT) %>% 
  left_join(facility %>% 
              dplyr::select(ID,site,payer_type_primary,payer_type_secondary),
            by = "ID") %>% 
  left_join(hospitalization %>% 
              dplyr::select(ID,hospitalization,n_hospitalized,n_not_hospitalized),
            by = "ID") %>% 
  left_join(glucose %>% 
              dplyr::select(ID,n_hyperglycemia,n_total,p_hyperglycemia),
            by = "ID")) %>% 
  saveRDS(.,paste0(path_pasc_cmr_folder,"/working/cleaned/index date characteristics.RDS"))




