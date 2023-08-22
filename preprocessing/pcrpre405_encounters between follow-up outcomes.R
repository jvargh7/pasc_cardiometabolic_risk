rm(list=ls());gc();source(".Rprofile")
source("analysis/pcra_analytic dataset for data availability.R")
source("analysis/pcra_analytic dataset for change in cardiometabolic indicators.R")


bmi_df = bmi_df %>%
  group_by(ID) %>% 
  mutate(previous_followup_date = case_when(is.na(dplyr::lag(MEASURE_DATE,1)) | is.null(dplyr::lag(MEASURE_DATE,1)) ~ origin_date,
                                            TRUE ~ dplyr::lag(MEASURE_DATE,1))) %>% 
  ungroup()
  
sbp_df = sbp_df %>%
  group_by(ID) %>% 
  mutate(previous_followup_date = case_when(is.na(dplyr::lag(MEASURE_DATE,1)) | is.null(dplyr::lag(MEASURE_DATE,1)) ~ origin_date,
                                            TRUE ~ dplyr::lag(MEASURE_DATE,1))) 


ldl_df = ldl_df %>%
  group_by(ID) %>% 
  mutate(previous_followup_date = case_when(is.na(dplyr::lag(SPECIMEN_DATE,1)) | is.null(dplyr::lag(SPECIMEN_DATE,1)) ~ origin_date,
                                            TRUE ~ dplyr::lag(SPECIMEN_DATE,1))) 


encounter_between_followup_bmi <- open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/encounter_",version,".parquet")) %>% 
  dplyr::select(ID,ENCOUNTERID,FACILITY_LOCATION,ENC_TYPE,
                ADMIT_DATE,DISCHARGE_DATE)  %>% 
  right_join(index_date %>% 
               dplyr::select(ID,index_date,origin_date,max_followup_date,COHORT),
             by = c("ID")) %>% 
  dplyr::filter(ADMIT_DATE >= origin_date, ADMIT_DATE <= max_followup_date)   %>% 
  left_join(bmi_df %>% 
              dplyr::select(ID, MEASURE_DATE,previous_followup_date),
            by = "ID") %>% 
  dplyr::filter(ADMIT_DATE >= previous_followup_date,ADMIT_DATE < MEASURE_DATE) %>% 
  group_by(ID,MEASURE_DATE,ENC_TYPE) %>% 
  tally()  %>% 
  collect() %>% 
  pivot_wider(names_from=ENC_TYPE,values_from=n,values_fill = 0)
  

encounter_between_followup_sbp <- open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/encounter_",version,".parquet")) %>% 
  dplyr::select(ID,ENCOUNTERID,FACILITY_LOCATION,ENC_TYPE,
                ADMIT_DATE,DISCHARGE_DATE)  %>% 
  right_join(index_date %>% 
               dplyr::select(ID,index_date,origin_date,max_followup_date,COHORT),
             by = c("ID")) %>% 
  dplyr::filter(ADMIT_DATE >= origin_date, ADMIT_DATE <= max_followup_date)   %>% 
  left_join(sbp_df %>% 
              dplyr::select(ID, MEASURE_DATE,previous_followup_date),
            by = "ID") %>% 
  dplyr::filter(ADMIT_DATE >= previous_followup_date,ADMIT_DATE < MEASURE_DATE) %>% 
  group_by(ID,MEASURE_DATE,ENC_TYPE) %>% 
  tally()  %>% 
  collect() %>% 
  pivot_wider(names_from=ENC_TYPE,values_from=n,values_fill = 0)


encounter_between_followup_ldl <- open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/encounter_",version,".parquet")) %>% 
  dplyr::select(ID,ENCOUNTERID,FACILITY_LOCATION,ENC_TYPE,
                ADMIT_DATE,DISCHARGE_DATE)  %>% 
  right_join(index_date %>% 
               dplyr::select(ID,index_date,origin_date,max_followup_date,COHORT),
             by = c("ID")) %>% 
  dplyr::filter(ADMIT_DATE >= origin_date, ADMIT_DATE <= max_followup_date)   %>% 
  left_join(ldl_df %>% 
              dplyr::select(ID, SPECIMEN_DATE,previous_followup_date),
            by = "ID") %>% 
  dplyr::filter(ADMIT_DATE >= previous_followup_date,ADMIT_DATE < SPECIMEN_DATE) %>% 
  group_by(ID,SPECIMEN_DATE,ENC_TYPE) %>% 
  tally()  %>% 
  collect() %>% 
  pivot_wider(names_from=ENC_TYPE,values_from=n,values_fill = 0)

saveRDS(encounter_between_followup_bmi,paste0(path_pasc_cmr_folder,"/working/cleaned/encounter_between_followup_bmi.RDS"))
saveRDS(encounter_between_followup_sbp,paste0(path_pasc_cmr_folder,"/working/cleaned/encounter_between_followup_sbp.RDS"))
saveRDS(encounter_between_followup_ldl,paste0(path_pasc_cmr_folder,"/working/cleaned/encounter_between_followup_ldl.RDS"))
