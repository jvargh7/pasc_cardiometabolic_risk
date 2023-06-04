rm(list=ls());gc();source(".Rprofile")

source("functions/month_replace.R")

# read_parquet(paste0(path_pasc_cmr_folder,"/working/raw/encounter_",version,".parquet")) %>% 
#   dplyr::select(ID,ENCOUNTERID,ENC_TYPE,admit_date,discharge_date,LOOKBACK1,LOOKBACK2,FOLLOWUP1) %>% 
#   write_parquet(.,paste0(path_pasc_cmr_folder,"/working/cleaned/encounter.parquet"))

# encounter %>%  
#   dplyr::select(ID,admit_date,discharge_date,ENCOUNTERID,LOOKBACK1,FOLLOWUP1) %>% 
#   arrange(ID,admit_date) %>% 
#   group_by(ID) %>% 
#   mutate(index_encounter = case_when(dplyr::lag(LOOKBACK1) == 1 & dplyr::lead(FOLLOWUP1) == 1 ~ 1,
#                                      TRUE ~ 0)) %>% 
#   dplyr::filter(index_encounter == 1) %>% 
#   saveRDS(.,paste0(path_pasc_cmr_folder,"/working/cleaned/encounter_index.RDS"))  
