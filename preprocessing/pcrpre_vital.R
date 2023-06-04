rm(list=ls());gc();source(".Rprofile")

source("functions/month_replace.R")


open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/vital_",version,".parquet")) %>% 
  dplyr::select(-ENCOUNTERID,-VITALID,-VITAL_SOURCE) %>% 
  dplyr::filter(!is.na(HT)|!is.na(WT)|!is.na(DIASTOLIC)|!is.na(SYSTOLIC)|!is.na(SMOKING)) %>% 
  # Using -1 so that min() can be applied across 
  mutate(smoking = case_when(SMOKING %in% c("01","02","03","04","05","06","07","08") ~ -1,
                             TRUE ~ 0)) %>% 
  
  # USED group_by --> summarize ---------
  group_by(ID,MEASURE_DATE) %>% 
  dplyr::summarize(across(one_of("HT","WT","DIASTOLIC","SYSTOLIC","smoking"),~min(.,na.rm=TRUE))) %>% 
  collect() %>% 
  mutate(smoking = smoking*-1) %>% 
  ungroup() %>% 
  saveRDS(.,paste0(path_pasc_cmr_folder,"/working/cleaned/vital.RDS"))
