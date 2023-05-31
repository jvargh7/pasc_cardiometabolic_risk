rm(list=ls());gc();source(".Rprofile")

source("functions/month_replace.R")


vital <- readRDS(paste0(path_pasc_cmr_folder,"/working/raw/vital_",version,".RDS")) %>% 
  mutate(measure_date = ymd(month_replace(MEASURE_DATE))) %>% 
  dplyr::select(-ENCOUNTERID,-VITALID,-MEASURE_DATE,-VITAL_SOURCE) %>% 
  dplyr::filter(!is.na(HT)|!is.na(WT)|!is.na(DIASTOLIC)|!is.na(SYSTOLIC)|!is.na(SMOKING)) %>% 
  # Using -1 so that min() can be applied across 
  mutate(smoking = case_when(SMOKING %in% c("01","02","03","04","05","06","07","08") ~ -1,
                             TRUE ~ 0))


vital %>% 
  group_by(ID,measure_date) %>% 
  summarize_at(vars(HT,WT,DIASTOLIC,SYSTOLIC,smoking),function(x) min(x,na.rm=TRUE)) %>% 
  mutate(smoking = smoking*-1) %>% 
  ungroup() %>% 
  saveRDS(.,paste0(path_pasc_cmr_folder,"/working/cleaned/vital.RDS"))
