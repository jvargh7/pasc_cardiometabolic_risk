rm(list=ls());gc();source(".Rprofile")

source("functions/month_replace.R")

death <- readRDS(paste0(path_pasc_cmr_folder,"/working/raw/death_",version,".RDS")) %>% 
  mutate(death_date = ymd(month_replace(DEATH_DATE))) %>% 
  dplyr::select(ID,death_date)

saveRDS(death,paste0(path_pasc_cmr_folder,"/working/cleaned/death.RDS"))
