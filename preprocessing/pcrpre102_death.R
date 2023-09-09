rm(list=ls());gc();source(".Rprofile")

source("functions/month_replace.R")

death <- read_parquet(paste0(path_pasc_cmr_folder,"/working/raw/death_",version,".parquet")) %>% 
  # mutate(death_date = ymd(month_replace(DEATH_DATE))) %>%
  dplyr::select(ID,DEATH_DATE) %>% 
  collect()

saveRDS(death,paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre102_death.RDS"))
