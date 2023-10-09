rm(list=ls());gc();source(".Rprofile")

source("functions/month_replace.R")

open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/lab_",version,".parquet")) %>% 
  dplyr::filter(LAB_LOINC %in% covid_lab_loincs) %>% 
  collect() %>% 
  saveRDS(.,paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre104_lab results covid tests.RDS"))





 
