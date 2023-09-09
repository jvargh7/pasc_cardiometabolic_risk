rm(list=ls());gc();source(".Rprofile")

source("functions/month_replace.R")

open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/lab_",version,".parquet")) %>% 
  dplyr::filter(LAB_LOINC %in% c("94500-6","94309-2",
                                 "94558-4","94534-5",
                                 "94759-8","95209-3",
                                 "95608-6","95380-2",
                                 "94316-7","95422-2",
                                 "94533-7","94565-9",
                                 "94756-4","94642-6",
                                 "94306-8","95406-5",
                                 "95425-5","87635")) %>% 
  collect() %>% 
  saveRDS(.,paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre104_lab results covid tests.RDS"))





 
