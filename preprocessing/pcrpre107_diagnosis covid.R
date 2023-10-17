rm(list=ls());gc();source(".Rprofile")

open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/diagnosis_",version,".parquet")) %>% 
  dplyr::filter(DX %in% icd10_covid) %>% 
  collect() %>% 
  dplyr::filter(ID %in% included_patients$ID) %>% 
  saveRDS(.,paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre107_diagnosis covid.RDS"))
