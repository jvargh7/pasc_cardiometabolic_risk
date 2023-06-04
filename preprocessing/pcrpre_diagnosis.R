rm(list=ls());gc();source(".Rprofile")



open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/diagnosis_",version,".parquet")) %>% 
  dplyr::filter(DX %in% c("U07.1","U07.2",
                          "J12.81","J12.82",
                          "B34.2","B97.2",
                          "B97.21","B97.29",
                          "U04","U04.9")) %>% 
  collect() %>% 
  saveRDS(.,paste0(path_pasc_cmr_folder,"/working/cleaned/diagnosis_covid.RDS"))
