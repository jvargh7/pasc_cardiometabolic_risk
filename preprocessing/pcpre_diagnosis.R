rm(list=ls());gc();source(".Rprofile")

source("functions/month_replace.R")


diagnosis <- readRDS(paste0(path_pasc_cmr_folder,"/working/raw/diagnosis_",version,".RDS")) %>% 
  mutate(dx_date = ymd(month_replace(DX_DATE)),
         admit_date = ymd(month_replace(ADMIT_DATE))) 

diagnosis %>% 
  dplyr::filter(DX %in% c("U07.1","U07.2",
                          "J12.81","J12.82",
                          "B34.2","B97.2",
                          "B97.21","B97.29",
                          "U04","U04.9")) %>% 
  saveRDS(.,paste0(path_pasc_cmr_folder,"/working/cleaned/diagnosis_covid.RDS"))