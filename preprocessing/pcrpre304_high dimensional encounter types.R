rm(list=ls());gc();source(".Rprofile")

index_date <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/index date.RDS"))

lb_hd_encounter_types = open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/encounter_",version,".parquet")) %>% 
  mutate(ID = as.character(ID)) %>% 
  dplyr::select(ID,ENCOUNTERID,ENC_TYPE,ADMIT_DATE) %>% 
  right_join(index_date %>% 
               dplyr::select(ID,origin_date,index_date,index_date_minus365,index_date_minus730, COHORT),
             by = c("ID")) %>% 
  mutate(date_type = case_when(ADMIT_DATE >= origin_date ~ "p4",
                               ADMIT_DATE >= index_date ~ "p3",
                               ADMIT_DATE >= index_date_minus365 ~ "p2",
                               ADMIT_DATE >= index_date_minus730 ~ "p1",
                               TRUE ~ NA_character_)) %>% 
  dplyr::filter(!is.na(date_type)) %>% 
  group_by(COHORT,ID,date_type,ENC_TYPE) %>% 
  tally() %>% 
  dplyr::filter(!is.na(ENC_TYPE)) %>% 
  collect() %>% 
  pivot_wider(names_from="ENC_TYPE",values_from="n")

saveRDS(lb_hd_encounter_types,paste0(path_pasc_cmr_folder,"/working/cleaned/high dimensional encounter types.RDS"))
