rm(list=ls());gc();source(".Rprofile")

demographic <- read_parquet(paste0(path_pasc_cmr_folder,"/working/raw/demographic_",version,".parquet")) %>% 
  dplyr::mutate(nhwhite = case_when(HISPANIC %in% c("N","NI","OT","R","UN") & RACE == "05" ~ 1,
                             TRUE ~ 0),
         nhblack = case_when(HISPANIC %in% c("N","NI","OT","R","UN") & RACE == "03" ~ 1,
                             TRUE ~ 0),
         hispanic = case_when(HISPANIC %in% c("Y") ~ 1,
                              TRUE ~ 0),
         nhother = case_when(HISPANIC %in% c("N","NI","OT","R","UN") & !RACE %in% c("05","03") ~ 1,
                             TRUE ~ 0),
         female = case_when(SEX == "F" ~ 1,
                            TRUE ~ 0),
         age = case_when(AGE > 99 ~ 99,
                         TRUE ~ as.numeric(AGE))) %>% 
  dplyr::select(ID,female,
                nhwhite,nhblack,hispanic,
                nhother,age,COHORT,matchid) %>% 
  collect()


saveRDS(demographic,paste0(path_pasc_cmr_folder,"/working/cleaned/demographic.RDS"))
