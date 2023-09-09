rm(list=ls());gc();source(".Rprofile")

# index_date is merged into demographic in pcrpre101_demographic.R from : "/working/source/Demographic_chakkalakal_v2.csv"
readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre101_demographic.RDS")) %>% 
  dplyr::select(ID,COHORT,index_date) %>% 
  mutate(origin_date = index_date + 30,
         index_date_minus365 = index_date - 365,
         index_date_minus730 = index_date - 730,
         max_followup_date = case_when(COHORT == "historical" ~ historical_followup_stop,
                                       TRUE ~ exposed_followup_stop)) %>% 
  saveRDS(.,paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre201_index date.RDS"))
