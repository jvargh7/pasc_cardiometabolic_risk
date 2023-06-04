rm(list=ls());gc();source(".Rprofile")

readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/demographic.RDS")) %>% 
  group_by(COHORT) %>% 
  summarize_at(vars(female,age),~mean(.)) %>% 
  write_csv("paper/table_unweighted summary by cohort.csv")
