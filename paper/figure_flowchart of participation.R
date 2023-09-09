rm(list=ls());gc();source(".Rprofile")
source(paste0(path_pasc_cmr_repo,"/analysis bmi/pcrab001_processing before imputation of lookback covariates.R"))
step1_bmi_df <- lookback_df %>% 
  dplyr::filter(!is.na(bmi))

step1_bmi_df %>% 
  group_by(COHORT) %>% 
  tally()


step2_bmi_df = readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/anthro followup.RDS")) %>% 
  dplyr::filter(ID %in% step1_bmi_df$ID,!is.na(bmi)) %>% 
  mutate(t = as.numeric(t)) %>% 
  arrange(ID,t) %>% 
  dplyr::select(ID,COHORT,bmi,t) 

# Unique patients
step2_bmi_df %>% 
  distinct(COHORT,ID) %>% 
  group_by(COHORT) %>% 
  tally()

# Observations in follow-up
step2_bmi_df %>% 
  group_by(COHORT) %>% 
  tally()
