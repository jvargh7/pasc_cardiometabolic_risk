rm(list=ls());gc();source(".Rprofile")
source(paste0(path_pasc_cmr_repo,"/analysis bmi/pcrab001_processing before imputation and lookback bmi exclusion.R"))
lookback_cpit2dm <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre209_cpit2dm diabetes during lookback period.RDS"))

step1_bmi_df = lookback_df %>% 
  dplyr::filter(!ID %in% c(lookback_cpit2dm$ID))

s1_c = step1_bmi_df %>% 
  group_by(COHORT) %>% 
  tally()

step2_bmi_df <- step1_bmi_df %>% 
  dplyr::filter(!is.na(bmi))

s2_c = step2_bmi_df %>% 
  group_by(COHORT) %>% 
  tally()





step3_bmi_df = readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre401_anthro followup.RDS")) %>% 
  dplyr::filter(ID %in% step2_bmi_df$ID,!is.na(bmi)) %>% 
  mutate(t = as.numeric(t)) %>% 
  arrange(ID,t) %>% 
  dplyr::select(ID,COHORT,bmi,t) 

# Unique patients
s3_p = step3_bmi_df %>% 
  distinct(COHORT,ID) %>% 
  group_by(COHORT) %>% 
  tally()

# Observations in follow-up
s3_o = step3_bmi_df %>% 
  group_by(COHORT) %>% 
  tally()

bind_rows(s1_c %>% mutate(step = "Step 1"),
          s2_c %>% mutate(step = "Step 2"),
          s3_o %>% mutate(step = "Step 3Obs"),
          s3_p %>% mutate(step = "Step 3")) %>% 
  write_csv(.,"paper/table_flowchart of participation.csv")
