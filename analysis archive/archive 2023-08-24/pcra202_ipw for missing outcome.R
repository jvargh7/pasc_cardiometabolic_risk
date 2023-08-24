rm(list=ls());gc();source(".Rprofile")
source("C:/code/external/functions/causality/trim_probabilities.R")

index_date <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/index date.RDS"))
predicted_probability <- bind_cols(
  read_csv(paste0(path_pasc_cmr_folder,"/working/models/pcra202_predicted probability for in_bmi_ID_min10_ntree1000.csv")) %>% rename(bmi_present = present,
                                                                                                                                      bmi_absent = absent),
  read_csv(paste0(path_pasc_cmr_folder,"/working/models/pcra202_predicted probability for in_sbp_ID_min10_ntree1000.csv"))  %>% rename(sbp_present = present,
                                                                                                                                       sbp_absent = absent),
  read_csv(paste0(path_pasc_cmr_folder,"/working/models/pcra202_predicted probability for in_ldl_ID_min10_ntree1000.csv")) %>% rename(ldl_present = present,
                                                                                                                                      ldl_absent = absent)) %>% 
  dplyr::select(contains("present"),contains("absent")) %>%
  bind_cols(index_date %>% 
              dplyr::select(ID,COHORT)) %>% 
  mutate(across(contains("present"),~trim_probabilities(.),.names = "{.col}_trimmed")) %>%  
 
  
  mutate(bmi_weights = 1/bmi_present_trimmed,
         sbp_weights = 1/sbp_present_trimmed,
         ldl_weights = 1/ldl_present_trimmed)


saveRDS(predicted_probability, paste0(path_pasc_cmr_folder,"/working/cleaned/ip weights for missing outcomes.RDS"))
