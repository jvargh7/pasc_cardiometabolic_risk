rm(list=ls());gc();source(".Rprofile")
# index_date <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/index date.RDS"))

# LOSS TO FOLLOW-UP -------------
source("analysis bmi/pcrab003_analytic dataset for data availability.R")
source("C:/code/external/functions/causality/trim_probabilities.R")

lookback_processed <- readRDS(paste0(path_pasc_cmr_folder,"/working/models pcrab/pcrab002_imputed lookback dataset.RDS")) %>% 
  # analytic_sample is coming from pcrab003_analytic dataset for data availability.R
  left_join(analytic_sample %>% 
              dplyr::select(ID, in_bmi_followup_ID),
            by = "ID")

predicted_probability <-read_csv(paste0(path_pasc_cmr_folder,"/working/sensitivity utilization/pcrsu206_predicted probability for loss to followup_min10_ntree2000.csv")) %>% 
  dplyr::select(available,missing) %>%
  bind_cols(lookback_processed %>% 
              dplyr::select(ID,COHORT)) %>% 
  mutate(across(contains("available"),~trim_probabilities(.),.names = "{.col}_trimmed")) %>%  
  
  mutate(ltfu_weights = 1/available_trimmed)


saveRDS(predicted_probability, paste0(path_pasc_cmr_folder,"/working/sensitivity utilization/pcrsu301_ip weights for missing outcomes.RDS"))

