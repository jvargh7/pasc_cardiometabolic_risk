rm(list=ls());gc();source(".Rprofile")


source("analysis/pcra_processing before imputation of lookback covariates.R")
source("analysis/pcra_analytic dataset for data availability.R")
source("analysis/pcra_analytic dataset for change in cardiometabolic indicators.R")


in_bmi_lookback = lookback_df %>% 
  dplyr::filter(!is.na(bmi)) %>% 
  dplyr::select(ID) %>% 
  pull()

in_sbp_lookback = lookback_df %>% 
  dplyr::filter(!is.na(SYSTOLIC)) %>% 
  dplyr::select(ID) %>% 
  pull()

in_ldl_lookback = lookback_df %>% 
  dplyr::filter(!is.na(ldl)) %>% 
  dplyr::select(ID) %>% 
  pull()

outcome_availability %>% 
  mutate(in_bmi_lookback_ID = case_when(ID %in% in_bmi_lookback ~ 1,
                                        TRUE ~ 0),
         in_sbp_lookback_ID = case_when(ID %in% in_sbp_lookback ~ 1,
                                        TRUE ~ 0),
         in_ldl_lookback_ID = case_when(ID %in% in_ldl_lookback ~ 1,
                                        TRUE ~ 0)
         ) %>% 
  group_by(in_bmi_lookback_ID,in_sbp_lookback_ID,in_bmi_ID,in_sbp_ID
           # in_ldl_lookback_ID,in_ldl_ID
  ) %>% tally() %>% 
  ungroup() %>% 
  mutate(across(matches("_ID"),function(x) case_when(x==1 ~ "Available",
                                                     TRUE ~ "Unavailable"))) %>% 
  mutate(n = paste0(n," (",
                    round(n*100/sum(n),1),")")) %>% 
  write_csv(.,"paper/table_outcome availability overall.csv")
