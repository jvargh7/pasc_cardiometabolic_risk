rm(list=ls());gc();source(".Rprofile")

source("analysis/pcra_analytic dataset for data availability.R")
source("analysis/pcra_ipw formula for main analysis.R")

library(mice)
source("C:/code/external/functions/causality/censoring_weights.R")

lookback_processed <- readRDS(paste0(path_pasc_cmr_folder,"/working/models/pcra102_imputed lookback dataset.RDS"))

mo_df <- lookback_processed %>% 
    mutate_at(vars(payer_type_primary,payer_type_secondary),
              function(x) case_when(is.na(x) ~ "No Insurance",
                                    TRUE ~ x)) %>% 
    mutate(site = case_when(is.na(site) ~ "Source1",
                            TRUE ~ site)) %>% 
    left_join(outcome_availability %>% 
                dplyr::select(ID,in_index_date,in_bmi_ID,in_sbp_ID,in_ldl_ID),
              by = "ID")


bmi_weights = censoring_weights(c_formula = ipw_missing_bmi,df = mo_df,censoring_var_yes = FALSE,standardized = TRUE,type = "glm")
sbp_weights = censoring_weights(c_formula = ipw_missing_sbp,df = mo_df,censoring_var_yes = FALSE,standardized = TRUE,type = "glm")
ldl_weights = censoring_weights(c_formula = ipw_missing_ldl,df = mo_df,censoring_var_yes = FALSE,standardized = TRUE,type = "glm")


index_date$bmi_weights = bmi_weights
index_date$sbp_weights = sbp_weights
index_date$ldl_weights = ldl_weights

index_date %>% 
  dplyr::select(ID,COHORT,bmi_weights,sbp_weights,ldl_weights) %>% 

saveRDS(.,paste0(path_pasc_cmr_folder,"/working/cleaned/ip weights for missing outcomes.RDS"))
