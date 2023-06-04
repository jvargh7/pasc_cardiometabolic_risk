rm(list=ls());gc();source(".Rprofile")

source("analysis/pcra_analytic dataset for data availability.R")
source("analysis/pcra_ipw formula for main analysis.R")

library(mice)
source("C:/code/external/functions/causality/censoring_weights.R")
bmi_weights = list()
sbp_weights = list()
ldl_weights = list()

for(i in 1:mi_dfs$m){
  
  mo_df = complete(mi_dfs,i) %>% 
    mutate_at(vars(payer_type_primary,payer_type_secondary),
              function(x) case_when(is.na(x) ~ "No Insurance",
                                    TRUE ~ x)) %>% 
    mutate(site = case_when(is.na(site) ~ "Source1",
                            TRUE ~ site)) %>% 
    left_join(outcome_availability %>% 
                dplyr::select(ID,in_index_date,in_bmi_ID,in_sbp_ID,in_ldl_ID),
              by = "ID") %>% 
    dplyr::filter(in_index_date == 1)
  
  
  bmi_weights[[i]] = censoring_weights(c_formula = ipw_missing_bmi,df = mo_df,censoring_var_yes = FALSE,standardized = TRUE,type = "glm")
  sbp_weights[[i]] = censoring_weights(c_formula = ipw_missing_sbp,df = mo_df,censoring_var_yes = FALSE,standardized = TRUE,type = "glm")
  ldl_weights[[i]] = censoring_weights(c_formula = ipw_missing_ldl,df = mo_df,censoring_var_yes = FALSE,standardized = TRUE,type = "glm")
  
}

bmi_weights_df = bind_cols(bmi_weights) %>% 
  rename_all(~paste0("MO",1:mi_dfs$m)) %>% 
  mutate_all(~case_when(. > 10 ~ 10,
                        . < 0.1 ~ 0.1,
                        TRUE ~ .))

sbp_weights_df = bind_cols(sbp_weights) %>% 
  rename_all(~paste0("MO",1:mi_dfs$m)) %>% 
  mutate_all(~case_when(. > 10 ~ 10,
                        . < 0.1 ~ 0.1,
                        TRUE ~ .))

ldl_weights_df = bind_cols(ldl_weights) %>% 
  rename_all(~paste0("MO",1:mi_dfs$m)) %>% 
  mutate_all(~case_when(. > 10 ~ 10,
                        . < 0.1 ~ 0.1,
                        TRUE ~ .))


bmi_weights_df$ID = mo_df$ID
sbp_weights_df$ID = mo_df$ID
ldl_weights_df$ID = mo_df$ID

saveRDS(bmi_weights_df,paste0(path_pasc_cmr_folder,"/working/cleaned/ip weights for missing bmi.RDS"))
saveRDS(sbp_weights_df,paste0(path_pasc_cmr_folder,"/working/cleaned/ip weights for missing sbp.RDS"))
saveRDS(ldl_weights_df,paste0(path_pasc_cmr_folder,"/working/cleaned/ip weights for missing ldl.RDS"))
