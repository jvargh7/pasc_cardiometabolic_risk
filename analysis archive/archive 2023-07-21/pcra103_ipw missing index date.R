rm(list=ls());gc();source(".Rprofile")

source("analysis/pcra_analytic dataset for data availability.R")
source("analysis/pcra_ipw formula for main analysis.R")

library(mice)
source("C:/code/external/functions/causality/censoring_weights.R")
mid_weights = list()

if(sum(outcome_availability$in_index_date) < nrow(outcome_availability)){
  
  for(i in 1:mi_dfs$m){
    
    m_df = complete(mi_dfs,i) %>% 
      mutate_at(vars(payer_type_primary,payer_type_secondary),
                function(x) case_when(is.na(x) ~ "No Insurance",
                                      TRUE ~ x)) %>% 
      mutate(site = case_when(is.na(site) ~ "Source1",
                              TRUE ~ site)) %>% 
      left_join(outcome_availability %>% 
                  dplyr::select(ID,in_index_date),
                by = "ID")
    
    
    mid_weights[[i]] = censoring_weights(c_formula = ipw_missing_index,df = m_df,censoring_var_yes = FALSE,standardized = TRUE,type = "glm")
    
    
    
    
  }
  
  mid_weights_df = bind_cols(mid_weights) %>% 
    rename_all(~paste0("MID",1:mi_dfs$m))
  
  mid_weights_df$ID = m_df$ID
  
  saveRDS(mid_weights_df,paste0(path_pasc_cmr_folder,"/working/cleaned/ip weights for missing index date.RDS"))
  
  # There is a tight distribution of MID weights ----
  mid_weights_df %>% 
    left_join(index_date %>% 
                dplyr::select(ID,index_date,COHORT),
              by = "ID") %>% 
    dplyr::filter(!is.na(COHORT)) %>% 
    ggplot(data=.,aes(x=MID4,group=COHORT,fill=COHORT)) +
    geom_density(alpha=0.2)
  
} else{
  file.remove(paste0(path_pasc_cmr_folder,"/working/cleaned/ip weights for missing index date.RDS"))
  
}
