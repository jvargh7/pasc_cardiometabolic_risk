rm(list=ls());gc();source(".Rprofile")


source("C:/code/external/functions/causality/treatment_weights.R")
source("analysis/pcra_ipw formula for main analysis.R")
library(mice)
mi_dfs <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/lookback mi_dfs.RDS"))
m = mi_dfs$m
tx_weights = list()

for (i in 1:m){
  print(i);
  df = complete(mi_dfs,i) %>% 
    mutate_at(vars(payer_type_primary,payer_type_secondary),
              function(x) case_when(is.na(x) ~ "No Insurance",
                                    TRUE ~ x)) %>% 
    mutate(site = case_when(is.na(site) ~ "Source1",
                            TRUE ~ site))
  
  tx_weights[[i]] = treatment_weights(a_formula = ipw_multiple_exposure,df = df,standardized = TRUE,type = "multinom")
  
  
  
}

tx_weights_df = bind_cols(tx_weights) %>% 
  rename_all(~paste0("W",1:m))

tx_weights_df$ID = df$ID

saveRDS(tx_weights_df,paste0(path_pasc_cmr_folder,"/working/cleaned/ip weights for COHORT.RDS"))

# Assessing IPTW --------------
index_date <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/index date.RDS"))

# There is non-overlap of IPTW ----
tx_weights_df %>% 
  left_join(index_date %>% 
              dplyr::select(ID,index_date,COHORT),
            by = "ID") %>% 
  dplyr::filter(!is.na(COHORT)) %>% 
  ggplot(data=.,aes(x=W4,group=COHORT,fill=COHORT)) +
  geom_density(alpha=0.2)