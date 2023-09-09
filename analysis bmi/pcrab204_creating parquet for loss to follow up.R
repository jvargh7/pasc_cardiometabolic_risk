rm(list=ls());gc();source(".Rprofile")

source("analysis bmi/pcrab003_analytic dataset for data availability.R")
rm(anthro_followup,demographic,index_date,lab_followup); gc()

source("analysis bmi/pcrab102_selecting high dimensional variables based on fdr.R")
source("analysis bmi/pcrab202_selecting ltfu high dimensional variables based on fdr.R")

lookback_processed <- readRDS(paste0(path_pasc_cmr_folder,"/working/models pcrab/pcrab002_imputed lookback dataset.RDS")) %>% 
  # analytic_sample is coming from pcrab003_analytic dataset for data availability.R
  left_join(analytic_sample %>% 
              dplyr::select(ID, in_bmi_followup_ID),
            by = "ID")

# Loads HD dataset + restricts based on a cutoff
source("analysis bmi/pcrab103_restricting high dimensional covariates based on frequency.R")
source("analysis bmi/pcrab203_restricting ltfu high dimensional covariates based on frequency.R")



outcome_df <- lookback_processed %>% 
  # dplyr::filter(ID %in% bmi_ID) %>% 
  left_join(hd_dataset_COHORT %>% 
              dplyr::select(ID, one_of(restricted_hdvars)),
            by = "ID") %>% 
  left_join(hd_dataset_censoring %>% 
              dplyr::select(ID, one_of(restricted_hdvars_censoring)),
            by = "ID")
write_parquet(outcome_df,paste0(path_pasc_cmr_folder,"/working/models pcrab/pcrab204_ipw for loss to followup data.parquet"))