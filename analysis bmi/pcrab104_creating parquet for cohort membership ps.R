rm(list=ls());gc();source(".Rprofile")

source("analysis bmi/pcrab003_analytic dataset for data availability.R")
rm(anthro_followup,demographic,index_date,lab_followup); gc()


source("analysis bmi/pcrab102_selecting high dimensional variables based on fdr.R")

lookback_processed <- readRDS(paste0(path_pasc_cmr_folder,"/working/models pcrab/pcrab002_imputed lookback dataset.RDS")) %>% 
  mutate(EXPOSED = case_when(COHORT == "exposed" ~ 1,
                             TRUE ~ 0),
         UNEXPOSED = case_when(COHORT == "unexposed" ~ 1,
                               TRUE ~ 0),
         HISTORICAL = case_when(COHORT == "historical" ~ 1,
                                TRUE ~ 0))

# Loads HD dataset + restricts based on a cutoff
source("analysis bmi/pcrab103_restricting high dimensional covariates based on frequency.R")



outcome_df <- lookback_processed %>% 
  # dplyr::filter(ID %in% bmi_ID) %>% 
  inner_join(hd_dataset_COHORT %>% 
              dplyr::select(ID, one_of(restricted_hdvars)),
            by = "ID")
write_parquet(outcome_df,paste0(path_pasc_cmr_folder,"/working/models pcrab/pcrab104_ipw for cohort membership data.parquet"))
