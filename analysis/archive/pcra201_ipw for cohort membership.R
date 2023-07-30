rm(list=ls());gc();source(".Rprofile")

source("analysis/pcra_analytic dataset for data availability.R")
rm(anthro_followup,demographic,index_date,lab_followup); gc()

source("analysis/pcra_selecting high dimensional variables based on fdr.R")

lookback_processed <- readRDS(paste0(path_pasc_cmr_folder,"/working/models/pcra102_imputed lookback dataset.RDS"))

hd_dataset_COHORT <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/high dimensional dataset for analysis.RDS")) %>% 
  dplyr::select(ID,ends_with("gtOne"),ends_with("gtMedian"),ends_with("gtQ3")) %>% 
  rename_all(~str_replace(.,"\\-","_")) %>% 
  dplyr::select(ID,one_of(selected_hdvars %>% 
                            # dplyr::filter(outcome == "bmi") %>% 
                            dplyr::select(variable) %>% 
                            pull() %>% unique()))

outcome_df <- lookback_processed %>% 
  # dplyr::filter(ID %in% bmi_ID) %>% 
  left_join(hd_dataset_COHORT,
            by = "ID")
write_parquet(outcome_df,paste0(path_pasc_cmr_folder,"/working/models/pcra201_ipw for cohort membership data.parquet"))
  
rm(hd_dataset_COHORT,lookback_processed); gc()

library(tidymodels)
tidymodels_prefer()
set.seed(501)
# The list of variables in outcome_df is determined by hd_dataset_<outcome> based on what is associated with the outcome under consideration
# Separate models for loss to follow-up should be fit for each outcome
data_split = initial_split(outcome_df,prop=0.7,strata = COHORT)

# MODIFY: THIS IS AN IMPORTANT GLOBAL VARIABLE -----
# Refer to pcra_workflow ipw for outcome.R
outcome = "COHORT"

source("analysis/pcra_workflow ipw for outcome.R")

saveRDS(tune_grid_predictions,paste0(path_pasc_cmr_folder,"/working/models/pcra201_tune grid predictions from 5CV RF COHORT.RDS"))
write_csv(tune_grid_metrics,"analysis/pcra201_collected metrics from 5CV RF COHORT.csv")
saveRDS(final_model,paste0(path_pasc_cmr_folder,"/working/models/pcra201_best fit model from FINAL RF COHORT.RDS"))
