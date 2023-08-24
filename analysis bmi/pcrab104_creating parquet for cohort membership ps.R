rm(list=ls());gc();source(".Rprofile")

source("analysis bmi/pcrab003_analytic dataset for data availability.R")
rm(anthro_followup,demographic,index_date,lab_followup); gc()

source("analysis bmi/pcrab102_selecting high dimensional variables based on fdr.R")

lookback_processed <- readRDS(paste0(path_pasc_cmr_folder,"/working/models pcrab/pcrab004_imputed lookback dataset.RDS")) %>% 
  mutate(EXPOSED = case_when(COHORT == "exposed" ~ 1,
                             TRUE ~ 0),
         UNEXPOSED = case_when(COHORT == "unexposed" ~ 1,
                               TRUE ~ 0),
         HISTORICAL = case_when(COHORT == "historical" ~ 1,
                                TRUE ~ 0))

hd_dataset_COHORT <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/high dimensional dataset for analysis.RDS")) %>% 
  dplyr::select(ID,ends_with("gtOne"),ends_with("gtMedian"),ends_with("gtQ3")) %>% 
  rename_all(~str_replace(.,"\\-","_")) %>% 
  dplyr::select(ID,one_of(selected_hdvars %>% 
                            dplyr::filter(outcome == "bmi") %>%
                            dplyr::select(variable) %>% 
                            pull() %>% unique()))

outcome_df <- lookback_processed %>% 
  # dplyr::filter(ID %in% bmi_ID) %>% 
  left_join(hd_dataset_COHORT,
            by = "ID")
write_parquet(outcome_df,paste0(path_pasc_cmr_folder,"/working/models pcrab/pcrab104_ipw for cohort membership data.parquet"))
