rm(list=ls());gc();source(".Rprofile")

source("analysis/pcra_analytic dataset for data availability.R")
rm(anthro_followup,demographic,index_date,lab_followup); gc()

source("analysis/pcra_selecting high dimensional variables based on fdr.R")

hd_dataset_missing_outcome <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/high dimensional dataset for analysis.RDS")) %>% 
  dplyr::select(ID,ends_with("gtOne"),ends_with("gtMedian"),ends_with("gtQ3")) %>% 
  rename_all(~str_replace(.,"\\-","_")) %>% 
  dplyr::select(ID,one_of(selected_hdvars_censoring %>% 
                            # dplyr::filter(outcome == "bmi") %>% 
                            dplyr::select(variable) %>% 
                            pull() %>% unique()))


outcome_availability %>% 
  dplyr::select(-matchid) %>% 
  # dplyr::filter(ID %in% bmi_ID) %>% 
  left_join(hd_dataset_missing_outcome,
            by = "ID") %>% 
  write_csv(.,paste0(path_pasc_cmr_folder,"/working/models/pcra202_ipw for outcome availability dataset.parquet"))
