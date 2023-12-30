rm(list=ls());gc();source(".Rprofile")
source(paste0(path_pasc_cmr_repo,"/analysis bmi/pcrab001_processing before imputation and lookback bmi exclusion.R"))
# lookback_cpit2dm <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre209_cpit2dm diabetes during lookback period.RDS"))
# landmark_cpit2dm <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre208_cpit2dm new onset diabetes during period till origin date.RDS"))


lookback_df <- lookback_df %>% 
  dplyr::filter(!is.na(bmi)
                # , !ID %in% lookback_cpit2dm$ID
                )

index_date <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre201_index date.RDS"))
demographic <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre101_demographic.RDS"))
anthro_followup <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre401_anthro followup.RDS"))
lab_followup <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre401_lab_followup_wide.RDS"))
# mi_dfs <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/lookback mi_dfs.RDS"))

# Exclusions ------

anthro_followup_summary <- anthro_followup %>% 
  group_by(ID) %>% 
  summarize(n_bmi = sum(!is.na(bmi)),
            max_t = max(t)) %>% 
  dplyr::filter(n_bmi >= 2, max_t >= 100)

saveRDS(anthro_followup_summary,paste0(path_pasc_cmr_folder,"/working/sensitivity utilization/pcrsu003_anthro followup summary.RDS"))

# Follow-up IDs --------

outcome_availability = demographic %>% 
  dplyr::select(ID, COHORT,matchid) %>% 
  mutate(in_sensitivity_utilization_ID = case_when(ID %in% anthro_followup_summary$ID ~ 1,
                                        TRUE ~ 0),
         in_bmi_lookback_ID = case_when(ID %in% lookback_df$ID ~ 1,
                                        TRUE ~ 0))


analytic_sample = outcome_availability %>% 
  dplyr::filter(in_bmi_lookback_ID == 1)

sum(analytic_sample$in_sensitivity_utilization_ID)
