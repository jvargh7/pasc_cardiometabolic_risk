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




bmi_followup_ID = anthro_followup %>% 
  dplyr::filter(!is.na(bmi)) %>% 
  dplyr::select(ID) %>% 
  pull()

sbp_followup_ID = anthro_followup %>% 
  dplyr::filter(!is.na(SYSTOLIC)) %>% 
  dplyr::select(ID) %>% 
  pull()

ldl_followup_ID = lab_followup %>% 
  dplyr::filter(!is.na(ldl)) %>% 
  dplyr::select(ID) %>% 
  pull()


outcome_availability = demographic %>% 
  dplyr::select(ID, COHORT,matchid) %>% 
  mutate(in_bmi_lookback_ID = case_when(ID %in% lookback_df$ID ~ 1,
                                        TRUE ~ 0),
         in_bmi_followup_ID = case_when(ID %in% bmi_followup_ID ~ 1,
                               TRUE ~ 0),
         in_sbp_followup_ID = case_when(ID %in% sbp_followup_ID ~ 1,
                               TRUE ~ 0),
         in_ldl_followup_ID = case_when(ID %in% ldl_followup_ID ~ 1,
                               TRUE ~ 0))


analytic_sample = outcome_availability %>% 
  dplyr::filter(in_bmi_lookback_ID == 1)

sum(analytic_sample$in_bmi_followup_ID)
