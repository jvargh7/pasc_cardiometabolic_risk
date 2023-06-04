index_date <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/index date.RDS"))
demographic <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/demographic.RDS"))
anthro_followup <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/anthro followup.RDS"))
lab_followup <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/lab_followup_wide.RDS"))
mi_dfs <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/lookback mi_dfs.RDS"))


bmi_ID = anthro_followup %>% 
  dplyr::filter(!is.na(bmi)) %>% 
  dplyr::select(ID) %>% 
  pull()

sbp_ID = anthro_followup %>% 
  dplyr::filter(!is.na(SYSTOLIC)) %>% 
  dplyr::select(ID) %>% 
  pull()

ldl_ID = lab_followup %>% 
  dplyr::filter(!is.na(ldl)) %>% 
  dplyr::select(ID) %>% 
  pull()


outcome_availability = demographic %>% 
  dplyr::select(ID, COHORT,matchid) %>% 
  mutate(in_index_date = case_when(ID %in% index_date$ID ~ 1,
                                   TRUE ~ 0),
         in_bmi_ID = case_when(ID %in% bmi_ID ~ 1,
                               TRUE ~ 0),
         in_sbp_ID = case_when(ID %in% sbp_ID ~ 1,
                               TRUE ~ 0),
         in_ldl_ID = case_when(ID %in% ldl_ID ~ 1,
                               TRUE ~ 0))
