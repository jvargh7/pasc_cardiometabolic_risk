
lookback_df <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/lookback dataset for analysis.RDS"))
index_date <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/index date.RDS"))

bmi_followup <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/anthro followup.RDS")) %>% 
  dplyr::filter(!is.na(bmi)) %>% 
  group_by(ID) %>% 
  mutate(n_bmi = sum(!is.na(bmi))) %>% 
  dplyr::filter(t == min(t))

sbp_followup <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/anthro followup.RDS")) %>% 
  dplyr::filter(!is.na(SYSTOLIC)) %>% 
  group_by(ID) %>% 
  mutate(n_sbp = sum(!is.na(SYSTOLIC))) %>% 
  dplyr::filter(t == min(t))

ldl_followup <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/lab_followup_wide.RDS")) %>% 
  dplyr::filter(!is.na(ldl)) %>% 
  group_by(ID) %>% 
  mutate(n_ldl = sum(!is.na(ldl))) %>% 
  dplyr::filter(t == min(t))


bind_rows(bmi_followup,
          sbp_followup,
          ldl_followup) %>% 
  distinct(ID,.keep_all=TRUE) %>% 
  group_by(COHORT) %>% 
  tally()
