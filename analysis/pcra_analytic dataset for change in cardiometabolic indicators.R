lookback_processed <- readRDS(paste0(path_pasc_cmr_folder,"/working/models/pcra102_imputed lookback dataset.RDS"))

tx_weights_df <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/ip weights for COHORT.RDS"))
mo_weights_df <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/ip weights for missing outcomes.RDS"))

imbalanced_variables <- c("age","antihypertensives")


bmi_df = anthro_followup %>% 
  dplyr::filter(!is.na(bmi)) %>% 
  mutate(t = as.numeric(t)) %>% 
  left_join(tx_weights_df %>% 
              dplyr::select(ID,sipw,sipw_sex,sipw_age,sipw_raceeth,sex_category,age_category,raceeth_category),
            by = "ID") %>% 
  left_join(mo_weights_df %>% 
              dplyr::select(ID,bmi_weights),
            by = "ID") %>% 
  left_join(lookback_processed %>% 
              dplyr::select(ID,one_of(imbalanced_variables),bmi) %>% 
              rename(lb_bmi = bmi),
            by = "ID") %>% 
  mutate(w = sipw*bmi_weights,
         w_sex = sipw_sex*bmi_weights,
         w_age = sipw_age*bmi_weights,
         w_raceeth = sipw_raceeth*bmi_weights) %>% 
  arrange(ID,t) %>% 
  mutate(ID = factor(ID),
         COHORT = factor(COHORT,levels=c("historical","unexposed","exposed")))

sbp_df = anthro_followup %>% 
  dplyr::filter(!is.na(SYSTOLIC)) %>% 
  mutate(t = as.numeric(t)) %>% 
  left_join(tx_weights_df %>% 
              dplyr::select(ID,sipw,sipw_sex,sipw_age,sipw_raceeth,sex_category,age_category,raceeth_category),
            by = "ID") %>% 
  left_join(mo_weights_df %>% 
              dplyr::select(ID,sbp_weights),
            by = "ID") %>% 
  left_join(lookback_processed %>% 
              dplyr::select(ID,one_of(imbalanced_variables),SYSTOLIC) %>% 
              rename(lb_SYSTOLIC = SYSTOLIC),
            by = "ID") %>% 
  mutate(w = sipw*sbp_weights,
         w_sex = sipw_sex*sbp_weights,
         w_age = sipw_age*sbp_weights,
         w_raceeth = sipw_raceeth*sbp_weights) %>% 
  arrange(ID,t) %>% 
  mutate(ID = factor(ID),
         COHORT = factor(COHORT,levels=c("historical","unexposed","exposed")))

ldl_df = lab_followup %>% 
  dplyr::filter(!is.na(ldl)) %>% 
  mutate(t = as.numeric(t)) %>% 
  left_join(tx_weights_df %>% 
              dplyr::select(ID,sipw,sipw_sex,sipw_age,sipw_raceeth,sex_category,age_category,raceeth_category),
            by = "ID") %>% 
  left_join(mo_weights_df %>% 
              dplyr::select(ID,ldl_weights),
            by = "ID") %>% 
  left_join(lookback_processed %>% 
              dplyr::select(ID,one_of(imbalanced_variables),ldl) %>% 
              rename(lb_ldl = ldl),
            by = "ID") %>% 
  mutate(w = sipw*ldl_weights,
         w_sex = sipw_sex*ldl_weights,
         w_age = sipw_age*ldl_weights,
         w_raceeth = sipw_raceeth*ldl_weights) %>% 
  arrange(ID,t) %>% 
  mutate(ID = factor(ID),
         COHORT = factor(COHORT,levels=c("historical","unexposed","exposed")))