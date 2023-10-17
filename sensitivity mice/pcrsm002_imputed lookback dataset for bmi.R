rm(list=ls());gc();source(".Rprofile")
library(mice)

source(paste0(path_pasc_cmr_repo,"/analysis bmi/pcrab001_processing before imputation and lookback bmi exclusion.R"))
source(paste0(path_pasc_cmr_repo,"/analysis bmi/pcrab004_matchid dataset.R"))

tx_weights_df <- readRDS(paste0(path_pasc_cmr_folder,"/working/models pcrab/pcrab301_ip weights for cohort membership.RDS"))
mo_weights_df <- readRDS(paste0(path_pasc_cmr_folder,"/working/models pcrab/pcrab301_ip weights for missing outcomes.RDS"))

imbalanced_variables <- c("age","antihypertensives","nhblack","hispanic","smoking","site","hospitalization",
                          "hba1c","serum_creatinine","hdl","ldl")

analytic_dataset_lookback <- lookback_df %>% 
  dplyr::filter(!is.na(bmi)) %>% 
  dplyr::select(ID, COHORT, matchid, 
                everything()) %>% 
  # From pcrab302_analytic dataset with ip weights for bmi.R
  dplyr::select(-matchid) %>% 
  left_join(matchid_df %>% dplyr::select(-COHORT),
            by = "ID")

bmi_df_summary = readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre401_anthro followup.RDS")) %>% 
  dplyr::filter(!is.na(bmi),ID %in% analytic_dataset_lookback$ID) %>% 
  mutate(t = as.numeric(t)) %>% 
  group_by(ID) %>% 
  summarize(mean_bmi = mean(bmi),
            sd_bmi = sd(bmi),
            min_bmi = min(bmi),
            max_bmi = max(bmi),
            mean_t = mean(t),
            sd_t = sd(t),
            min_t = min(t),
            max_t = max(t))


before_imputation <- analytic_dataset_lookback %>% 
  left_join(tx_weights_df %>%
              dplyr::select(ID,sipw,sipw_sex,sipw_age,sipw_raceeth,sipw_hospitalization,
                            sex_category,age_category,raceeth_category),
            by = "ID") %>%
  left_join(mo_weights_df %>%
              dplyr::select(ID,ltfu_weights),
            by = "ID") %>%
  left_join(bmi_df_summary,
            by = "ID") %>% 
  mutate(w = sipw*ltfu_weights,
         w_sex = sipw_sex*ltfu_weights,
         w_age = sipw_age*ltfu_weights,
         w_raceeth = sipw_raceeth*ltfu_weights,
         w_hospitalization = sipw_hospitalization*ltfu_weights) %>%
  mutate(ID = factor(ID),
         COHORT = factor(COHORT,levels=c("historical","unexposed","exposed"))) %>% 
  mutate(age_category_2 = case_when(age_category == "40 to 64" ~ 1,
                                    TRUE ~ 0),
         age_category_3 = case_when(age_category == "65 plus" ~ 1,
                                    TRUE ~ 0),
         raceeth_category_2 = case_when(raceeth_category == "NH Black" ~ 1,
                                        TRUE ~ 0),
         raceeth_category_3 = case_when(raceeth_category == "Hispanic" ~ 1,
                                        TRUE ~ 0),
         raceeth_category_4 = case_when(raceeth_category == "NH Other" ~ 1,
                                        TRUE ~ 0),
         sex_category_2 = case_when(sex_category == "Female" ~ 1,
                                    TRUE ~ 0),
         COHORThistorical = case_when(COHORT == "historical" ~ 1,
                                   TRUE ~ 0),
         COHORTunexposed = case_when(COHORT == "unexposed" ~ 1,
                                     TRUE ~ 0)) %>% 
  
  mutate(COHORThistorical_age_category_2 = case_when(COHORT == "historical" & age_category_2 == 1 ~ 1,
                                                  TRUE ~ 0),
         COHORThistorical_age_category_3 = case_when(COHORT == "historical" & age_category_3 == 1 ~ 1,
                                                  TRUE ~ 0),
         COHORTunexposed_age_category_2 = case_when(COHORT == "unexposed" & age_category_2 == 1 ~ 1,
                                                  TRUE ~ 0),
         COHORTunexposed_age_category_3 = case_when(COHORT == "unexposed" & age_category_3 == 1 ~ 1,
                                                  TRUE ~ 0),
         
         COHORThistorical_raceeth_category_2 = case_when(COHORT == "historical" & raceeth_category_2 == 1 ~ 1,
                                                      TRUE ~ 0),
         COHORThistorical_raceeth_category_3 = case_when(COHORT == "historical" & raceeth_category_3 == 1 ~ 1,
                                                      TRUE ~ 0),
         COHORThistorical_raceeth_category_4 = case_when(COHORT == "historical" & raceeth_category_4 == 1 ~ 1,
                                                      TRUE ~ 0),
         COHORTunexposed_raceeth_category_2 = case_when(COHORT == "unexposed" & raceeth_category_2 == 1 ~ 1,
                                                      TRUE ~ 0),
         COHORTunexposed_raceeth_category_3 = case_when(COHORT == "unexposed" & raceeth_category_3 == 1 ~ 1,
                                                      TRUE ~ 0),
         COHORTunexposed_raceeth_category_4 = case_when(COHORT == "unexposed" & raceeth_category_4 == 1 ~ 1,
                                                      TRUE ~ 0),
         
         COHORThistorical_sex_category_2 = case_when(COHORT == "historical" & sex_category_2 == 1 ~ 1,
                                                  TRUE ~ 0),
         COHORTunexposed_sex_category_2 = case_when(COHORT == "unexposed" & sex_category_2 == 1 ~ 1,
                                                  TRUE ~ 0),
         
         COHORThistorical_hospitalization = case_when(COHORT == "historical" & hospitalization == 1 ~ 1,
                                                   TRUE ~ 0),
         COHORTunexposed_hospitalization = case_when(COHORT == "unexposed" & hospitalization == 1 ~ 1,
                                                   TRUE ~ 0)) %>% 
  dplyr::select(-age_category,-raceeth_category,-sex_category,-COHORT,
                -nhblack,-hispanic,-nhother,-nhwhite,-female)


interaction_terms = c("COHORThistorical_age_category_2",
                      "COHORThistorical_age_category_3",
                      "COHORTunexposed_age_category_2",
                      "COHORTunexposed_age_category_3",
                      
                      "COHORThistorical_raceeth_category_2","COHORThistorical_raceeth_category_3",
                      "COHORThistorical_raceeth_category_4",
                      
                      "COHORTunexposed_raceeth_category_2","COHORTunexposed_raceeth_category_3",
                      "COHORTunexposed_raceeth_category_4",
                      
                      "COHORThistorical_sex_category_2","COHORTunexposed_sex_category_2",
                      "COHORThistorical_hospitalization","COHORTunexposed_hospitalization"
                      )

mi_null <- mice(before_imputation,
                maxit = 0)

method = mi_null$method
pred = mi_null$predictorMatrix

pred[c("ID","index_date","matchid"),] <- 0
pred[,c("ID","index_date","matchid")] <- 0

for(i_t in interaction_terms){
  print(i_t)
  exposure_term = str_extract(i_t,"^(COHORTunexposed|COHORThistorical)")
  em_term = str_replace(i_t,pattern=paste0(exposure_term,"_"),replacement = "")
  method[i_t] = paste0("~I(",exposure_term,"*",em_term,")")
  
  # Do not use interaction terms for imputation of the source variables
  pred[c(exposure_term,em_term),i_t] <- 0
}



# Takes ~4h
mi_dfs <- mice(before_imputation,
               method = method,
               pred = pred,
               m=5,maxit=30,seed=500)


saveRDS(mi_dfs, paste0(path_pasc_cmr_folder,"/working/sensitivity mice/pcrsm002_mi_dfs.RDS"))
