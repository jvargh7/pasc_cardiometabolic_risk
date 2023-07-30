rm(list=ls());gc();source(".Rprofile")


# Uses datasets generated from:
# preprocessing/pcrpre04_index date characteristics.R
# preprocessing/pcrpre05_clinical characteristics prior to infection.R
# peprocessing/pcrpre06_healthcare utilization during followup.R
# preprocessing/pcrpre07_community characteristics.R
index_date <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/index date.RDS"))

demographic <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/demographic.RDS"))
index_date_characteristics <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/index date characteristics.RDS"))
lookback_clinical_characteristics <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/lookback clinical characteristics.RDS"))
lb_healthcare_utilization <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/lookback healthcare utilization.RDS"))





lookback_df = demographic %>% 
  left_join(index_date_characteristics %>% 
              dplyr::select(ID,site,payer_type_primary,payer_type_secondary,
                            hospitalization,n_hospitalized,n_not_hospitalized,
                            p_hyperglycemia),
            by = "ID") %>% 
  left_join(
            lookback_clinical_characteristics %>% 
              dplyr::select(ID, bmi, HT, SYSTOLIC, smoking, obesity,cardiovascular,cerebrovascular,
                            hypertension,pulmonary,hyperlipidemia,antidepressants,
                            antipsychotics,antihypertensives,statins,immunosuppresants,
                            hba1c,glucose,alt,ast,serum_creatinine,hdl,ldl),
            by = "ID")  %>% 
  left_join(lb_healthcare_utilization %>% dplyr::select(-COHORT),
            by="ID") %>% 
  # Those values of HbA1c outside [3,20] are set to NA_real_
  mutate(hba1c = case_when(hba1c < 3 | hba1c > 20 ~ NA_real_,
                           TRUE ~ hba1c))
  
saveRDS(lookback_df,paste0(path_pasc_cmr_folder,"/working/cleaned/lookback dataset for analysis.RDS"))
