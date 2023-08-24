# Coming from preprocessing/pcpre402_creating lookback dataset.R

lookback_df <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/lookback dataset for analysis.RDS")) %>% 
  # Those who were not marked with a hospitalization ENC_TYPE are not hospitalized
  # Those who did not have a comorbidity in the last 730 days are treated as not having the disease
  # Those who were not prescribed a drug in the last 365 days are treated as not being prescribed the drug by anyone else
  mutate_at(vars(hospitalization,n_hospitalized,n_not_hospitalized,
                 obesity,cardiovascular,cerebrovascular,hypertension,pulmonary,hyperlipidemia,
                 antidepressants,
                 antipsychotics,antihypertensives,statins,immunosuppresants,
                 starts_with("lb")),function(x) case_when(is.na(x) ~ 0,
                                                          TRUE ~ x)) %>% 
  mutate(calendar_month = month.abb[month(index_date)])