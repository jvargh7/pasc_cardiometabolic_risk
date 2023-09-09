rm(list=ls());gc();source(".Rprofile")

# Instead of taking lookback_df directly, use this
# This does a sensible imputation of 0 for hospitalizations, diagnosis codes, medication and lookback encounter counts
source("analysis bmi/pcrab001_processing before imputation and lookback bmi exclusion.R")

lookback_df %>% 
  dplyr::filter(!is.na(bmi)) %>% 
  mutate(bmi_category = case_when(bmi < 18.5 ~ "Underweight",
                                  bmi >= 18.5 & bmi < 25.0 ~ "Normal",
                                  bmi >= 25.0 & bmi < 30.0 ~ "Overweight",
                                  bmi >= 30.0 ~ "Obese",
                                  TRUE ~ NA_character_)) %>% 
  group_by(bmi_category) %>% 
  tally() %>% 
  mutate(p = n/sum(n))


lookback_df %>% 
  dplyr::filter(!is.na(bmi)) %>% 
  mutate(bmi_category = case_when(bmi < 18.5 ~ "Underweight",
                                  bmi >= 18.5 & bmi < 25.0 ~ "Normal",
                                  bmi >= 25.0 & bmi < 30.0 ~ "Overweight",
                                  bmi >= 30.0 ~ "Obese",
                                  TRUE ~ NA_character_)) %>% 
  group_by(bmi_category,COHORT) %>% 
  tally() %>% 
  ungroup() %>% 
  group_by(COHORT) %>% 
  mutate(p = n/sum(n)) %>% 
  mutate(n_p = paste0(n," (",round(p*100,1),"%)")) %>% 
  dplyr::select(COHORT,bmi_category,n_p) %>% 
  pivot_wider(names_from=bmi_category,values_from=n_p)
