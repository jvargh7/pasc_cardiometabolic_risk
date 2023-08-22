rm(list=ls());gc();source(".Rprofile")

source("analysis/pcra_analytic dataset for data availability.R")
source("analysis/pcra_analytic dataset for change in cardiometabolic indicators.R")


unique_patients <- outcome_availability %>% 
  group_by(COHORT) %>% 
  summarize(bmi = sum(in_bmi_ID),
            sbp = sum(in_sbp_ID),
            ldl = sum(in_ldl_ID)) %>% 
  pivot_longer(cols=-one_of("COHORT"),names_to="var",values_to="val") %>% 
  pivot_wider(names_from="COHORT",values_from="val") %>% 
  dplyr::select(var,exposed,unexposed,historical)

unique_observations <- 
  bind_rows(bmi_df %>% 
  group_by(COHORT) %>% 
    tally() %>% 
    mutate(var = "bmi"),
  sbp_df %>% 
    group_by(COHORT) %>% 
    tally() %>% 
    mutate(var = "sbp"),
  ldl_df %>% 
    group_by(COHORT) %>% 
    tally() %>% 
    mutate(var = "ldl")
  ) %>% 
  pivot_wider(names_from="COHORT",values_from="n") %>% 
  dplyr::select(var,exposed,unexposed,historical)

bind_rows(unique_patients %>% mutate(type = "Unique Patients"),
          unique_observations %>% mutate(type = "Observations")) %>% 
  write_csv(.,file="paper/table_data availability by cohort.csv")

outcome_availability %>% 
  group_by(in_bmi_ID,in_sbp_ID
           # ,in_ldl_ID
           ) %>% tally() %>% 
  ungroup() %>% 
  mutate(across(matches("_ID"),function(x) case_when(x==1 ~ "Available",
                                          TRUE ~ "Unavailable"))) %>% 
  mutate(n = paste0(n," (",
                    round(n*100/sum(n),1),")")) %>% 
  write_csv(.,"paper/table_outcome availability overall.csv")
