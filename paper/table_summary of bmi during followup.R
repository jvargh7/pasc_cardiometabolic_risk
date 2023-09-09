rm(list=ls());gc();source(".Rprofile")
source("analysis bmi/pcrab003_analytic dataset for data availability.R")
source("analysis bmi/pcrabaux_ipw formula and variables.R")
source("functions/followup_summary.R")
source("analysis bmi/pcrab302_analytic dataset with ip weights for bmi.R")




first_followup <- bmi_df %>% 
  group_by(ID) %>% 
  dplyr::filter(t == min(t)) %>% 
  ungroup() %>% 
  group_by(COHORT) %>% 
  summarize(summary = followup_summary(t)) %>% 
  mutate(var = "bmi") %>% 
  pivot_wider(names_from="COHORT",values_from="summary") %>% 
  dplyr::select(var,exposed,unexposed,historical)


var_first_followup <- bmi_df %>% 
                                  group_by(ID) %>% 
                                  dplyr::filter(t == min(t)) %>% 
                                  ungroup() %>% 
                                  group_by(COHORT) %>% 
                                  summarize(summary = followup_summary(bmi)) %>% 
                                  mutate(var = "bmi") %>% 
  pivot_wider(names_from="COHORT",values_from="summary") %>% 
  dplyr::select(var,exposed,unexposed,historical)


n_followup <- bmi_df %>% 
                          group_by(COHORT,ID) %>% 
                          tally() %>% 
                          ungroup() %>% 
                          group_by(COHORT) %>% 
                          summarize(summary = followup_summary(n)) %>% 
                          mutate(var = "bmi") %>% 
  pivot_wider(names_from="COHORT",values_from="summary") %>% 
  dplyr::select(var,exposed,unexposed,historical)

max_duration_followup <- bmi_df %>% 
                                     group_by(COHORT,ID) %>% 
                                     summarize(duration = max(t)) %>% 
                                     ungroup() %>% 
                                     group_by(COHORT) %>% 
                                     summarize(summary = followup_summary(duration)) %>% 
                                     mutate(var = "bmi") %>% 
  pivot_wider(names_from="COHORT",values_from="summary") %>% 
  dplyr::select(var,exposed,unexposed,historical)


max_change_followup <- bmi_df %>% 
                                   group_by(COHORT,ID) %>% 
                                   summarize(change = max(bmi) - min(bmi)) %>% 
                                   ungroup() %>% 
                                   group_by(COHORT) %>% 
                                   summarize(summary = followup_summary(change)) %>% 
                                   mutate(var = "bmi") %>% 
  pivot_wider(names_from="COHORT",values_from="summary") %>% 
  dplyr::select(var,exposed,unexposed,historical)


bind_rows(first_followup %>% mutate(type = "Time to first follow-up visit"),
          var_first_followup %>% mutate(type = "Variable at first follow-up (units)"),
          n_followup %>% mutate(type = "Number of follow-ups"),
          max_duration_followup %>% mutate(type = "Maximum duration of follow-up (days)"),
          max_change_followup %>% mutate(type = "Maximum change in variable during follow-up (units)")
) %>% 
  arrange(var) %>% 
  write_csv(.,file="paper/table_summary of bmi during followup.csv")
