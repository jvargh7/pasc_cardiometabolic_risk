rm(list=ls());gc();source(".Rprofile")
source("analysis/pcra_analytic dataset for data availability.R")
source("analysis/pcra_ipw formula for main analysis.R")
source("functions/followup_summary.R")
source("analysis/pcra_analytic dataset for change in cardiometabolic indicators.R")

first_followup <- bind_rows(bmi_df %>% 
                              group_by(ID) %>% 
                              dplyr::filter(t == min(t)) %>% 
                              ungroup() %>% 
                              group_by(COHORT) %>% 
                              summarize(summary = followup_summary(t)) %>% 
                              mutate(var = "bmi"),
                            sbp_df %>% 
                              group_by(ID) %>% 
                              dplyr::filter(t == min(t)) %>% 
                              ungroup() %>%  
                              group_by(COHORT) %>% 
                              summarize(summary = followup_summary(t)) %>%
                              mutate(var = "sbp"),
                            ldl_df %>% 
                              group_by(ID) %>% 
                              dplyr::filter(t == min(t)) %>% 
                              ungroup() %>%  
                              group_by(COHORT) %>% 
                              summarize(summary = followup_summary(t)) %>% 
                              mutate(var = "ldl")
) %>% 
  pivot_wider(names_from="COHORT",values_from="summary") %>% 
  dplyr::select(var,exposed,unexposed,historical)


var_first_followup <- bind_rows(bmi_df %>% 
                              group_by(ID) %>% 
                              dplyr::filter(t == min(t)) %>% 
                              ungroup() %>% 
                              group_by(COHORT) %>% 
                              summarize(summary = followup_summary(bmi)) %>% 
                              mutate(var = "bmi"),
                            sbp_df %>% 
                              group_by(ID) %>% 
                              dplyr::filter(t == min(t)) %>% 
                              ungroup() %>%  
                              group_by(COHORT) %>% 
                              summarize(summary = followup_summary(SYSTOLIC)) %>%
                              mutate(var = "sbp"),
                            ldl_df %>% 
                              group_by(ID) %>% 
                              dplyr::filter(t == min(t)) %>% 
                              ungroup() %>%  
                              group_by(COHORT) %>% 
                              summarize(summary = followup_summary(ldl)) %>% 
                              mutate(var = "ldl")
) %>% 
  pivot_wider(names_from="COHORT",values_from="summary") %>% 
  dplyr::select(var,exposed,unexposed,historical)


n_followup <- bind_rows(bmi_df %>% 
                          group_by(COHORT,ID) %>% 
                          tally() %>% 
                          ungroup() %>% 
                          group_by(COHORT) %>% 
                          summarize(summary = followup_summary(n)) %>% 
                          mutate(var = "bmi"),
                        sbp_df %>% 
                          group_by(COHORT,ID) %>% 
                          tally() %>% 
                          ungroup() %>%  
                          group_by(COHORT) %>% 
                          summarize(summary = followup_summary(n)) %>%
                          mutate(var = "sbp"),
                        ldl_df %>% 
                          group_by(COHORT,ID) %>% 
                          tally() %>% 
                          ungroup() %>%  
                          group_by(COHORT) %>% 
                          summarize(summary = followup_summary(n)) %>% 
                          mutate(var = "ldl")
) %>% 
  pivot_wider(names_from="COHORT",values_from="summary") %>% 
  dplyr::select(var,exposed,unexposed,historical)

max_duration_followup <- bind_rows(bmi_df %>% 
                          group_by(COHORT,ID) %>% 
                          summarize(duration = max(t)) %>% 
                          ungroup() %>% 
                          group_by(COHORT) %>% 
                          summarize(summary = followup_summary(duration)) %>% 
                          mutate(var = "bmi"),
                        sbp_df %>% 
                          group_by(COHORT,ID) %>% 
                          summarize(duration = max(t)) %>% 
                          ungroup() %>%  
                          group_by(COHORT) %>% 
                          summarize(summary = followup_summary(duration)) %>%
                          mutate(var = "sbp"),
                        ldl_df %>% 
                          group_by(COHORT,ID) %>% 
                          summarize(duration = max(t)) %>% 
                          ungroup() %>%  
                          group_by(COHORT) %>% 
                          summarize(summary = followup_summary(duration)) %>% 
                          mutate(var = "ldl")
) %>% 
  pivot_wider(names_from="COHORT",values_from="summary") %>% 
  dplyr::select(var,exposed,unexposed,historical)


max_change_followup <- bind_rows(bmi_df %>% 
                                   group_by(COHORT,ID) %>% 
                                   summarize(change = max(bmi) - min(bmi)) %>% 
                                   ungroup() %>% 
                                   group_by(COHORT) %>% 
                                   summarize(summary = followup_summary(change)) %>% 
                                   mutate(var = "bmi"),
                                 sbp_df %>% 
                                   group_by(COHORT,ID) %>% 
                                   summarize(change = max(SYSTOLIC) - min(SYSTOLIC)) %>% 
                                   ungroup() %>%  
                                   group_by(COHORT) %>% 
                                   summarize(summary = followup_summary(change)) %>%
                                   mutate(var = "sbp"),
                                 ldl_df %>% 
                                   group_by(COHORT,ID) %>% 
                                   summarize(change = max(ldl) - min(ldl)) %>% 
                                   ungroup() %>%  
                                   group_by(COHORT) %>% 
                                   summarize(summary = followup_summary(change)) %>% 
                                   mutate(var = "ldl")
) %>% 
  pivot_wider(names_from="COHORT",values_from="summary") %>% 
  dplyr::select(var,exposed,unexposed,historical)


bind_rows(first_followup %>% mutate(type = "Time to first follow-up visit"),
          var_first_followup %>% mutate(type = "Variable at first follow-up (units)"),
          n_followup %>% mutate(type = "Number of follow-ups"),
          max_duration_followup %>% mutate(type = "Maximum duration of follow-up (days)"),
          max_change_followup %>% mutate(type = "Maximum change in variable during follow-up (units)")
          ) %>% 
  arrange(var) %>% 
  write_csv(.,file="paper/table_summary of outcomes during followup.csv")
