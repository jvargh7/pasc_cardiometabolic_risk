# Marginal estimates for COHORT at time = 0 (need to QC) -----------
source("C:/code/external/functions/imputation/marginal_geeglm.R")

bmi_COHORT_margin = sbp_COHORT_margin = ldl_COHORT_margin = list()


for (margin_value in c("exposed","unexposed","historical")){
  bmi_COHORT_margin[[margin_value]] = marginal_geeglm(bmi_fit,margin_var = "COHORT",
                                                      margin_value = margin_value,
                                                      modifier_var = "t",modifier_value = 0)
  sbp_COHORT_margin[[margin_value]] = marginal_geeglm(sbp_fit,margin_var = "COHORT",
                                                      margin_value = margin_value,
                                                      modifier_var = "t",modifier_value = 0)
  ldl_COHORT_margin[[margin_value]] = marginal_geeglm(ldl_fit,margin_var = "COHORT",
                                                      margin_value = margin_value,
                                                      modifier_var = "t",modifier_value = 0)
}

bind_rows(
  bind_rows(bmi_COHORT_margin) %>% mutate(margin_value = c("exposed","unexposed","historical"),
                                          outcome = "bmi"),
  bind_rows(sbp_COHORT_margin) %>% mutate(margin_value = c("exposed","unexposed","historical"),
                                          outcome = "sbp"),
  bind_rows(ldl_COHORT_margin) %>% mutate(margin_value = c("exposed","unexposed","historical"),
                                          outcome = "ldl") 
) %>% 
  mutate(coef_ci = paste0(round(mean,1),
                          " (",round(mean-1.96*se,1), ",",
                          round(mean+1.96*se,1),")")) %>% 
  write_csv(.,"analysis/pcra301_COHORT margin at time 0.csv")


# Marginal estimates for COHORT at time = 100 -------
bmi_COHORT_margin100 = sbp_COHORT_margin100 = ldl_COHORT_margin100 = list()


for (margin_value in c("exposed","unexposed","historical")){
  bmi_COHORT_margin[[margin_value]] = marginal_geeglm(bmi_fit,margin_var = "COHORT",
                                                      margin_value = margin_value,
                                                      modifier_var = "t",modifier_value = 100)
  sbp_COHORT_margin[[margin_value]] = marginal_geeglm(sbp_fit,margin_var = "COHORT",
                                                      margin_value = margin_value,
                                                      modifier_var = "t",modifier_value = 100)
  ldl_COHORT_margin[[margin_value]] = marginal_geeglm(ldl_fit,margin_var = "COHORT",
                                                      margin_value = margin_value,
                                                      modifier_var = "t",modifier_value = 100)
}

bind_rows(
  bind_rows(bmi_COHORT_margin) %>% mutate(margin_value = c("exposed","unexposed","historical"),
                                          outcome = "bmi"),
  bind_rows(sbp_COHORT_margin) %>% mutate(margin_value = c("exposed","unexposed","historical"),
                                          outcome = "sbp"),
  bind_rows(ldl_COHORT_margin) %>% mutate(margin_value = c("exposed","unexposed","historical"),
                                          outcome = "ldl") 
) %>% 
  mutate(coef_ci = paste0(round(mean,1),
                          " (",round(mean-1.96*se,1), ",",
                          round(mean+1.96*se,1),")")) %>% 
  write_csv(.,"analysis/pcra301_COHORT margin at time 100.csv")


# Contrasts at t = 100 ------
source("C:/code/external/functions/imputation/clean_mi_contrasts.R")
bmi_COHORTexposed = contrasts_geeglm(fit = bmi_fit,model_matrix = NULL,
                                     exposure = "COHORTexposed",modifier = "t",vcov_type = "robust",
                                     exposure_value = 1, modifier_value = 100)

bmi_COHORTunexposed = contrasts_geeglm(fit =bmi_fit,model_matrix = NULL,
                                       exposure = "COHORTunexposed",modifier = "t",vcov_type = "robust",
                                       exposure_value = 1, modifier_value = 100)

sbp_COHORTexposed = contrasts_geeglm(fit = sbp_fit,model_matrix = NULL,
                                     exposure = "COHORTexposed",modifier = "t",vcov_type = "robust",
                                     exposure_value = 1, modifier_value = 100)

sbp_COHORTunexposed = contrasts_geeglm(fit =sbp_fit,model_matrix = NULL,
                                       exposure = "COHORTunexposed",modifier = "t",vcov_type = "robust",
                                       exposure_value = 1, modifier_value = 100)

ldl_COHORTexposed = contrasts_geeglm(fit =ldl_fit,model_matrix = NULL,
                                     exposure = "COHORTexposed",modifier = "t",vcov_type = "robust",
                                     exposure_value = 1, modifier_value = 100)

ldl_COHORTunexposed = contrasts_geeglm(fit =ldl_fit,model_matrix = NULL,
                                       exposure = "COHORTunexposed",modifier = "t",vcov_type = "robust",
                                       exposure_value = 1, modifier_value = 100)


bind_rows(
  bmi_COHORTexposed %>% mutate(outcome = "BMI",exposure = "Exposed",modifier = "'t'",modifier_value = 100),
  bmi_COHORTunexposed %>% mutate(outcome = "BMI",exposure = "Unexposed",modifier = "'t'",modifier_value = 100),
  
  sbp_COHORTexposed %>% mutate(outcome = "SBP",exposure = "Exposed",modifier = "'t'",modifier_value = 100),
  sbp_COHORTunexposed %>% mutate(outcome = "SBP",exposure = "Unexposed",modifier = "'t'",modifier_value = 100),
  
  ldl_COHORTexposed %>% mutate(outcome = "LDL",exposure = "Exposed",modifier = "'t'",modifier_value = 100),
  ldl_COHORTunexposed %>% mutate(outcome = "LDL",exposure = "Unexposed",modifier = "'t'",modifier_value = 100)
  
  
) %>% 
  write_csv(.,"analysis/pcra301_contrasts of change in cardiometabolic indicators.csv")




# Marginal estimates for time = 100 at COHORT ------------
source("C:/code/external/functions/imputation/contrasts_geeglm.R")
source("C:/code/external/functions/preprocessing/round_d.R")
bmi_t100_margin = sbp_t100_margin = ldl_t100_margin = list()

for (margin_value in c("exposed","unexposed","historical")){
  bmi_t100_margin[[margin_value]] = contrasts_geeglm(fit = bmi_fit,model_matrix = NULL,
                                                     modifier = paste0("COHORT",margin_value),exposure = "t",vcov_type = "robust",
                                                     exposure_value = 100, modifier_value = 1,e_m_term = FALSE) %>% 
    mutate(margin_value = margin_value)
  
  
  sbp_t100_margin[[margin_value]] = contrasts_geeglm(fit = sbp_fit,model_matrix = NULL,
                                                     modifier = paste0("COHORT",margin_value),exposure = "t",vcov_type = "robust",
                                                     exposure_value = 100, modifier_value = 1,e_m_term = FALSE) %>% 
    mutate(margin_value = margin_value)
  
  ldl_t100_margin[[margin_value]] = contrasts_geeglm(fit = ldl_fit,model_matrix = NULL,
                                                     modifier = paste0("COHORT",margin_value),exposure = "t",vcov_type = "robust",
                                                     exposure_value = 100, modifier_value = 1,e_m_term = FALSE) %>% 
    mutate(margin_value = margin_value)
  
}

bind_rows(
  bind_rows(bmi_t100_margin)  %>%  mutate(outcome = "bmi"),
  bind_rows(sbp_t100_margin) %>% mutate(outcome = "sbp"),
  bind_rows(ldl_t100_margin) %>% mutate(outcome = "ldl") 
) %>% 
  write_csv(.,"analysis/pcra301_t margin at time 100.csv")

