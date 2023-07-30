rm(list=ls());gc();source(".Rprofile")
source("analysis/pcra_analytic dataset for data availability.R")
source("analysis/pcra_ipw formula for main analysis.R")

# imbalanced_variables come from pcra_analytic dataset for change in cardiometabolic indicators.R 
source("analysis/pcra_analytic dataset for change in cardiometabolic indicators.R")

library(geepack)

bmi_fit_sex <- geeglm(formula = as.formula(paste0("bmi ~ COHORT*sex_category*t + lb_bmi +",paste0(imbalanced_variables,collapse=" + "))),
                  data = bmi_df,weights=w_sex,id = ID,corstr="exchangeable")
bmi_fit_age <- geeglm(formula = as.formula(paste0("bmi ~ COHORT*age_category*t + lb_bmi +",paste0(imbalanced_variables,collapse=" + "))),
                      data = bmi_df,weights=w_sex,id = ID,corstr="exchangeable")
bmi_fit_raceeth <- geeglm(formula = as.formula(paste0("bmi ~ COHORT*raceeth_category*t + lb_bmi +",paste0(imbalanced_variables,collapse=" + "))),
                      data = bmi_df %>% dplyr::filter(raceeth_category %in% c("NH White","NH Black","Hispanic")),weights=w_sex,id = ID,corstr="exchangeable")
print("BMI completed")
sbp_fit_sex <- geeglm(formula = as.formula(paste0("SYSTOLIC ~ COHORT*sex_category*t + lb_SYSTOLIC +",paste0(imbalanced_variables,collapse=" + "))),
                  data = sbp_df,weights=w_sex,id = ID,corstr="exchangeable")
sbp_fit_age <- geeglm(formula = as.formula(paste0("SYSTOLIC ~ COHORT*age_category*t + lb_SYSTOLIC +",paste0(imbalanced_variables,collapse=" + "))),
                  data = sbp_df,weights=w_sex,id = ID,corstr="exchangeable")
sbp_fit_raceeth <- geeglm(formula = as.formula(paste0("SYSTOLIC ~ COHORT*raceeth_category*t + lb_SYSTOLIC +",paste0(imbalanced_variables,collapse=" + "))),
                  data = sbp_df %>% dplyr::filter(raceeth_category %in% c("NH White","NH Black","Hispanic")),weights=w_sex,id = ID,corstr="exchangeable")

print("SBP completed")
ldl_fit_sex <- geeglm(formula = as.formula(paste0("ldl ~ COHORT*sex_category*t + lb_ldl +",paste0(imbalanced_variables,collapse=" + "))),
                  data = ldl_df,weights=w_sex,id = ID,corstr="exchangeable")
ldl_fit_age <- geeglm(formula = as.formula(paste0("ldl ~ COHORT*age_category*t + lb_ldl +",paste0(imbalanced_variables,collapse=" + "))),
                  data = ldl_df,weights=w_sex,id = ID,corstr="exchangeable")
ldl_fit_raceeth <- geeglm(formula = as.formula(paste0("ldl ~ COHORT*raceeth_category*t + lb_ldl +",paste0(imbalanced_variables,collapse=" + "))),
                  data = ldl_df %>% dplyr::filter(raceeth_category %in% c("NH White","NH Black","Hispanic")),weights=w_sex,id = ID,corstr="exchangeable")
print("LDL completed")

# Save -------

source("C:/code/external/functions/imputation/save_mi_geeglm.R")
save_geeglm(bmi_fit_sex) %>% 
  saveRDS(paste0(path_pasc_cmr_folder,"/working/models/pcra302 bmi_fit_sex.RDS"))
save_geeglm(sbp_fit_sex) %>% 
  saveRDS(paste0(path_pasc_cmr_folder,"/working/models/pcra302 sbp_fit_sex.RDS"))
save_geeglm(ldl_fit_sex) %>% 
  saveRDS(paste0(path_pasc_cmr_folder,"/working/models/pcra302 ldl_fit_sex.RDS"))


save_geeglm(bmi_fit_age) %>% 
  saveRDS(paste0(path_pasc_cmr_folder,"/working/models/pcra302 bmi_fit_age.RDS"))
save_geeglm(sbp_fit_age) %>% 
  saveRDS(paste0(path_pasc_cmr_folder,"/working/models/pcra302 sbp_fit_age.RDS"))
save_geeglm(ldl_fit_age) %>% 
  saveRDS(paste0(path_pasc_cmr_folder,"/working/models/pcra302 ldl_fit_age.RDS"))

save_geeglm(bmi_fit_raceeth) %>% 
  saveRDS(paste0(path_pasc_cmr_folder,"/working/models/pcra302 bmi_fit_raceeth.RDS"))
save_geeglm(sbp_fit_raceeth) %>% 
  saveRDS(paste0(path_pasc_cmr_folder,"/working/models/pcra302 sbp_fit_raceeth.RDS"))
save_geeglm(ldl_fit_raceeth) %>% 
  saveRDS(paste0(path_pasc_cmr_folder,"/working/models/pcra302 ldl_fit_raceeth.RDS"))

# Coefficients -----

bind_rows(
  broom::tidy(bmi_fit_sex) %>% mutate(model = "PCRA302",outcome = "BMI",modifier = "sex"),
  broom::tidy(sbp_fit_sex) %>% mutate(model = "PCRA302",outcome = "SBP", modifier = "sex"),
  broom::tidy(ldl_fit_sex) %>% mutate(model = "PCRA302",outcome = "LDL", modifier = "sex"),
  broom::tidy(bmi_fit_age) %>% mutate(model = "PCRA302",outcome = "BMI",modifier = "age"),
  broom::tidy(sbp_fit_age) %>% mutate(model = "PCRA302",outcome = "SBP", modifier = "age"),
  broom::tidy(ldl_fit_age) %>% mutate(model = "PCRA302",outcome = "LDL", modifier = "age"),
  broom::tidy(bmi_fit_raceeth) %>% mutate(model = "PCRA302",outcome = "BMI",modifier = "raceeth"),
  broom::tidy(sbp_fit_raceeth) %>% mutate(model = "PCRA302",outcome = "SBP", modifier = "raceeth"),
  broom::tidy(ldl_fit_raceeth) %>% mutate(model = "PCRA302",outcome = "LDL", modifier = "raceeth")
) %>% 
  write_csv(.,"analysis/pcra302_coefficients cardiometabolic indicators sociodemographic.csv")


# Marginal plots ------
library(sjPlot)
bmi_plot_sex <- plot_model(bmi_fit_sex,type="pred",terms=c("t","COHORT","sex_category"),
                       # ci.lvl = NA,
                       axis.lim = list(c(0,100),c(28,32)),
                       vcov.fun = vcov())

bmi_plot_age <- plot_model(bmi_fit_age,type="pred",terms=c("t","COHORT","age_category"),
                           # ci.lvl = NA,
                           axis.lim = list(c(0,100),c(28,32)),
                           vcov.fun = vcov())

bmi_plot_raceeth <- plot_model(bmi_fit_raceeth,type="pred",terms=c("t","COHORT","raceeth_category"),
                           # ci.lvl = NA,
                           axis.lim = list(c(0,100),c(28,32)),
                           vcov.fun = vcov())


sbp_plot_sex <- plot_model(sbp_fit_sex,type="pred",terms=c("t","COHORT","sex_category"),
                           # ci.lvl = NA,
                           axis.lim = list(c(0,100),c(28,32)),
                           vcov.fun = vcov())

sbp_plot_age <- plot_model(sbp_fit_age,type="pred",terms=c("t","COHORT","age_category"),
                           # ci.lvl = NA,
                           axis.lim = list(c(0,100),c(28,32)),
                           vcov.fun = vcov())

sbp_plot_raceeth <- plot_model(sbp_fit_raceeth,type="pred",terms=c("t","COHORT","raceeth_category"),
                               # ci.lvl = NA,
                               axis.lim = list(c(0,100),c(28,32)),
                               vcov.fun = vcov())


ldl_plot_sex <- plot_model(ldl_fit_sex,type="pred",terms=c("t","COHORT","sex_category"),
                           # ci.lvl = NA,
                           axis.lim = list(c(0,100),c(28,32)),
                           vcov.fun = vcov())

ldl_plot_age <- plot_model(ldl_fit_age,type="pred",terms=c("t","COHORT","age_category"),
                           # ci.lvl = NA,
                           axis.lim = list(c(0,100),c(28,32)),
                           vcov.fun = vcov())

ldl_plot_raceeth <- plot_model(ldl_fit_raceeth,type="pred",terms=c("t","COHORT","raceeth_category"),
                               # ci.lvl = NA,
                               axis.lim = list(c(0,100),c(28,32)),
                               vcov.fun = vcov())

bind_rows(
          bmi_plot_sex$data %>% data.frame() %>% mutate(outcome = "bmi",modifier = "sex"),
          bmi_plot_age$data %>% data.frame() %>% mutate(outcome = "bmi",modifier = "age"),
          bmi_plot_raceeth$data %>% data.frame() %>% mutate(outcome = "bmi",modifier = "raceeth"),
          
          sbp_plot_sex$data %>% data.frame() %>% mutate(outcome = "sbp",modifier = "sex"),
          sbp_plot_age$data %>% data.frame() %>% mutate(outcome = "sbp",modifier = "age"),
          sbp_plot_raceeth$data %>% data.frame() %>% mutate(outcome = "sbp",modifier = "raceeth"),
          
          ldl_plot_sex$data %>% data.frame() %>% mutate(outcome = "ldl",modifier = "sex"),
          ldl_plot_age$data %>% data.frame() %>% mutate(outcome = "ldl",modifier = "age"),
          ldl_plot_raceeth$data %>% data.frame() %>% mutate(outcome = "ldl",modifier = "raceeth")
          ) %>% 
  write_csv(.,file="analysis/pcra302_marginal predictions at t by COHORT and modifier.csv")

