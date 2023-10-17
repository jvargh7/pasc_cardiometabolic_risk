rm(list=ls());gc();source(".Rprofile")
# source("analysis bmi/pcrab003_analytic dataset for data availability.R")
source("analysis bmi/pcrabaux_ipw formula and variables.R")

# imbalanced_variables come from pcrab302_analytic dataset with ip weights for bmi 
source("sensitivity utilization/pcrsu302_analytic dataset with ip weights for bmi.R")

library(geepack)

bmi_fit_sex <- geeglm(formula = as.formula(paste0("bmi ~ COHORT*sex_category*t + lb_bmi +",paste0(imbalanced_variables,collapse=" + "))),
                  data = bmi_df,weights=w_sex,id = ID,corstr="exchangeable")
bmi_fit_age <- geeglm(formula = as.formula(paste0("bmi ~ COHORT*age_category*t + lb_bmi +",paste0(imbalanced_variables,collapse=" + "))),
                      data = bmi_df,weights=w_age,id = ID,corstr="exchangeable")
bmi_fit_raceeth <- geeglm(formula = as.formula(paste0("bmi ~ COHORT*raceeth_category*t + lb_bmi +",
                                                      paste0(imbalanced_variables[!imbalanced_variables %in% 
                                                                                    c("nhwhite","nhblack","hispanic","nhother")],
                                                             collapse=" + "))),
                      data = bmi_df %>% dplyr::filter(raceeth_category %in% c("NH White","NH Black","Hispanic")),weights=w_raceeth,id = ID,corstr="exchangeable")
bmi_fit_hospitalization <- geeglm(formula = as.formula(paste0("bmi ~ COHORT*hospitalization*t + lb_bmi +",
                                                              paste0(imbalanced_variables[!imbalanced_variables %in% 
                                                                                            c("hospitalization")],
                                                                     collapse=" + "))),
                      data = bmi_df,weights=w_hospitalization,id = ID,corstr="exchangeable")

# Save -------

source("C:/code/external/functions/imputation/save_mi_geeglm.R")
save_geeglm(bmi_fit_sex) %>% 
  saveRDS(paste0(path_pasc_cmr_folder,"/working/sensitivity utilization/pcrsu402 bmi_fit_sex.RDS"))

save_geeglm(bmi_fit_age) %>% 
  saveRDS(paste0(path_pasc_cmr_folder,"/working/sensitivity utilization/pcrsu402 bmi_fit_age.RDS"))

save_geeglm(bmi_fit_raceeth) %>% 
  saveRDS(paste0(path_pasc_cmr_folder,"/working/sensitivity utilization/pcrsu402 bmi_fit_raceeth.RDS"))

save_geeglm(bmi_fit_hospitalization) %>% 
  saveRDS(paste0(path_pasc_cmr_folder,"/working/sensitivity utilization/pcrsu402 bmi_fit_hospitalization.RDS"))


# Coefficients -----

bind_rows(
  broom::tidy(bmi_fit_sex) %>% mutate(model = "PCRSU402",outcome = "BMI",modifier = "sex"),

  broom::tidy(bmi_fit_age) %>% mutate(model = "PCRSU402",outcome = "BMI",modifier = "age"),

  broom::tidy(bmi_fit_raceeth) %>% mutate(model = "PCRSU402",outcome = "BMI",modifier = "raceeth"),
  
  broom::tidy(bmi_fit_hospitalization) %>% mutate(model = "PCRSU402",outcome = "BMI",modifier = "hospitalization")
) %>% 
  write_csv(.,"sensitivity utilization/pcrsu402_coefficients cardiometabolic indicators sociodemographic.csv")


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

bmi_plot_hospitalization <- plot_model(bmi_fit_hospitalization,type="pred",terms=c("t","COHORT","hospitalization"),
                               # ci.lvl = NA,
                               axis.lim = list(c(0,100),c(28,32)),
                               vcov.fun = vcov())

bind_rows(
          bmi_plot_sex$data %>% data.frame() %>% mutate(outcome = "bmi",modifier = "sex"),
          bmi_plot_age$data %>% data.frame() %>% mutate(outcome = "bmi",modifier = "age"),
          bmi_plot_raceeth$data %>% data.frame() %>% mutate(outcome = "bmi",modifier = "raceeth"),
          bmi_plot_hospitalization$data %>% data.frame() %>% mutate(outcome = "bmi",modifier = "hospitalization")
           ) %>% 
  write_csv(.,file="sensitivity utilization/pcrsu402_marginal predictions at t by COHORT and modifier.csv")

