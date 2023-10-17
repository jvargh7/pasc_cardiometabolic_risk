rm(list=ls());gc();source(".Rprofile")
source("analysis bmi/pcrab003_analytic dataset for data availability.R")
# source("analysis bmi/pcrabaux_ipw formula and variables.R")

# imbalanced_variables come from pcra_analytic dataset for change in cardiometabolic indicators.R 
imbalanced_variables <- c("age","antihypertensives","nhblack","hispanic","smoking","site","hospitalization","hba1c","serum_creatinine","hdl","ldl")


library(mice)
mi_dfs <- readRDS(paste0(path_pasc_cmr_folder,"/working/sensitivity mice/pcrsm002_mi_dfs.RDS"))

library(geepack)

bmi_fit_sex <- bmi_fit_age <- bmi_fit_raceeth <- bmi_fit_hospitalization <- list()

for (i in 1:5){
  imputed_dataset = complete(mi_dfs,i)
  source("sensitivity mice/pcrsm302_analytic dataset with ip weights for bmi.R")
  bmi_fit_sex[[i]] <- geeglm(formula = as.formula(paste0("bmi ~ COHORT*sex_category*t + lb_bmi +",paste0(imbalanced_variables,collapse=" + "))),
                        data = bmi_df,weights=w_sex,id = ID,corstr="exchangeable")
  bmi_fit_age[[i]] <- geeglm(formula = as.formula(paste0("bmi ~ COHORT*age_category*t + lb_bmi +",paste0(imbalanced_variables,collapse=" + "))),
                        data = bmi_df,weights=w_age,id = ID,corstr="exchangeable")
  bmi_fit_raceeth[[i]] <- geeglm(formula = as.formula(paste0("bmi ~ COHORT*raceeth_category*t + lb_bmi +",
                                                        paste0(imbalanced_variables[!imbalanced_variables %in% 
                                                                                      c("nhwhite","nhblack","hispanic","nhother")],
                                                               collapse=" + "))),
                            data = bmi_df %>% dplyr::filter(raceeth_category %in% c("NH White","NH Black","Hispanic")),weights=w_raceeth,id = ID,corstr="exchangeable")
  bmi_fit_hospitalization[[i]] <- geeglm(formula = as.formula(paste0("bmi ~ COHORT*hospitalization*t + lb_bmi +",
                                                                paste0(imbalanced_variables[!imbalanced_variables %in% 
                                                                                              c("hospitalization")],
                                                                       collapse=" + "))),
                                    data = bmi_df,weights=w_hospitalization,id = ID,corstr="exchangeable")
  
  rm(bmi_df)
  
}

library(geepack)

# Save -------

source("C:/code/external/functions/imputation/save_mi_geeglm.R")
save_mi_geeglm(bmi_fit_sex) %>% 
  saveRDS(paste0(path_pasc_cmr_folder,"/working/sensitivity mice/pcrsm402 bmi_fit_sex.RDS"))

save_mi_geeglm(bmi_fit_age) %>% 
  saveRDS(paste0(path_pasc_cmr_folder,"/working/sensitivity mice/pcrsm402 bmi_fit_age.RDS"))

save_mi_geeglm(bmi_fit_raceeth) %>% 
  saveRDS(paste0(path_pasc_cmr_folder,"/working/sensitivity mice/pcrsm402 bmi_fit_raceeth.RDS"))

save_mi_geeglm(bmi_fit_hospitalization) %>% 
  saveRDS(paste0(path_pasc_cmr_folder,"/working/sensitivity mice/pcrsm402 bmi_fit_hospitalization.RDS"))


library(sjPlot)

bmi_plot_sex <- bmi_plot_age <- bmi_plot_raceeth <- bmi_plot_hospitalization <- list()

for(i in 1:5){
  
  bmi_plot_sex[[i]] <- plot_model(bmi_fit_sex[[i]],type="pred",terms=c("t","COHORT","sex_category"),
                                  # ci.lvl = NA,
                                  axis.lim = list(c(0,100),c(28,32)),
                                  vcov.fun = vcov())
  
  bmi_plot_age[[i]] <- plot_model(bmi_fit_age[[i]],type="pred",terms=c("t","COHORT","age_category"),
                                  # ci.lvl = NA,
                                  axis.lim = list(c(0,100),c(28,32)),
                                  vcov.fun = vcov())
  
  bmi_plot_raceeth[[i]] <- plot_model(bmi_fit_raceeth[[i]],type="pred",terms=c("t","COHORT","raceeth_category"),
                                      # ci.lvl = NA,
                                      axis.lim = list(c(0,100),c(28,32)),
                                      vcov.fun = vcov())
  
  bmi_plot_hospitalization[[i]] <- plot_model(bmi_fit_hospitalization[[i]],type="pred",terms=c("t","COHORT","hospitalization"),
                                              # ci.lvl = NA,
                                              axis.lim = list(c(0,100),c(28,32)),
                                              vcov.fun = vcov())
  
  
}


map_dfr(1:5,
         function(i){
           
           bind_rows(
             bmi_plot_sex[[i]]$data %>% data.frame() %>% mutate(outcome = "bmi",modifier = "sex"),
             bmi_plot_age[[i]]$data %>% data.frame() %>% mutate(outcome = "bmi",modifier = "age"),
             bmi_plot_raceeth[[i]]$data %>% data.frame() %>% mutate(outcome = "bmi",modifier = "raceeth"),
             bmi_plot_hospitalization[[i]]$data %>% data.frame() %>% mutate(outcome = "bmi",modifier = "hospitalization")
           ) %>% 
             mutate(index = i)
           
         }) %>% 
  write_csv(.,file="sensitivity mice/pcrsm402_marginal predictions at t by COHORT and modifier.csv")

