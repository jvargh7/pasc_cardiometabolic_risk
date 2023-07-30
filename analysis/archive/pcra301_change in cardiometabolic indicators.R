rm(list=ls());gc();source(".Rprofile")

tx_weights_df <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/ip weights for COHORT.RDS"))
source("analysis/pcra_analytic dataset for data availability.R")

w_cols = colnames(tx_weights_df)[str_detect(colnames(tx_weights_df),"W[0-9]+")]

library(geepack)
library(lme4)
bmi_fit = sbp_fit = ldl_fit = list()

for(i in 1:length(w_cols)){
  print(i)
  
  # https://stackoverflow.com/questions/27073002/geeglm-in-r-error-message-about-variable-lengths-differing
  # geeglm kept crashing because ID was a character variable and not a factor
  bmi_df = anthro_followup %>% 
    dplyr::filter(!is.na(bmi)) %>% 
    mutate(t = as.numeric(t)) %>%
    rename(w = w_cols[i]) %>% 
    arrange(ID,t) %>% 
    mutate(ID = factor(ID)) 
  
  sbp_df = anthro_followup %>% 
    dplyr::filter(!is.na(SYSTOLIC)) %>% 
    mutate(t = as.numeric(t)) %>%
    rename(w = w_cols[i]) %>% 
    arrange(ID,t) %>% 
    mutate(ID = factor(ID))
  
  ldl_df = lab_followup %>% 
    dplyr::filter(!is.na(ldl)) %>% 
    mutate(t = as.numeric(t)) %>%
    rename(w = w_cols[i]) %>% 
    arrange(ID,t) %>% 
    mutate(ID = factor(ID))
  
  bmi_fit[[i]] <- geeglm(formula = bmi ~ COHORT*t,data = bmi_df,weights=w,id = ID,corstr="exchangeable")
  print("BMI completed")
  sbp_fit[[i]] <- geeglm(formula = SYSTOLIC ~ COHORT*t,data = sbp_df,weights=w,id = ID,corstr="exchangeable")
  print("SBP completed")
  ldl_fit[[i]] <- geeglm(formula = ldl ~ COHORT*t,data = ldl_df,weights=w,id = ID,corstr="exchangeable")
  print("LDL completed")

  
  
}

source("C:/code/external/functions/imputation/clean_mi_conditionalregression.R")

bmi_fit_out = clean_mi_conditionalregression(bmi_fit,link="geeglm identity")
sbp_fit_out = clean_mi_conditionalregression(sbp_fit,link="geeglm identity")
ldl_fit_out = clean_mi_conditionalregression(ldl_fit,link="geeglm identity")

bind_rows(
  bmi_fit_out %>% mutate(model = "PCRA201",outcome = "BMI"),
  sbp_fit_out %>% mutate(model = "PCRA201",outcome = "SBP"),
  ldl_fit_out %>% mutate(model = "PCRA201",outcome = "LDL")
) %>% 
  write_csv(.,"analysis/pcra201_change in cardiometabolic indicators.csv")


library(sjPlot)

bmi_plot <- plot_model(bmi_fit[[2]],type="pred",terms=c("t","COHORT"),
                       # ci.lvl = NA,
                       vcov.fun = vcov()) +
  scale_color_discrete(name="",labels=c("Unexposed","Historical","Exposed"),type = c("red","blue","darkgreen")) +
  xlab("Time since origin date (30 days after index date)") +
  ylab("Body mass index (kg/m2)") +
  ggtitle("") +
  theme_bw() +
  scale_y_continuous(limits=c(28,30),breaks=seq(28,30,by=0.5))

bmi_plot



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
