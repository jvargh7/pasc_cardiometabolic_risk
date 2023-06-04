rm(list=ls());gc();source(".Rprofile")

tx_weights_df <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/ip weights for COHORT.RDS"))
source("analysis/pcra_analytic dataset for change in indicators.R")

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
