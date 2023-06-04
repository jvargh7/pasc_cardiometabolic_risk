rm(list=ls());gc();source(".Rprofile")
source("analysis/pcra_analytic dataset for data availability.R")
source("analysis/pcra_ipw formula for main analysis.R")

tx_weights_df <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/ip weights for COHORT.RDS"))
mid_weights_df <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/ip weights for missing index date.RDS"))
bmi_weights_df <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/ip weights for missing bmi.RDS"))
sbp_weights_df <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/ip weights for missing sbp.RDS"))
ldl_weights_df <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/ip weights for missing ldl.RDS"))


w_cols = colnames(tx_weights_df)[str_detect(colnames(tx_weights_df),"W[0-9]+")]

library(geepack)
library(lme4)
bmi_fit = sbp_fit = ldl_fit = list()

for(i in 1:length(w_cols)){
  print(i)
  
  # https://stackoverflow.com/questions/27073002/geeglm-in-r-error-message-about-variable-lengths-differing
  # geeglm kept crashing because ID was a character variable and not a factor
  
  # BMI DF -----------
  bmi_df = anthro_followup %>% 
    dplyr::filter(!is.na(bmi)) %>% 
    left_join(tx_weights_df,
              by = "ID") %>% 
    left_join(mid_weights_df,
              by = "ID") %>% 
    left_join(bmi_weights_df,
              by = "ID") %>% 
    rename(tx_w = w_cols[i],
           mid_w = paste0("MID",i),
           mo_w = paste0("MO",i)
           ) %>% 
    mutate(t = as.numeric(t),
           w = tx_w*mid_w*mo_w) %>%
    arrange(ID,t)  %>% 
    dplyr::select(-HT) %>% 
    left_join(complete(mi_dfs,i) %>% 
                mutate(site = case_when(is.na(site) ~ "Source1",
                                        TRUE ~ site)) %>% 
                dplyr::select(-COHORT) %>% 
                rename(bmi_historical = bmi,
                       SYSTOLIC_historical = SYSTOLIC,
                       ldl_historical = ldl),
              by = "ID") %>% 
    mutate(ID = factor(ID)) 
  
  # SBP DF ------------
  sbp_df = anthro_followup %>% 
    dplyr::filter(!is.na(SYSTOLIC)) %>% 
    left_join(tx_weights_df,
              by = "ID") %>% 
    left_join(mid_weights_df,
              by = "ID") %>% 
    left_join(sbp_weights_df,
              by = "ID") %>% 
    rename(tx_w = w_cols[i],
           mid_w = paste0("MID",i),
           mo_w = paste0("MO",i)
    ) %>% 
    mutate(t = as.numeric(t),
           w = tx_w*mid_w*mo_w)%>% 
    arrange(ID,t)  %>% 
    dplyr::select(-HT) %>% 
    left_join(complete(mi_dfs,i) %>% 
                mutate(site = case_when(is.na(site) ~ "Source1",
                                        TRUE ~ site)) %>% 
                dplyr::select(-COHORT) %>% 
                rename(bmi_historical = bmi,
                       SYSTOLIC_historical = SYSTOLIC,
                       ldl_historical = ldl),
              by = "ID") %>% 
    mutate(ID = factor(ID)) 
  
  # LDL DF -----------
  ldl_df = lab_followup %>% 
    dplyr::filter(!is.na(ldl)) %>% 
    left_join(tx_weights_df,
              by = "ID") %>% 
    left_join(mid_weights_df,
              by = "ID") %>% 
    left_join(ldl_weights_df,
              by = "ID") %>% 
    rename(tx_w = w_cols[i],
           mid_w = paste0("MID",i),
           mo_w = paste0("MO",i)
    ) %>% 
    mutate(t = as.numeric(t),
           w = tx_w*mid_w*mo_w)  %>% 
    arrange(ID,t)  %>% 
    dplyr::select(-alt,-ast,-glucose,-hba1c,-hdl,-serum_creatinine) %>%
    left_join(complete(mi_dfs,i) %>% 
                mutate(site = case_when(is.na(site) ~ "Source1",
                                        TRUE ~ site)) %>% 
                dplyr::select(-COHORT) %>% 
                rename(bmi_historical = bmi,
                       SYSTOLIC_historical = SYSTOLIC,
                       ldl_historical = ldl),
              by = "ID") %>% 
    mutate(ID = factor(ID)) 
  
  # FITTING MODELS ---------
  bmi_fit[[i]] <- geeglm(formula = paste0("bmi ~ COHORT*t + bmi_historical",
                                          demo_covariates,index_date_covariates,
                                          comorbidity_covariates,medication_covariates,labs_covariates) %>% 
                           as.formula(),
                         data = bmi_df,weights=w,id = ID,corstr="exchangeable")
  print("BMI completed")
  sbp_fit[[i]] <- geeglm(formula = paste0("SYSTOLIC ~ COHORT*t+ SYSTOLIC_historical",
                                          demo_covariates,index_date_covariates,
                                          comorbidity_covariates,medication_covariates,labs_covariates) %>% 
                           as.formula(),
                         data = sbp_df,weights=w,id = ID,corstr="exchangeable")
  print("SBP completed")
  ldl_fit[[i]] <- geeglm(formula = paste0("ldl ~ COHORT*t + ldl_historical",
                                          demo_covariates,index_date_covariates,
                                          comorbidity_covariates,medication_covariates,labs_covariates)%>% 
                           as.formula(), 
                         data = ldl_df,weights=w,id = ID,corstr="exchangeable")
  print("LDL completed")
  
  
  
}

source("C:/code/external/functions/imputation/clean_mi_conditionalregression.R")

bmi_fit_out = clean_mi_conditionalregression(bmi_fit,link="geeglm identity")
sbp_fit_out = clean_mi_conditionalregression(sbp_fit,link="geeglm identity")
ldl_fit_out = clean_mi_conditionalregression(ldl_fit,link="geeglm identity")

bind_rows(
  bmi_fit_out %>% mutate(model = "PCRA203",outcome = "BMI"),
  sbp_fit_out %>% mutate(model = "PCRA203",outcome = "SBP"),
  ldl_fit_out %>% mutate(model = "PCRA203",outcome = "LDL")
) %>% 
  write_csv(.,"analysis/pcra203_change in cardiometabolic indicators.csv")


source("C:/code/external/functions/imputation/clean_mi_contrasts.R")
bmi_COHORThistorical = clean_mi_contrasts(model_list = bmi_fit,model_matrix = NULL,
                                              link="geeglm identity",exposure = "COHORThistorical",modifier = "t",vcov_type = "robust",
                                          exposure_value = 1, modifier_value = 100)

bmi_COHORTunexposed = clean_mi_contrasts(model_list = bmi_fit,model_matrix = NULL,
                                              link="geeglm identity",exposure = "COHORTunexposed",modifier = "t",vcov_type = "robust",
                                         exposure_value = 1, modifier_value = 100)

sbp_COHORThistorical = clean_mi_contrasts(model_list = sbp_fit,model_matrix = NULL,
                                          link="geeglm identity",exposure = "COHORThistorical",modifier = "t",vcov_type = "robust",
                                          exposure_value = 1, modifier_value = 100)

sbp_COHORTunexposed = clean_mi_contrasts(model_list = sbp_fit,model_matrix = NULL,
                                         link="geeglm identity",exposure = "COHORTunexposed",modifier = "t",vcov_type = "robust",
                                         exposure_value = 1, modifier_value = 100)

ldl_COHORThistorical = clean_mi_contrasts(model_list = ldl_fit,model_matrix = NULL,
                                          link="geeglm identity",exposure = "COHORThistorical",modifier = "t",vcov_type = "robust",
                                          exposure_value = 1, modifier_value = 100)

ldl_COHORTunexposed = clean_mi_contrasts(model_list = ldl_fit,model_matrix = NULL,
                                         link="geeglm identity",exposure = "COHORTunexposed",modifier = "t",vcov_type = "robust",
                                         exposure_value = 1, modifier_value = 100)


bind_rows(
  bmi_COHORThistorical %>% mutate(outcome = "BMI",exposure = "Historical",modifier = "'t'",modifier_value = 100),
  bmi_COHORTunexposed %>% mutate(outcome = "BMI",exposure = "Unexposed",modifier = "'t'",modifier_value = 100),
  
  sbp_COHORThistorical %>% mutate(outcome = "SBP",exposure = "Historical",modifier = "'t'",modifier_value = 100),
  sbp_COHORTunexposed %>% mutate(outcome = "SBP",exposure = "Unexposed",modifier = "'t'",modifier_value = 100),
  
  ldl_COHORThistorical %>% mutate(outcome = "LDL",exposure = "Historical",modifier = "'t'",modifier_value = 100),
  ldl_COHORTunexposed %>% mutate(outcome = "LDL",exposure = "Unexposed",modifier = "'t'",modifier_value = 100)
  
  
) %>% 
  write_csv(.,"analysis/pcra303_contrasts of change in cardiometabolic indicators.csv")


# Coefficient plot ------------
contrasts_df <- read_csv("analysis/pcra303_contrasts of change in cardiometabolic indicators.csv")
coefs_df <- read_csv("analysis/pcra203_change in cardiometabolic indicators.csv")
source("functions/intercept_slope_plot.R")

bmi_coef_plot <- intercept_slope_plot(
  contrasts_df %>% dplyr::filter(outcome == "BMI"),
  coefs_df %>% dplyr::filter(outcome == "BMI")
) + 
  scale_color_discrete(limits=rev,name="",labels=c("Unexposed","Historical","Exposed"),type = c("darkgreen","blue","red")) +
  theme_bw() +
  xlab("Body mass index (kg/m2)") +
  ylab("") +
  geom_vline(xintercept = 0,linetype=2,col="grey20")

sbp_coef_plot <- intercept_slope_plot(
  contrasts_df %>% dplyr::filter(outcome == "SBP"),
  coefs_df %>% dplyr::filter(outcome == "SBP")
) + 
  scale_color_discrete(limits=rev,name="",labels=c("Unexposed","Historical","Exposed"),type = c("darkgreen","blue","red")) +
  theme_bw() +
  xlab("Systolic Blood Pressure (mm Hg)") +
  ylab("") +
  geom_vline(xintercept = 0,linetype=2,col="grey20")

ldl_coef_plot <- intercept_slope_plot(
  contrasts_df %>% dplyr::filter(outcome == "LDL"),
  coefs_df %>% dplyr::filter(outcome == "LDL")
) + 
  scale_color_discrete(limits=rev,name="",labels=c("Unexposed","Historical","Exposed"),type = c("darkgreen","blue","red")) +
  theme_bw() +
  xlab("Low Density Lipoprotein (mg/dL)") +
  ylab("") +
  geom_vline(xintercept = 0,linetype=2,col="grey20")

# Model plot ---------------
library(sjPlot)

bmi_plot <- plot_model(bmi_fit[[3]],type="pred",terms=c("t","COHORT"),
                       # ci.lvl = NA,
                       vcov.fun = vcov()) +
  scale_color_discrete(name="",labels=c("Unexposed","Historical","Exposed"),type = c("darkgreen","blue","red")) +
  xlab("Time since origin date (30 days after index date)") +
  ylab("Body mass index (kg/m2)") +
  ggtitle("") +
  theme_bw() +
  scale_y_continuous(limits=c(28,32),breaks=seq(28,32,by=1))

sbp_plot <- plot_model(sbp_fit[[3]],type="pred",terms=c("t","COHORT"),
                       # ci.lvl = NA,
                       vcov.fun = vcov()) +
  scale_color_discrete(name="",labels=c("Unexposed","Historical","Exposed"),type = c("darkgreen","blue","red")) +
  xlab("Time since origin date (30 days after index date)") +
  ylab("Systolic Blood Pressure (mm Hg)") +
  ggtitle("") +
  theme_bw() 

ldl_plot <- plot_model(ldl_fit[[3]],type="pred",terms=c("t","COHORT"),
                       # ci.lvl = NA,
                       vcov.fun = vcov()) +
  scale_color_discrete(name="",labels=c("Unexposed","Historical","Exposed"),type = c("darkgreen","blue","red")) +
  xlab("Time since origin date (30 days after index date)") +
  ylab("Low Density Lipoprotein (mg/dL)") +
  ggtitle("") +
  theme_bw() 

library(ggpubr)
ggarrange(bmi_plot,
          sbp_plot,
          ldl_plot,
          labels=c("","",""),
          nrow=3,ncol=1,common.legend = TRUE) %>% 
  ggsave(.,filename=paste0(path_pasc_cmr_folder,"/figures/marginal plot covariate adjustment for change.png"),width=8,height=9)

ggarrange(bmi_coef_plot,
          bmi_plot,
          sbp_coef_plot,
          sbp_plot,
          ldl_coef_plot,
          ldl_plot,
          labels=LETTERS[1:6],
          nrow=3,ncol=2,common.legend = TRUE) %>% 
  ggsave(.,filename=paste0(path_pasc_cmr_folder,"/figures/marginal plot and coefs covariate adjustment for change.png"),width=12,height=8)
