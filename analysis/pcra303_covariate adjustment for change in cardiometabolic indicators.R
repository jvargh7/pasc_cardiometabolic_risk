rm(list=ls());gc();source(".Rprofile")
source("analysis/pcra_analytic dataset for data availability.R")
source("analysis/pcra_ipw formula for main analysis.R")

tx_weights_df <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/ip weights for COHORT.RDS"))
mid_weights_df <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/ip weights for missing index date.RDS"))
bmi_weights_df <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/ip weights for missing bmi.RDS"))
sbp_weights_df <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/ip weights for missing sbp.RDS"))
ldl_weights_df <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/ip weights for missing ldl.RDS"))


w_cols = colnames(tx_weights_df)[str_detect(colnames(tx_weights_df),"W[0-9]+")]

library(mice)
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
                                          comorbidity_covariates,medication_covariates,labs_covariates,lb_hc_covariates) %>% 
                           as.formula(),
                         data = bmi_df,weights=w,id = ID,corstr="exchangeable")
  print("BMI completed")
  sbp_fit[[i]] <- geeglm(formula = paste0("SYSTOLIC ~ COHORT*t+ SYSTOLIC_historical",
                                          demo_covariates,index_date_covariates,
                                          comorbidity_covariates,medication_covariates,labs_covariates,lb_hc_covariates) %>% 
                           as.formula(),
                         data = sbp_df,weights=w,id = ID,corstr="exchangeable")
  print("SBP completed")
  ldl_fit[[i]] <- geeglm(formula = paste0("ldl ~ COHORT*t + ldl_historical",
                                          demo_covariates,index_date_covariates,
                                          comorbidity_covariates,medication_covariates,labs_covariates,lb_hc_covariates)%>% 
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


# Save ----------
source("C:/code/external/functions/imputation/save_mi_geeglm.R")
save_mi_geeglm(bmi_fit) %>% 
  saveRDS(paste0(path_pasc_cmr_folder,"/working/pcra303 bmi_fit.RDS"))
save_mi_geeglm(sbp_fit) %>% 
  saveRDS(paste0(path_pasc_cmr_folder,"/working/pcra303 sbp_fit.RDS"))
save_mi_geeglm(ldl_fit) %>% 
  saveRDS(paste0(path_pasc_cmr_folder,"/working/pcra303 ldl_fit.RDS"))

# Marginal estimates ----------
source("C:/code/external/functions/imputation/clean_mi_marginalprediction.R")

bmi_COHORT_margin = sbp_COHORT_margin = ldl_COHORT_margin = list()
for (margin_value in c("exposed","unexposed","historical")){
  
  bmi_COHORT_margin[[margin_value]] = clean_mi_marginalprediction(bmi_fit,margin_var = "COHORT",
                                                                margin_value = margin_value,link="geeglm identity",
                                                                modifier_var = "t",modifier_value = 0)
  sbp_COHORT_margin[[margin_value]] = clean_mi_marginalprediction(sbp_fit,margin_var = "COHORT",
                                                                margin_value = margin_value,link="geeglm identity",
                                                                modifier_var = "t",modifier_value = 0)
  ldl_COHORT_margin[[margin_value]] = clean_mi_marginalprediction(ldl_fit,margin_var = "COHORT",
                                                                margin_value = margin_value,link="geeglm identity",
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
  mutate(coef_ci = paste0(round(theta_D,1),
                          " (",round(L,1), ",",
                          round(U,1),")")) %>% 
  write_csv(.,"analysis/pcra303_COHORT margin at time 0.csv")

# Time contrasts -----------
bmi_t100_margin = sbp_t100_margin = ldl_t100_margin = list()

for (margin_value in c("exposed","unexposed","historical")){


  bmi_t100_margin[[margin_value]] = clean_mi_contrasts(model_list = bmi_fit,model_matrix = NULL,
                                          link="geeglm identity",modifier = paste0("COHORT",margin_value),exposure = "t",vcov_type = "robust",
                                          exposure_value = 100, modifier_value = 1,e_m_term = FALSE) %>% 
    mutate(margin_value = margin_value)
  
  sbp_t100_margin[[margin_value]] = clean_mi_contrasts(model_list = sbp_fit,model_matrix = NULL,
                                                       link="geeglm identity",modifier = paste0("COHORT",margin_value),exposure = "t",vcov_type = "robust",
                                                       exposure_value = 100, modifier_value = 1,e_m_term = FALSE) %>% 
    mutate(margin_value = margin_value)
  
  ldl_t100_margin[[margin_value]] = clean_mi_contrasts(model_list = ldl_fit,model_matrix = NULL,
                                                       link="geeglm identity",modifier = paste0("COHORT",margin_value),exposure = "t",vcov_type = "robust",
                                                       exposure_value = 100, modifier_value = 1,e_m_term = FALSE) %>% 
    mutate(margin_value = margin_value)

}

bind_rows(
  bind_rows(bmi_t100_margin)  %>%  mutate(outcome = "bmi"),
  bind_rows(sbp_t100_margin) %>% mutate(outcome = "sbp"),
  bind_rows(ldl_t100_margin) %>% mutate(outcome = "ldl") 
) %>% 
  write_csv(.,"analysis/pcra303_t margin at time 100.csv")

# Coefficient plot ------------
library(ggpubr)

contrasts_df <- read_csv("analysis/pcra303_contrasts of change in cardiometabolic indicators.csv")
coefs_df <- read_csv("analysis/pcra303_change in cardiometabolic indicators.csv")
source("functions/intercept_slope_plot.R")

bmi_coef_plot <- intercept_slope_plot(
  contrasts_df %>% dplyr::filter(outcome == "BMI"),
  coefs_df %>% dplyr::filter(outcome == "BMI")
) + 
  scale_color_discrete(name="",labels=c("Exposed","Unexposed","Historical"),type = c("red","darkgreen","blue")) +
  theme_bw() +
  xlab("Body mass index (kg/m2)") +
  ylab("") +
  geom_vline(xintercept = 0,linetype=2,col="grey20") +
  theme(legend.text = element_text(size = 12),
        axis.text = element_text(size = 12))

sbp_coef_plot <- intercept_slope_plot(
  contrasts_df %>% dplyr::filter(outcome == "SBP"),
  coefs_df %>% dplyr::filter(outcome == "SBP")
) + 
  scale_color_discrete(name="",labels=c("Exposed","Unexposed","Historical"),type = c("red","darkgreen","blue")) +
  theme_bw() +
  xlab("Systolic Blood Pressure (mm Hg)") +
  ylab("") +
  geom_vline(xintercept = 0,linetype=2,col="grey20")  +
  theme(legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_blank())

ldl_coef_plot <- intercept_slope_plot(
  contrasts_df %>% dplyr::filter(outcome == "LDL"),
  coefs_df %>% dplyr::filter(outcome == "LDL")
) + 
  scale_color_discrete(name="",labels=c("Exposed","Unexposed","Historical"),type = c("red","darkgreen","blue")) +
  theme_bw() +
  xlab("Low Density Lipoprotein (mg/dL)") +
  ylab("") +
  geom_vline(xintercept = 0,linetype=2,col="grey20")  +
  theme(legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_blank())

ggarrange(bmi_coef_plot,
          sbp_coef_plot,
          ldl_coef_plot,
          # ggplot() + theme_blank(),
          labels=c("B","C","D"),
          nrow=1,ncol=3,widths = c(1.8,1,1),common.legend = TRUE) %>% 
  ggsave(.,filename=paste0(path_pasc_cmr_folder,"/figures/marginal plot coefs for change.png"),width=10,height=4)

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


test <- readRDS(paste0(path_pasc_cmr_folder,"/working/pcra303 bmi_fit.RDS"))
