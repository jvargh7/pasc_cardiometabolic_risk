rm(list=ls());gc();source(".Rprofile")
source("analysis/pcra_analytic dataset for data availability.R")
source("analysis/pcra_ipw formula for main analysis.R")

# imbalanced_variables come from pcra_analytic dataset for change in cardiometabolic indicators.R 
source("analysis/pcra_analytic dataset for change in cardiometabolic indicators.R")

library(geepack)

bmi_fit <- geeglm(formula = as.formula(paste0("bmi ~ COHORT*t + lb_bmi +",paste0(imbalanced_variables,collapse=" + "))),
                  data = bmi_df,weights=w,id = ID,corstr="exchangeable")
print("BMI completed")
sbp_fit <- geeglm(formula = as.formula(paste0("SYSTOLIC ~ COHORT*t + lb_SYSTOLIC +",paste0(imbalanced_variables,collapse=" + "))),
                  data = sbp_df,weights=w,id = ID,corstr="exchangeable")
print("SBP completed")
ldl_fit <- geeglm(formula = as.formula(paste0("ldl ~ COHORT*t + lb_ldl +",paste0(imbalanced_variables,collapse=" + "))),
                  data = ldl_df,weights=w,id = ID,corstr="exchangeable")
print("LDL completed")


# Save -------

source("C:/code/external/functions/imputation/save_mi_geeglm.R")
save_geeglm(bmi_fit) %>% 
  saveRDS(paste0(path_pasc_cmr_folder,"/working/models/pcra301 bmi_fit.RDS"))
save_geeglm(sbp_fit) %>% 
  saveRDS(paste0(path_pasc_cmr_folder,"/working/models/pcra301 sbp_fit.RDS"))
save_geeglm(ldl_fit) %>% 
  saveRDS(paste0(path_pasc_cmr_folder,"/working/models/pcra301 ldl_fit.RDS"))

# Coefficients -----

bind_rows(
  broom::tidy(bmi_fit) %>% mutate(model = "PCRA301",outcome = "BMI"),
  broom::tidy(sbp_fit) %>% mutate(model = "PCRA301",outcome = "SBP"),
  broom::tidy(ldl_fit) %>% mutate(model = "PCRA301",outcome = "LDL")
) %>% 
  write_csv(.,"analysis/pcra301_coefficients cardiometabolic indicators.csv")





# Model plot ---------------
library(sjPlot)
# Here terms indicates for which terms marginal effects should be displayed. 
# At least one term is required to calculate effects, maximum length is three terms, 
# where the second and third term indicate the groups, i.e. predictions of first term are grouped 
# by the levels of the second (and third) term. terms may also indicate higher order terms (e.g. interaction terms).
label_order <- c("Historical","Unexposed","Exposed")
color_order <- c("blue","darkgreen","red")

bmi_plot <- plot_model(bmi_fit,type="pred",terms=c("t","COHORT"),
                       # ci.lvl = NA,
                       axis.lim = list(c(0,100),c(28,32)),
                       vcov.fun = vcov()) +
  scale_color_discrete(name="",labels= label_order,type = color_order) +
  scale_fill_discrete(name="",labels= label_order,type = color_order) +
  xlab("Time since origin date (30 days after index date)") +
  ylab("Body mass index (kg/m2)") +
  ggtitle("") +
  theme_bw() 
  # scale_y_continuous(limits=c(28,32),breaks=seq(28,32,by=1))

sbp_plot <- plot_model(sbp_fit,type="pred",terms=c("t","COHORT"),
                       # ci.lvl = NA,
                       axis.lim = list(c(0,100),c(120,130)),
                       vcov.fun = vcov()) +
  scale_color_discrete(name="",labels= label_order,type = color_order) +
  scale_fill_discrete(name="",labels= label_order,type = color_order) +
  xlab("Time since origin date (30 days after index date)") +
  ylab("Systolic Blood Pressure (mm Hg)") +
  ggtitle("") +
  theme_bw() 

ldl_plot <- plot_model(ldl_fit,type="pred",terms=c("t","COHORT"),
                       # ci.lvl = NA,
                       axis.lim = list(c(0,100),c(0,120)),
                       vcov.fun = vcov()) +
  scale_color_discrete(name="",labels= label_order,type = color_order) +
  scale_fill_discrete(name="",labels= label_order,type = color_order) +
  xlab("Time since origin date (30 days after index date)") +
  ylab("Low Density Lipoprotein (mg/dL)") +
  ggtitle("") +
  theme_bw() 


bind_rows(
  bmi_plot$data %>% data.frame() %>% mutate(outcome = "bmi",modifier = ""),

  sbp_plot$data %>% data.frame() %>% mutate(outcome = "sbp",modifier = ""),

  ldl_plot$data %>% data.frame() %>% mutate(outcome = "ldl",modifier = ""),
) %>% 
  write_csv(.,file="analysis/pcra301_marginal predictions at t by COHORT.csv")


library(ggpubr)
ggarrange(bmi_plot,
          sbp_plot,
          ldl_plot,
          labels=c("","",""),
          nrow=3,ncol=1,common.legend = TRUE) %>% 
  ggsave(.,filename=paste0(path_pasc_cmr_folder,"/figures/marginal plot for change.png"),width=8,height=9)



# Contrasts ------
source("C:/code/external/functions/imputation/contrasts_geeglm.R")

difference_grid = expand.grid(cohort = c("COHORThistorical","COHORTexposed","COHORTunexposed"),
                              modifier = c(0,100))


pcra301_contrast_fit <- function(fit,x,y){
  if(x %in% names(fit$coefficients)){
    
    contrasts_geeglm(fit=fit,
                     model_matrix = NULL,
                     exposure = x,
                     modifier = "t",
                     vcov_type = "robust",
                     exposure_value = 1,
                     modifier_value = y
    ) %>% 
      dplyr::filter(term %in% c("Contrast 2")) %>% 
      mutate(exposure = x,
             modifier = "t",
             exposure_value = 1,
             modifier_value = y)
  } else{
    contrasts_geeglm(fit=fit,
                     model_matrix = NULL,
                     exposure = "t",
                     vcov_type = "robust",
                     exposure_value = y
    ) %>% 
      dplyr::filter(term == "Contrast 2") %>% 
      mutate(exposure = x,
             modifier = "t",
             exposure_value = 1,
             modifier_value = y)
    
  }
}


  bmi_contrast = map2_dfr(difference_grid$cohort,
                          difference_grid$modifier,
                          function(x,y){
                            
                            pcra301_contrast_fit(bmi_fit,x,y)
                            
                          })

  sbp_contrast = map2_dfr(difference_grid$cohort,
                          difference_grid$modifier,
                          function(x,y){
                            
                            pcra301_contrast_fit(sbp_fit,x,y)
                            
                          })
  ldl_contrast = map2_dfr(difference_grid$cohort,
                          difference_grid$modifier,
                          function(x,y){
                            
                            pcra301_contrast_fit(ldl_fit,x,y)
                            
                          })
  
  
  bind_rows(bmi_contrast %>% mutate(outcome = "bmi"),
            sbp_contrast %>% mutate(outcome = "sbp"),
            ldl_contrast %>% mutate(outcome = "ldl")) %>% 
    write_csv(.,"analysis/pcra301_contrasts of change in cardiometabolic indicators.csv")





