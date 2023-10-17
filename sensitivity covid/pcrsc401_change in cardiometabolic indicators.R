rm(list=ls());gc();source(".Rprofile")
source("analysis bmi/pcrab003_analytic dataset for data availability.R")
# source("analysis bmi/pcrabaux_ipw formula and variables.R")

# imbalanced_variables come from pcra_analytic dataset for change in cardiometabolic indicators.R 
source("sensitivity covid/pcrsc302_analytic dataset with ip weights for bmi.R")

library(geepack)

bmi_fit <- geeglm(formula = as.formula(paste0("bmi ~ COHORT*t + lb_bmi +",
                                              paste0(imbalanced_variables,collapse=" + "),
                                              "+",
                                              paste0(covid_variables,collapse=" + ")
                                              )),
                  data = bmi_df,weights=w,id = ID,corstr="exchangeable")
print("BMI completed")

# Save -------

source("C:/code/external/functions/imputation/save_mi_geeglm.R")
save_geeglm(bmi_fit) %>% 
  saveRDS(paste0(path_pasc_cmr_folder,"/working/sensitivity covid/pcrsc401 bmi_fit.RDS"))


# Coefficients -----

bind_rows(
  broom::tidy(bmi_fit) %>% mutate(model = "PCRAB401",outcome = "BMI")
) %>% 
  write_csv(.,"sensitivity covid/pcrsc401_coefficients change in bmi.csv")





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


bind_rows(
  bmi_plot$data %>% data.frame() %>% mutate(outcome = "bmi",modifier = "")) %>% 
  write_csv(.,file="sensitivity covid/pcrsc401_marginal predictions at t by COHORT.csv")


# library(ggpubr)
# bmi_plot %>% 
#   ggsave(.,filename=paste0(path_pasc_cmr_folder,"/figures/marginal plot for change in bmi.png"),width=6,height=4)



# Contrasts ------
source("C:/code/external/functions/imputation/contrasts_geeglm.R")

difference_grid = expand.grid(cohort = c("COHORThistorical","COHORTexposed","COHORTunexposed"),
                              modifier = c(0,100))


pcra401_contrast_fit <- function(fit,x,y){
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
                            
                            pcra401_contrast_fit(bmi_fit,x,y)
                            
                          })

  
  bind_rows(bmi_contrast %>% mutate(outcome = "bmi")) %>% 
    write_csv(.,"sensitivity covid/pcrsc401_contrasts of change in bmi.csv")





