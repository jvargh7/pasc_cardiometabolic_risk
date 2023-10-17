rm(list=ls());gc();source(".Rprofile")
source("analysis bmi/pcrab003_analytic dataset for data availability.R")
# source("analysis bmi/pcrabaux_ipw formula and variables.R")

# imbalanced_variables come from pcra_analytic dataset for change in cardiometabolic indicators.R 
imbalanced_variables <- c("age","antihypertensives","nhblack","hispanic","smoking","site","hospitalization","hba1c","serum_creatinine","hdl","ldl")

library(mice)
mi_dfs <- readRDS(paste0(path_pasc_cmr_folder,"/working/sensitivity mice/pcrsm002_mi_dfs.RDS"))

library(geepack)

bmi_fit <- list()

for (i in 1:5){
  imputed_dataset = complete(mi_dfs,i)
  source("sensitivity mice/pcrsm302_analytic dataset with ip weights for bmi.R")
  bmi_fit[[i]] <- geeglm(formula = as.formula(paste0("bmi ~ COHORT*t + lb_bmi +",paste0(imbalanced_variables,collapse=" + "))),
                    data = bmi_df,weights=w,id = ID,corstr="exchangeable")
  rm(bmi_df)
  
}


print("BMI completed")

# Save -------

source("C:/code/external/functions/imputation/save_mi_geeglm.R")
save_mi_geeglm(bmi_fit) %>% 
  saveRDS(paste0(path_pasc_cmr_folder,"/working/sensitivity mice/pcrsm401 bmi_fit.RDS"))


# Coefficients -----
source("C:/code/external/functions/imputation/clean_mi_conditionalregression.R")

clean_mi_conditionalregression(bmi_fit,link = "geeglm identity") %>% 
  write_csv(.,"sensitivity mice/pcrsm401_coefficients change in bmi.csv")





# Model plot ---------------
library(sjPlot)
# Here terms indicates for which terms marginal effects should be displayed. 
# At least one term is required to calculate effects, maximum length is three terms, 
# where the second and third term indicate the groups, i.e. predictions of first term are grouped 
# by the levels of the second (and third) term. terms may also indicate higher order terms (e.g. interaction terms).
label_order <- c("Historical","Unexposed","Exposed")
color_order <- c("blue","darkgreen","red")

bmi_plot <- list()
for(i in 1:5){
  bmi_plot[[i]] <- plot_model(bmi_fit[[i]],type="pred",terms=c("t","COHORT"),
                         # ci.lvl = NA,
                         axis.lim = list(c(0,100),c(28,32)),
                         vcov.fun = vcov()) +
    scale_color_discrete(name="",labels= label_order,type = color_order) +
    scale_fill_discrete(name="",labels= label_order,type = color_order) +
    xlab("Time since origin date (30 days after index date)") +
    ylab("Body mass index (kg/m2)") +
    ggtitle("") +
    theme_bw() 
  
  
}

  # scale_y_continuous(limits=c(28,32),breaks=seq(28,32,by=1))

imap_dfr(bmi_plot,
        function(b_p,name){
          b_p$data %>% data.frame() %>% 
            mutate(outcome = "bmi",modifier = "",index = name)
          
        }) %>% 
  write_csv(.,file="sensitivity mice/pcrsm401_marginal predictions at t by COHORT.csv")


# Contrasts ------
source("C:/code/external/functions/imputation/clean_mi_contrasts.R")

difference_grid = expand.grid(cohort = c("COHORThistorical","COHORTexposed","COHORTunexposed"),
                              modifier = c(0,100))


pcra401_contrast_fit <- function(fit,x,y){
  print(x)
  if(x %in% names(fit[[1]]$coefficients)){
    
    df = clean_mi_contrasts(model_list = fit,
                    link = "geeglm identity",
                     exposure = x,
                     modifier = "t",
                     vcov_type = "robust",
                     exposure_value = 1,
                     modifier_value = y
    ) %>% 
      dplyr::filter(iv %in% c("Contrast 2")) %>% 
      mutate(exposure = x,
             modifier = "t",
             exposure_value = 1,
             modifier_value = y)
    print(df$B_D)
  } else{
    df = clean_mi_contrasts(model_list = fit,
                       link = "geeglm identity",
                     model_matrix = NULL,
                     exposure = "t",
                     vcov_type = "robust",
                     exposure_value = y
    ) %>% 
      dplyr::filter(iv == "Contrast 2") %>% 
      mutate(exposure = x,
             modifier = "t",
             exposure_value = 1,
             modifier_value = y)
    
  }
  
  return(df)
}


  bmi_contrast = map2_dfr(difference_grid$cohort,
                          difference_grid$modifier,
                          function(x,y){
                            
                            pcra401_contrast_fit(bmi_fit,x,y)
                            
                          })

  
  bind_rows(bmi_contrast %>% mutate(outcome = "bmi")) %>% 
    write_csv(.,"sensitivity mice/pcrsm401_contrasts of change in bmi.csv")





