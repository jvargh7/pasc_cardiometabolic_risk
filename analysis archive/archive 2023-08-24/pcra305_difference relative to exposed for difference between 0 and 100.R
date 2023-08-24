# Contrasts relative to within group reference (within exposure+socio-dem, t = 0) -----------
rm(list=ls());gc();source(".Rprofile")
bmi_fit_overall <- readRDS(paste0(path_pasc_cmr_folder,"/working/models/pcra301 bmi_fit.RDS"))
sbp_fit_overall <- readRDS(paste0(path_pasc_cmr_folder,"/working/models/pcra301 sbp_fit.RDS"))
ldl_fit_overall <- readRDS(paste0(path_pasc_cmr_folder,"/working/models/pcra301 ldl_fit.RDS"))

bmi_fit_sex <- readRDS(paste0(path_pasc_cmr_folder,"/working/models/pcra302 bmi_fit_sex.RDS"))
sbp_fit_sex <- readRDS(paste0(path_pasc_cmr_folder,"/working/models/pcra302 sbp_fit_sex.RDS"))
ldl_fit_sex <- readRDS(paste0(path_pasc_cmr_folder,"/working/models/pcra302 ldl_fit_sex.RDS"))
bmi_fit_age <- readRDS(paste0(path_pasc_cmr_folder,"/working/models/pcra302 bmi_fit_age.RDS"))
sbp_fit_age <- readRDS(paste0(path_pasc_cmr_folder,"/working/models/pcra302 sbp_fit_age.RDS"))
ldl_fit_age <- readRDS(paste0(path_pasc_cmr_folder,"/working/models/pcra302 ldl_fit_age.RDS"))
bmi_fit_raceeth <- readRDS(paste0(path_pasc_cmr_folder,"/working/models/pcra302 bmi_fit_raceeth.RDS"))
sbp_fit_raceeth <- readRDS(paste0(path_pasc_cmr_folder,"/working/models/pcra302 sbp_fit_raceeth.RDS"))
ldl_fit_raceeth <- readRDS(paste0(path_pasc_cmr_folder,"/working/models/pcra302 ldl_fit_raceeth.RDS"))

source("analysis/pcra_difference grids.R")
source("C:/code/external/functions/imputation/contrasts_geeglm.R")


x = "COHORTexposed"
y = "sex_categoryMale"
y = "sex_categoryFemale"
z = 100
pcra305_contrast_fit <- function(gee_saved_fit,x,y = NULL,z){
  # x --> exposure
  # y --> modifier1
  # z --> modifier2_value
  
  names_het = names(gee_saved_fit$coefficients)
  
  mm = matrix(c(
    rep(c(0),each=length(names_het)),
    rep(c(0),each=length(names_het)),
    rep(c(0),each=length(names_het))),
    nrow=3,
    byrow=TRUE
  )
  

  
  x_ref = "COHORTexposed"
  
  if(!is.null(y)){
    
    # b0 + b1 COHORTexposed + b2 sexMale + b3 t + b4 COHORTexposed.sexMale + b5 COHORTexposed.t + b6 sexMale.t + b7 COHORTexposed.sexMale.t
    # Assume x_ref = "COHORTexposed"
    # Change from t = 0 to t = 100 for x_ref --> similar to pcra305
    mm[1,which(names_het %in% "t")] <- z # b3
    mm[1,which(names_het %in% paste0(x_ref,":t"))] <- z*1 #b5
    mm[1,which(names_het %in% paste0(y,":t"))] <- z*1 #b6
    mm[1,which(names_het %in% paste0(x_ref,":",y,":t"))] <- z*1*1 #b7
    
    # Change from t = 0 to t = 100 for x
    mm[2,which(names_het %in% "t")] <- z # b3
    mm[2,which(names_het %in% paste0(x,":t"))] <- z*1 #b5'
    mm[2,which(names_het %in% paste0(y,":t"))] <- z*1 #b6
    mm[2,which(names_het %in% paste0(x,":",y,":t"))] <- z*1*1 #b7'
    
    # Difference between the above 2 (x - x_ref)
    mm[3,which(names_het %in% paste0(x_ref,":t"))] <- z*-1 #b5
    mm[3,which(names_het %in% paste0(x,":t"))] <- z*1 #b5'
    mm[3,which(names_het %in% paste0(x_ref,":",y,":t"))] <- z*-1*1 #b7
    mm[3,which(names_het %in% paste0(x,":",y,":t"))] <- z*1*1 #b7'
    
    
  } else{
    # Change from t = 0 to t = 100 for x_ref
    mm[1,which(names_het %in% "t")] <- z
    mm[1,which(names_het %in% paste0(x_ref,":t"))] <- z*1
    
    # Change from t = 0 to t = 100 for x
    mm[2,which(names_het %in% "t")] <- z
    mm[2,which(names_het %in% paste0(x,":t"))] <- z*1
    
    # Difference between the above 2 (x - x_ref)
    mm[3,which(names_het %in% paste0(x_ref,":t"))] <- z*-1
    mm[3,which(names_het %in% paste0(x,":t"))] <- z*1
    
  }
  
  if(x_ref == x){
    
    mm[3,] <- 0
  }
  
  contrasts_geeglm(
    model_matrix = mm,
    vcov_gee = gee_saved_fit$robust.cov,
    coef_gee = gee_saved_fit$coefficients,
    dfcom_gee = gee_saved_fit$df.residual
  )   %>% 
    return(.)
  
}

# Overall Contrasts -------------
contrast_overall <- map_dfr(1:nrow(difference_grid_overall),
                            function(i){
                              print(i)
                              x_name = difference_grid_overall$cohort[i]
                              # y_name = difference_grid_overall$modifier1[i]
                              z_value = difference_grid_overall$modifier2_value[i]
                              bind_rows(
                                pcra305_contrast_fit(bmi_fit_overall,x=x_name,z=z_value) %>% 
                                  mutate(exposure = x_name,
                                         modifier2 = "t",
                                         exposure_value = 1,
                                         modifier2_value = z_value,
                                         outcome = "bmi"),
                                pcra305_contrast_fit(sbp_fit_overall,x_name,z=z_value) %>% 
                                  mutate(exposure = x_name,
                                         modifier2 = "t",
                                         exposure_value = 1,
                                         modifier2_value = z_value,
                                         outcome = "sbp"),
                                pcra305_contrast_fit(ldl_fit_overall,x_name,z=z_value) %>% 
                                  mutate(exposure = x_name,
                                         modifier2 = "t",
                                         exposure_value = 1,
                                         modifier2_value = z_value,
                                         outcome = "ldl")) %>% 
                                return(.)
                            })




# Sex Contrasts -------------
contrast_sex <- map_dfr(1:nrow(difference_grid_sex),
                        function(i){
                          print(i)
                          x_name = difference_grid_sex$cohort[i]
                          y_name = difference_grid_sex$modifier1[i]
                          z_value = difference_grid_sex$modifier2_value[i]
                          bind_rows(
                            pcra305_contrast_fit(bmi_fit_sex,x_name,y_name,z_value) %>% 
                              mutate(exposure = x_name,
                                     modifier1 = y_name,
                                     modifier2 = "t",
                                     exposure_value = 1,
                                     modifier1_value = 1,
                                     modifier2_value = z_value,
                                     outcome = "bmi"),
                            pcra305_contrast_fit(sbp_fit_sex,x_name,y_name,z_value) %>% 
                              mutate(exposure = x_name,
                                     modifier1 = y_name,
                                     modifier2 = "t",
                                     exposure_value = 1,
                                     modifier1_value = 1,
                                     modifier2_value = z_value,
                                     outcome = "sbp"),
                            pcra305_contrast_fit(ldl_fit_sex,x_name,y_name,z_value) %>% 
                              mutate(exposure = x_name,
                                     modifier1 = y_name,
                                     modifier2 = "t",
                                     exposure_value = 1,
                                     modifier1_value = 1,
                                     modifier2_value = z_value,
                                     outcome = "ldl")) %>% 
                            return(.)
                        })

# Age contrasts ----------
contrast_age <- map_dfr(1:nrow(difference_grid_age),
                        function(i){
                          print(i)
                          x_name = difference_grid_age$cohort[i]
                          y_name = difference_grid_age$modifier1[i]
                          z_value = difference_grid_age$modifier2_value[i]
                          bind_rows(
                            pcra305_contrast_fit(bmi_fit_age,x_name,y_name,z_value) %>% 
                              mutate(exposure = x_name,
                                     modifier1 = y_name,
                                     modifier2 = "t",
                                     exposure_value = 1,
                                     modifier1_value = 1,
                                     modifier2_value = z_value,
                                     outcome = "bmi"),
                            pcra305_contrast_fit(sbp_fit_age,x_name,y_name,z_value) %>% 
                              mutate(exposure = x_name,
                                     modifier1 = y_name,
                                     modifier2 = "t",
                                     exposure_value = 1,
                                     modifier1_value = 1,
                                     modifier2_value = z_value,
                                     outcome = "sbp"),
                            pcra305_contrast_fit(ldl_fit_age,x_name,y_name,z_value) %>% 
                              mutate(exposure = x_name,
                                     modifier1 = y_name,
                                     modifier2 = "t",
                                     exposure_value = 1,
                                     modifier1_value = 1,
                                     modifier2_value = z_value,
                                     outcome = "ldl")) %>% 
                            return(.)
                        })

# Race-Ethnicity Contrasts -------------


contrast_raceeth <- map_dfr(1:nrow(difference_grid_raceeth),
                            function(i){
                              print(i)
                              x_name = difference_grid_raceeth$cohort[i]
                              y_name = difference_grid_raceeth$modifier1[i]
                              z_value = difference_grid_raceeth$modifier2_value[i]
                              bind_rows(
                                pcra305_contrast_fit(bmi_fit_raceeth,x_name,y_name,z_value) %>% 
                                  mutate(exposure = x_name,
                                         modifier1 = y_name,
                                         modifier2 = "t",
                                         exposure_value = 1,
                                         modifier1_value = 1,
                                         modifier2_value = z_value,
                                         outcome = "bmi"),
                                pcra305_contrast_fit(sbp_fit_raceeth,x_name,y_name,z_value) %>% 
                                  mutate(exposure = x_name,
                                         modifier1 = y_name,
                                         modifier2 = "t",
                                         exposure_value = 1,
                                         modifier1_value = 1,
                                         modifier2_value = z_value,
                                         outcome = "sbp"),
                                pcra305_contrast_fit(ldl_fit_raceeth,x_name,y_name,z_value) %>% 
                                  mutate(exposure = x_name,
                                         modifier1 = y_name,
                                         modifier2 = "t",
                                         exposure_value = 1,
                                         modifier1_value = 1,
                                         modifier2_value = z_value,
                                         outcome = "ldl")) %>% 
                                return(.)
                            })



bind_rows(contrast_overall,
          contrast_sex %>% mutate(modifier_var = "sex_category"),
          contrast_age %>% mutate(modifier_var = "age_category"),
          contrast_raceeth %>% mutate(modifier_var = "raceeth_category")) %>% 
  dplyr::filter(term %in% c("Contrast 3")) %>% 
  write_csv(.,"analysis/pcra305_difference relative to exposed for difference between 0 and 100.csv")
