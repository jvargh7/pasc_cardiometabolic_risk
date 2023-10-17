# Contrasts relative to reference category (Historical, Ref EMM, t = 0) -----------
rm(list=ls());gc();source(".Rprofile")

bmi_fit_sex <- readRDS(paste0(path_pasc_cmr_folder,"/working/sensitivity mice/pcrsm402 bmi_fit_sex.RDS"))
bmi_fit_age <- readRDS(paste0(path_pasc_cmr_folder,"/working/sensitivity mice/pcrsm402 bmi_fit_age.RDS"))
bmi_fit_raceeth <- readRDS(paste0(path_pasc_cmr_folder,"/working/sensitivity mice/pcrsm402 bmi_fit_raceeth.RDS"))
bmi_fit_hospitalization <- readRDS(paste0(path_pasc_cmr_folder,"/working/sensitivity mice/pcrsm402 bmi_fit_hospitalization.RDS"))

source("C:/code/external/functions/imputation/contrasts_geeglm.R")
source("C:/code/external/functions/preprocessing/prepare_contrasts_3way.R")
source("C:/code/external/functions/preprocessing/prepare_contrasts.R")
source("analysis bmi/pcrabaux_difference grids.R")

source("C:/code/external/functions/imputation/clean_mi_contrasts.R")


pcrsm403_contrast_fit <- function(gee_saved_fit,x,y,z){
  # x --> exposure
  # y --> modifier1
  # z --> modifier2_value
  
  if(x %in% names(gee_saved_fit[[1]]$coefficients)){
    if(y %in% names(gee_saved_fit[[1]]$coefficients)){
    # Exposure = 1, Modifier 1 = 1, Modifier 2 = z vs Exposure = 1, Modifier 1 = 0, Modifier 2 = 0
      mm = prepare_contrasts_3way(nterms_het = length(gee_saved_fit[[1]]$coefficients),
                                  names_het = names(gee_saved_fit[[1]]$coefficients),
                                  cov_het = gee_saved_fit[[1]]$robust.cov,
                                  # Temporary ------
                                  names_cov_het = names(gee_saved_fit[[1]]$coefficients),
                                  exposure = x,
                                  modifier1 = y,
                                  modifier2 = "t",
                                  exposure_value = 1,
                                  modifier1_value = 1,
                                  modifier2_value = z,
                                  e_m1_m2_term = TRUE,
                                  e_m1_term = TRUE,
                                  e_m2_term = TRUE)
    } else{
      # Exposure = 1, Modifier 1 = 0
      mm = prepare_contrasts(nterms_het = length(gee_saved_fit[[1]]$coefficients),
                             names_het = names(gee_saved_fit[[1]]$coefficients),
                             cov_het = gee_saved_fit[[1]]$robust.cov,
                             # Temporary ------
                             names_cov_het = names(gee_saved_fit[[1]]$coefficients),
                             exposure = x,
                             modifier = "t",
                             exposure_value = 1,
                             modifier_value = z,
                             e_m_term = TRUE) %>% 
        .[[2]]
      
    }
  } else if(y %in% names(gee_saved_fit[[1]]$coefficients)){
    # Exposure = 0, Modifier 1 = 1
    mm = prepare_contrasts(nterms_het = length(gee_saved_fit[[1]]$coefficients),
                           names_het = names(gee_saved_fit[[1]]$coefficients),
                           cov_het = gee_saved_fit[[1]]$robust.cov,
                           # Temporary ------
                           names_cov_het = names(gee_saved_fit[[1]]$coefficients),
                           exposure = y,
                           modifier = "t",
                           exposure_value = 1,
                           modifier_value = z,
                           e_m_term = TRUE) %>% 
      .[[2]]
    
  } else {
    # Exposure = 0, Modifier 1 = 0
    mm = prepare_contrasts(nterms_het = length(gee_saved_fit[[1]]$coefficients),
                           names_het = names(gee_saved_fit[[1]]$coefficients),
                           cov_het = gee_saved_fit[[1]]$robust.cov,
                           # Temporary ------
                           names_cov_het = names(gee_saved_fit[[1]]$coefficients),
                           exposure = "t",
                           exposure_value = z,
                           e_m_term = TRUE) %>% 
      .[[2]]
    
  }

# MODIFIED from contrasts_geeglm to clean_mi_contrasts ----------
clean_mi_contrasts(gee_saved_fit,
                   link = "geeglm identity",
                   model_matrix = mm
                   )  %>% 
    dplyr::filter(iv %in% c("Contrast 2"))  %>% 
  return(.)
  
}


# Sex Contrasts -------------


contrast_sex <- map_dfr(1:nrow(difference_grid_sex),
                        function(i){
                          print(i)
                          x_name = difference_grid_sex$cohort[i]
                          y_name = difference_grid_sex$modifier1[i]
                          z_value = difference_grid_sex$modifier2_value[i]
                          bind_rows(
                            pcrsm403_contrast_fit(bmi_fit_sex,x_name,y_name,z_value) %>% 
                            mutate(exposure = x_name,
                                   modifier1 = y_name,
                                   modifier2 = "t",
                                   exposure_value = 1,
                                   modifier1_value = 1,
                                   modifier2_value = z_value,
                                   outcome = "bmi")) %>% 
                            return(.)
                          })

# Age Contrasts -------------


contrast_age <- map_dfr(1:nrow(difference_grid_age),
                        function(i){
                          print(i)
                          x_name = difference_grid_age$cohort[i]
                          y_name = difference_grid_age$modifier1[i]
                          z_value = difference_grid_age$modifier2_value[i]
                          bind_rows(
                            pcrsm403_contrast_fit(bmi_fit_age,x_name,y_name,z_value) %>% 
                              mutate(exposure = x_name,
                                     modifier1 = y_name,
                                     modifier2 = "t",
                                     exposure_value = 1,
                                     modifier1_value = 1,
                                     modifier2_value = z_value,
                                     outcome = "bmi")) %>% 
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
                            pcrsm403_contrast_fit(bmi_fit_raceeth,x_name,y_name,z_value) %>% 
                              mutate(exposure = x_name,
                                     modifier1 = y_name,
                                     modifier2 = "t",
                                     exposure_value = 1,
                                     modifier1_value = 1,
                                     modifier2_value = z_value,
                                     outcome = "bmi")) %>% 
                            return(.)
                        })

# Hospitalization Contrasts ---------
contrast_hospitalization <- map_dfr(1:nrow(difference_grid_hospitalization),
                            function(i){
                              print(i)
                              x_name = difference_grid_hospitalization$cohort[i]
                              y_name = difference_grid_hospitalization$modifier1[i]
                              z_value = difference_grid_hospitalization$modifier2_value[i]
                              bind_rows(
                                pcrsm403_contrast_fit(bmi_fit_hospitalization,x_name,y_name,z_value) %>% 
                                  mutate(exposure = x_name,
                                         modifier1 = y_name,
                                         modifier2 = "t",
                                         exposure_value = 1,
                                         modifier1_value = 1,
                                         modifier2_value = z_value,
                                         outcome = "bmi")) %>% 
                                return(.)
                            })

bind_rows(contrast_sex %>% mutate(modifier_var = "sex_category"),
          contrast_age %>% mutate(modifier_var = "age_category"),
          contrast_raceeth %>% mutate(modifier_var = "raceeth_category"),
          contrast_hospitalization %>% mutate(modifier_var = "hospitalization_category")) %>% 
  write_csv(.,"sensitivity mice/pcrsm403_contrasts of change in cardiometabolic indicators sociodemographic.csv")
