rm(list=ls());gc();source(".Rprofile")
# library(glmmLasso)
# library(glmnet)
# library(PGEE)
library(geepack)
hd_dataset <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/high dimensional dataset for analysis.RDS")) %>% 
  dplyr::select(ID,ends_with("gtOne"),ends_with("gtMedian"),ends_with("gtQ3")) %>% 
  rename_all(~str_replace(.,"\\-","_"))

# anthro_followup <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/anthro followup.RDS"))
# lab_followup <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/lab_followup_wide.RDS"))

var_names = hd_dataset %>% 
  dplyr::select(-ID) %>% 
  names(.)

bmi_df = readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/anthro followup.RDS")) %>% 
  dplyr::filter(!is.na(bmi)) %>% 
  mutate(t = as.numeric(t)) %>% 
  arrange(ID,t) %>% 
  dplyr::select(ID,bmi,t) 

sbp_df = readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/anthro followup.RDS")) %>% 
  dplyr::filter(!is.na(SYSTOLIC)) %>% 
  mutate(t = as.numeric(t)) %>% 
  arrange(ID,t) %>% 
  dplyr::select(ID,SYSTOLIC,t)

ldl_df = readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/lab_followup_wide.RDS")) %>% 
  dplyr::filter(!is.na(ldl)) %>% 
  mutate(t = as.numeric(t)) %>% 
  arrange(ID,t) %>% 
  dplyr::select(ID,ldl,t)

# https://www.ssc.wisc.edu/sscc/pubs/MM/MM_TestEffects.html
# LRT of mixed models requires fit with MLE
# We fit with REML = FALSE because mixed models don't have good asymptotic distributions to test against.
# - When n is large and variables are independent, Wald test and LRT are equivalent
# - When there is some correlation among predictors Wald and LRT can vary
# - LRT is generally preferred over Wald tests of fixed effects in mixed models 
# - When there is little correlation between predictors, Wald is fine for linear mixed models

wald_est = map_dfr(var_names,
                         function(v_n){
                           b_df <- bmi_df %>% 
                             left_join(hd_dataset %>% dplyr::select(ID,one_of(v_n)),
                                       by = c("ID"))
                           s_df <- sbp_df %>% 
                             left_join(hd_dataset %>% dplyr::select(ID,one_of(v_n)),
                                       by = c("ID")) 
                           l_df <- ldl_df %>% 
                             left_join(hd_dataset %>% dplyr::select(ID,one_of(v_n)),
                                       by = c("ID"))
                           bmi_m1 <- geeglm(as.formula(paste0("bmi ~ t + ",v_n)),data=b_df,id=ID,corstr = "exchangeable")
                           sbp_m1 <- geeglm(as.formula(paste0("SYSTOLIC ~ t + ",v_n)),data=s_df,id=ID,corstr = "unstructured")
                           ldl_m1 <- geeglm(as.formula(paste0("ldl ~ t + ",v_n)),data=l_df,id=ID,corstr = "unstructured")
                           
                           bmi1_dev = deviance(bmi_m1) %>% as.numeric()
                           sbp1_dev = deviance(sbp_m1) %>% as.numeric()
                           ldl1_dev = deviance(ldl_m1) %>% as.numeric()
                           
                           lambda_bmi = bmi0_dev - bmi1_dev
                           lambda_sbp = sbp0_dev - sbp1_dev
                           lambda_ldl = ldl0_dev - ldl1_dev
                           
                           rm(bmi_m1,sbp_m1,ldl_m1,
                              b_df,s_df,l_df); gc();
                           
                           data.frame(variable = v_n,
                                      
                                      bmi1_dev = bmi1_dev,
                                      lambda_bmi = lambda_bmi,
                                      pval_bmi = (1-pchisq(lambda_bmi,1)),
                                      
                                      sbp1_dev = sbp1_dev,
                                      lambda_sbp = lambda_sbp,
                                      pval_sbp = (1-pchisq(lambda_sbp,1)),
                                      
                                      ldl1_dev = ldl1_dev,
                                      lambda_ldl = lambda_ldl,
                                      pval_ldl = (1-pchisq(lambda_ldl,1))
                           ) %>% 
                             return(.)
                           
                           
                         })
t1 = Sys.time()
t1 - t0

saveRDS(wald_est,paste0(path_pasc_cmr_folder,"/working/models/pcra101_high dimensional covariates with outcome.RDS"))





