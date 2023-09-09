rm(list=ls());gc();source(".Rprofile")
# library(glmmLasso)
# library(glmnet)
# library(PGEE)



source("analysis bmi/pcrab003_analytic dataset for data availability.R")
rm(anthro_followup,demographic,index_date,lab_followup); gc()


source("analysis bmi/pcrab102_selecting high dimensional variables based on fdr.R")

hd_dataset_COHORT <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre403_high dimensional dataset for analysis.RDS")) %>% 
  dplyr::select(ID,ends_with("gtOne"),ends_with("gtMedian"),ends_with("gtQ3")) %>% 
  rename_all(~str_replace(.,"\\-","_")) %>% 
  dplyr::select(-one_of(selected_hdvars %>% 
                          # dplyr::filter(outcome == "bmi") %>% 
                          dplyr::select(variable) %>% 
                          pull() %>% unique())) %>% 
  left_join(outcome_availability,
            by="ID") %>% 
  dplyr::filter(ID %in% lookback_df$ID)

var_names = hd_dataset_COHORT %>% 
  dplyr::select(-names(outcome_availability)) %>% 
  names(.)

gc()

t0 = Sys.time()
bmi_c0 <- glm(as.formula(paste0("in_bmi_followup_ID ~ 1")),data=hd_dataset_COHORT,family = binomial())
t1 = Sys.time()
t1 - t0

bmi0_dev = deviance(bmi_c0) %>% as.numeric()

lrt_est_censoring = map_dfr(var_names,
                            function(v_n){
                              # c[]: censoring, m[]: model 
                              bmi_c1 <- glm(as.formula(paste0("in_bmi_followup_ID ~ ",v_n)),data=hd_dataset_COHORT,family = binomial())

                              bmi1_dev = deviance(bmi_c1) %>% as.numeric()

                              lambda_bmi = bmi0_dev - bmi1_dev

                              rm(bmi_c1); gc();
                              
                              data.frame(variable = v_n,
                                         
                                         bmi1_dev = bmi1_dev,
                                         lambda_bmi = lambda_bmi,
                                         pval_bmi = (1-pchisq(lambda_bmi,1))
                              ) %>% 
                                return(.)
                              
                              
                            })
saveRDS(lrt_est_censoring,paste0(path_pasc_cmr_folder,"/working/models pcrab/pcrab201_additional high dimensional covariates with loss to followup.RDS"))
