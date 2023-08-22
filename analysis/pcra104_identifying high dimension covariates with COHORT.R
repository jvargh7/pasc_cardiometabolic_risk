rm(list=ls());gc();source(".Rprofile")

hd_dataset <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/high dimensional dataset for analysis.RDS")) %>% 
  dplyr::select(ID,ends_with("gtOne"),ends_with("gtMedian"),ends_with("gtQ3")) %>% 
  rename_all(~str_replace(.,"\\-","_"))

var_names = hd_dataset %>% 
  dplyr::select(-ID) %>% 
  names(.)

index_date <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/index date.RDS")) %>% 
  mutate(exposed = case_when(COHORT == "exposed" ~ 1,
                             TRUE ~ 0),
         unexposed = case_when(COHORT == "unexposed" ~ 1,
                               TRUE ~ 0),
         historical = case_when(COHORT == "historical" ~ 1,
                                TRUE ~ 0))

exposed_m0 <- glm(as.formula(paste0("exposed ~ 1")),data=index_date,family=binomial())
unexposed_m0 <- glm(as.formula(paste0("unexposed ~ 1")),data=index_date,family=binomial())
historical_m0 <- glm(as.formula(paste0("historical ~ 1")),data=index_date,family=binomial())

e0_dev = deviance(exposed_m0)
u0_dev = deviance(unexposed_m0)
h0_dev = deviance(historical_m0)

library(furrr)
options(future.globals.maxSize= (12*1024*1024*1024)) #8GB
# https://stackoverflow.com/questions/40536067/how-to-adjust-future-global-maxsize
plan(multisession)
t0 = Sys.time()
lrt_est = future_map_dfr(var_names,
                         function(v_n){
                           model_df <- index_date %>% 
                             left_join(hd_dataset %>% dplyr::select(ID,one_of(v_n)),
                                       by = c("ID")) 
                           
                           
                           exposed_m1 <- glm(as.formula(paste0("exposed ~ ",v_n)),data=model_df,family=binomial())
                           unexposed_m1 <- glm(as.formula(paste0("unexposed ~ ",v_n)),data=model_df,family=binomial())
                           historical_m1 <- glm(as.formula(paste0("historical ~ ",v_n)),data=model_df,family=binomial())
                           
                           e1_dev = deviance(exposed_m1) %>% as.numeric()
                           u1_dev = deviance(unexposed_m1) %>% as.numeric()
                           h1_dev = deviance(historical_m1) %>% as.numeric()
                           
                           lambda_exposed = e0_dev - e1_dev
                           lambda_unexposed = u0_dev - u1_dev
                           lambda_historical = h0_dev - h1_dev
                           
                           rm(exposed_m1,unexposed_m1,historical_m1,
                              model_df); gc();
                           
                           data.frame(variable = v_n,
                                      
                                      e1_dev = e1_dev,
                                      lambda_exposed = lambda_exposed,
                                      pval_exposed = (1-pchisq(lambda_exposed,1)),
                                      
                                      u1_dev = u1_dev,
                                      lambda_unexposed = lambda_unexposed,
                                      pval_unexposed = (1-pchisq(lambda_unexposed,1)),
                                      
                                      h1_dev = h1_dev,
                                      lambda_historical = lambda_historical,
                                      pval_historical = (1-pchisq(lambda_historical,1))
                           ) %>% 
                             return(.)
                           
                           
                         })

t1 = Sys.time()
t1 - t0

saveRDS(lrt_est,paste0(path_pasc_cmr_folder,"/working/models/pcra104_high dimensional covariates with COHORT.RDS"))
