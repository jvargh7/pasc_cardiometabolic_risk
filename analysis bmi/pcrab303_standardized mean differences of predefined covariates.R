rm(list=ls());gc();source(".Rprofile")

lookback_processed <- readRDS(paste0(path_pasc_cmr_folder,"/working/models pcrab/pcrab002_imputed lookback dataset.RDS"))
predicted_probability <- readRDS(paste0(path_pasc_cmr_folder,"/working/models pcrab/pcrab301_ip weights for cohort membership.RDS")) %>% 
  dplyr::select(-hospitalization)

source("C:/code/external/functions/causality/population_standardized_bias.R")

p_vars = c("female","nhblack","nhother","hispanic",
           "hospitalization","smoking",
           
           "obesity","cardiovascular","cerebrovascular",
           "hypertension","pulmonary","hyperlipidemia",
           
           "antidepressants","antipsychotics","antihypertensives",
           "statins","immunosuppresants"
           )
c_vars = c("age","hba1c","glucose","alt",
           "HT","bmi","SYSTOLIC",
           "ast","serum_creatinine","hdl","ldl",
           
           "lb_hospitalized","lb_telehealth","lb_outpatient",
           "lb_n_glucose","lb_n_hdl","lb_n_ldl","lb_n_serum_creatinine",
           "lb_n_alt","lb_n_ast","lb_n_hba1c","lb_n_labvisits"
           )
g_vars = c("site","payer_type_primary2","payer_type_secondary")

df = lookback_processed %>% 
  mutate(payer_type_primary2 = case_when(payer_type_primary %in% c("Bluecross","Private or Other") ~ "Private",
                                         TRUE ~ payer_type_primary)) %>% 
  left_join(predicted_probability,
            by=c("ID","COHORT"))

p_psb = map_dfr(p_vars,
                function(p_v){
                  
                  population_standardized_bias(df=df,x_var=p_v,t_var="COHORT",ipw="sipw",type = "p") %>% 
                    return(.)
                  
                  
                  
                  
                })

c_psb = map_dfr(c_vars,
                function(c_v){
                  
                  population_standardized_bias(df=df,x_var=c_v,t_var="COHORT",ipw="sipw",type = "c") %>% 
                    return(.)
                })

g_psb = map_dfr(g_vars,
                function(g_v){
                  
                  population_standardized_bias(df=df,x_var=g_v,t_var="COHORT",ipw_var="sipw",type = "g") %>% 
                    return(.)
                })


bind_rows(p_psb,
          c_psb,
          g_psb) %>% 
  write_csv(.,file="analysis bmi/pcrab303_population standardized bias for main analysis.csv")

