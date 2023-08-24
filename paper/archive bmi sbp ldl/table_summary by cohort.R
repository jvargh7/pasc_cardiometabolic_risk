rm(list=ls());gc();source(".Rprofile")

# Instead of taking lookback_df directly, use this
# This does a sensible imputation of 0 for hospitalizations, diagnosis codes, medication and lookback encounter counts
source("analysis/pcra_processing before imputation of lookback covariates.R")

library(gtsummary)
# Unweighted -----------
(unweighted <- lookback_df %>% 
  tbl_summary(by = COHORT,
              include=c(female,age,
                        nhwhite,nhblack,hispanic, nhother,
                        smoking, 
                        site,
                        payer_type_primary,payer_type_secondary,
                        hospitalization, 
                        p_hyperglycemia, 
                        bmi, HT, SYSTOLIC, 
                        obesity, cardiovascular, cerebrovascular, hypertension,
                        pulmonary, hyperlipidemia, antidepressants, antipsychotics,
                        antihypertensives, statins, immunosuppresants, 
                        hba1c, glucose, alt, ast, 
                        serum_creatinine, hdl, ldl),
              missing = "ifany",
              missing_text = "Missing",
              value = list(p_hyperglycemia = 1),
              type = list(female ~ "dichotomous",
                          age ~ "continuous",
                          
                          nhwhite ~ "dichotomous",
                          nhblack ~ "dichotomous",
                          hispanic ~ "dichotomous",
                          nhother ~ "dichotomous",
                          
                          smoking ~ "dichotomous",
                          
                          site ~ "categorical",
                          
                       payer_type_primary ~ "categorical",
                       
                       payer_type_secondary ~ "categorical",
                       hospitalization ~ "dichotomous",
                       
                       p_hyperglycemia ~ "dichotomous",
                       HT ~ "continuous",
                       bmi ~ "continuous",
                       SYSTOLIC ~ "continuous",
                       antidepressants ~ "dichotomous",
                       antipsychotics ~ "dichotomous",
                       antihypertensives ~ "dichotomous",
                       statins ~ "dichotomous",
                       immunosuppresants ~ "dichotomous",
                       
                       obesity ~ "dichotomous",
                       cardiovascular ~ "dichotomous",
                       cerebrovascular ~ "dichotomous",
                       hypertension ~ "dichotomous",
                       pulmonary ~ "dichotomous",
                       hyperlipidemia ~ "dichotomous",
                       
                       hba1c ~ "continuous",
                       glucose ~ "continuous",
                       alt ~ "continuous2",
                       ast ~ "continuous2",
                       serum_creatinine ~ "continuous2",
                       hdl ~ "continuous",
                       ldl ~ "continuous"
                       ),
              digits = list(age ~ c(1,1),
                            nhwhite ~ c(0,1),
                            nhblack ~ c(0,1),
                            hispanic ~ c(0,1),
                            HT ~ c(1,1),
                            bmi ~ c(1,1),
                            SYSTOLIC  ~ c(1,1),
                            hba1c ~ c(1,1),
                            glucose ~ c(1,1),
                            alt ~ c(1,1,1,1,1),
                            ast ~ c(1,1,1,1,1),
                            serum_creatinine ~ c(1,1,1,1,1),
                            hdl ~ c(1,1),
                            ldl ~ c(1,1)
                            ),
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_continuous2() ~ c("{median} ({p25}, {p75})", "{min}, {max}"))) %>% 
  add_n() %>% 
  add_overall()) %>%
  as_gt() %>%
  gt::gtsave(filename = "paper/table_unweighted summary by cohort.docx")

# Weighted ------
library(survey)
predicted_probability <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/ip weights for COHORT.RDS"))

lookback_svy <-  lookback_df %>% 
  left_join(predicted_probability,
            by=c("ID","COHORT")) %>% 
  svydesign(data=.,~1,weights=~sipw)

(weighted <- lookback_svy %>% 
  tbl_svysummary(by = COHORT,
              include=c(female,age,
                        nhwhite,nhblack,hispanic, nhother,
                        smoking, 
                        site,
                        payer_type_primary,payer_type_secondary,
                        hospitalization, 
                        p_hyperglycemia, 
                        bmi, HT, SYSTOLIC, 
                        obesity, cardiovascular, cerebrovascular, hypertension,
                        pulmonary, hyperlipidemia, antidepressants, antipsychotics,
                        antihypertensives, statins, immunosuppresants, 
                        hba1c, glucose, alt, ast, 
                        serum_creatinine, hdl, ldl),
              missing = "ifany",
              missing_text = "Missing",
              value = list(p_hyperglycemia = 1),
              type = list(female ~ "dichotomous",
                          age ~ "continuous",
                          
                          nhwhite ~ "dichotomous",
                          nhblack ~ "dichotomous",
                          hispanic ~ "dichotomous",
                          nhother ~ "dichotomous",
                          
                          smoking ~ "dichotomous",
                          
                          site ~ "categorical",
                          
                          payer_type_primary ~ "categorical",
                          
                          payer_type_secondary ~ "categorical",
                          hospitalization ~ "dichotomous",
                          
                          p_hyperglycemia ~ "dichotomous",
                          HT ~ "continuous",
                          bmi ~ "continuous",
                          SYSTOLIC ~ "continuous",
                          antidepressants ~ "dichotomous",
                          antipsychotics ~ "dichotomous",
                          antihypertensives ~ "dichotomous",
                          statins ~ "dichotomous",
                          immunosuppresants ~ "dichotomous",
                          
                          obesity ~ "dichotomous",
                          cardiovascular ~ "dichotomous",
                          cerebrovascular ~ "dichotomous",
                          hypertension ~ "dichotomous",
                          pulmonary ~ "dichotomous",
                          hyperlipidemia ~ "dichotomous",
                          
                          hba1c ~ "continuous",
                          glucose ~ "continuous",
                          alt ~ "continuous2",
                          ast ~ "continuous2",
                          serum_creatinine ~ "continuous2",
                          hdl ~ "continuous",
                          ldl ~ "continuous"
              ),
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_continuous2() ~ c("{median} ({p25}, {p75})", "{min}, {max}"))) %>% 
  add_overall()) %>%
  as_gt() %>%
  gt::gtsave(filename = "paper/table_weighted summary by cohort.docx")

psb_df = read_csv("analysis/pcra203_population standardized bias for main analysis.csv") %>% 
  dplyr::select(variable,level,treatment_level,psb) %>% 
  pivot_wider(names_from=treatment_level,values_from=psb) %>% 
  mutate_if(is.numeric,~round(.,3)) %>% 
  dplyr::select(variable,level,exposed,unexposed,historical)


write_csv(psb_df,
          "paper/table_weighted population standardized bias.csv")
