rm(list=ls());gc();source(".Rprofile")

# Instead of taking lookback_df directly, use this
# This does a sensible imputation of 0 for hospitalizations, diagnosis codes, medication and lookback encounter counts
source("analysis/pcra_processing before imputation of lookback covariates.R")
source("analysis/pcra_analytic dataset for data availability.R")


encounter_followup <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/encounter_followup_long.RDS")) %>% 
  group_by(ID,ENC_TYPE) %>% 
  summarize(count = sum(n)) %>% 
  pivot_wider(names_from=ENC_TYPE,values_from=count,values_fill=0)


library(gtsummary)
# Unweighted -----------
(unweighted <- lookback_df %>% 
   left_join(encounter_followup,
             by = "ID") %>% 
   mutate(availability_category = case_when(ID %in% bmi_ID & ID %in% sbp_ID ~ 1,
                                            ID %in% bmi_ID & !ID %in% sbp_ID ~ 2,
                                            !ID %in% bmi_ID & ID %in% sbp_ID ~ 3,
                                            TRUE ~ 4)) %>% 
   mutate(availability_category = factor(availability_category,levels=c(1:4),
                                         labels=c("Both","BMI only","SBP only","Neither"))) %>% 
   tbl_summary(by = availability_category,
               include=c(COHORT, female,age,
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
                         serum_creatinine, hdl, ldl,
                         
                         IP,OA,OT,AV,NI,TH,ED,OS,EI,UN,IS,IC
                         ),
               missing = "ifany",
               missing_text = "Missing",
               value = list(p_hyperglycemia = 1),
               type = list(COHORT ~ "categorical",
                           female ~ "dichotomous",
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
                           ldl ~ "continuous",
                           
                           IP~ "continuous2",OA~ "continuous2",
                           OT~ "continuous2",AV~ "continuous2",NI~ "continuous2",
                           TH~ "continuous2",ED~ "continuous2",OS~ "continuous2",
                           EI~ "continuous2",UN~ "continuous2",IS~ "continuous2",IC~ "continuous2"
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
                             ldl ~ c(1,1),
                             
                             IP~ c(1,1,1,1,1),OA~ c(1,1,1,1,1),
                             OT~ c(1,1,1,1,1),AV~ c(1,1,1,1,1),NI~ c(1,1,1,1,1),
                             TH~ c(1,1,1,1,1),ED~ c(1,1,1,1,1),OS~ c(1,1,1,1,1),
                             EI~ c(1,1,1,1,1),UN~ c(1,1,1,1,1),IS~ c(1,1,1,1,1),IC~ c(1,1,1,1,1)
               ),
               statistic = list(all_continuous() ~ "{mean} ({sd})",
                                all_continuous2() ~ c("{median} ({p25}, {p75})", "{min}, {max}"))) %>% 
   add_n() %>% 
   add_overall()) %>%
  as_gt() %>%
  gt::gtsave(filename = "paper/table_unweighted summary by outcome availability category.docx")

