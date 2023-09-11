rm(list=ls());gc();source(".Rprofile")

# Instead of taking lookback_df directly, use this
# This does a sensible imputation of 0 for hospitalizations, diagnosis codes, medication and lookback encounter counts
source("analysis bmi/pcrab001_processing before imputation and lookback bmi exclusion.R")
lookback_cpit2dm <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre209_cpit2dm diabetes during lookback period.RDS"))
landmark_cpit2dm <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre208_cpit2dm new onset diabetes during period till origin date.RDS"))

lb_bmi_ID <- lookback_df %>% 
  dplyr::filter(!is.na(bmi),!ID %in% c(lookback_cpit2dm$ID)) %>% 
  dplyr::select(ID) %>% 
  pull()

lookback_df %>% 
  dplyr::filter(!is.na(bmi)) %>% 
  dplyr::select(ID,COHORT) %>% 
  group_by(COHORT) %>% 
  tally()

bmi_followup_ID = readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre401_anthro followup.RDS")) %>% 
  dplyr::filter(!is.na(bmi)) %>% 
  dplyr::select(ID) %>% 
  pull()


encounter_followup <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre404_encounters during followup_long.RDS")) %>% 
  group_by(ID,ENC_TYPE) %>% 
  summarize(count = sum(n)) %>% 
  pivot_wider(names_from=ENC_TYPE,values_from=count,values_fill=0)


library(gtsummary)
# Unweighted -----------
(unweighted <- lookback_df %>% 
   dplyr::filter(!is.na(bmi),!ID %in% c(lookback_cpit2dm$ID)) %>% 
   mutate(bmi_category = case_when(bmi < 18.5 ~ "Underweight",
                                   bmi >= 18.5 & bmi < 25.0 ~ "Normal",
                                   bmi >= 25.0 & bmi < 30.0 ~ "Overweight",
                                   bmi >= 30.0 ~ "Obese",
                                   TRUE ~ NA_character_)) %>% 
   left_join(encounter_followup,
             by = "ID") %>% 
   mutate(bmi_availability = case_when(ID %in% bmi_followup_ID ~ 1,
                                       TRUE ~ 2
                                       ),
          landmark_cpit2dm = case_when(ID %in% landmark_cpit2dm$ID ~ 1,
                                       TRUE ~ 2)
          ) %>% 
   mutate(
          bmi_availability = factor(bmi_availability,levels=c(1:2),
                                         labels=c("Lookback and Follow-up","Lookback only")),
          landmark_cpit2dm = factor(landmark_cpit2dm,levels=c(1,2),labels=c("New onset in Landmark",
                                                                            "No onset in Landmark"))
          
          ) %>% 
   tbl_summary(by = bmi_availability,
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
                         
                         IP,OA,OT,AV,NI,TH,ED,OS,EI,UN,IS,IC,
                         bmi_category, landmark_cpit2dm
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
                           EI~ "continuous2",UN~ "continuous2",IS~ "continuous2",IC~ "continuous2",
                           bmi_category ~ "categorical", landmark_cpit2dm ~ "categorical"
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
  gt::gtsave(filename = "paper/table_unweighted summary lookback by followup.docx")

