rm(list=ls());gc();source(".Rprofile")

index_date <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre201_index date.RDS"))

# Anthropometry ---------
bmi_lookback <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre103_vital.RDS")) %>% 
  dplyr::select(ID,MEASURE_DATE,HT,WT,ORIGINAL_BMI) %>% 
  mutate(HT = case_when(HT > ht_max_possible | HT < ht_min_possible ~ NA_real_,
                        TRUE ~ HT)) %>% 
  group_by(ID) %>% 
  mutate(HT = zoo::na.locf(HT,na.rm=FALSE)) %>% 
  mutate(HT = zoo::na.locf(HT,na.rm=FALSE,fromLast=TRUE)) %>% 
  ungroup()  %>% 
  left_join(index_date %>% 
              dplyr::select(ID,COHORT,index_date,index_date_minus365),
            by = "ID") %>% 
  arrange(ID,MEASURE_DATE) %>% 
  # https://www.cdc.gov/nccdphp/dnpao/growthcharts/training/bmiage/page5_2.html
  mutate(WT = case_when(WT > wt_max_possible | WT < wt_min_possible ~ NA_real_,
                        TRUE ~ WT)) %>% 
  
  mutate(bmi = case_when(!is.na(ORIGINAL_BMI) ~ ORIGINAL_BMI,
                         HT == 0 ~ NA_real_,
                         !is.na(HT) ~ WT*703/(HT^2),
                         TRUE ~ NA_real_)) %>% 
  mutate(bmi = case_when(bmi < bmi_min_possible | bmi > bmi_max_possible ~ NA_real_,
                         TRUE ~ bmi)) %>% 
  # It's ok even if there are NA_real_ in bmi and other variables
  dplyr::filter(!is.na(bmi)) %>% 
  dplyr::filter(MEASURE_DATE < index_date, MEASURE_DATE >= index_date_minus365) %>% 
  group_by(ID) %>% 
  dplyr::filter(MEASURE_DATE == max(MEASURE_DATE)) %>% 
  ungroup() %>% 
  dplyr::select(ID,HT,WT,bmi,ORIGINAL_BMI) %>% 
  dplyr::filter(ID %in% included_patients$ID)


sbp_lookback <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre103_vital.RDS")) %>% 
  dplyr::select(ID,MEASURE_DATE,SYSTOLIC) %>%
  mutate(SYSTOLIC = case_when(SYSTOLIC > sbp_max_possible | SYSTOLIC < sbp_min_possible ~ NA_real_,
                              TRUE ~ abs(SYSTOLIC))) %>% 
  dplyr::filter(!is.na(SYSTOLIC)) %>% 
  left_join(index_date %>% 
              dplyr::select(ID,COHORT,index_date,index_date_minus365),
            by = "ID") %>% 
  dplyr::filter(MEASURE_DATE < index_date, MEASURE_DATE >= index_date_minus365) %>% 
  arrange(ID,MEASURE_DATE)  %>% 
  group_by(ID) %>% 
  dplyr::filter(MEASURE_DATE == max(MEASURE_DATE)) %>% 
  ungroup() %>% 
  # It's nook even if there are NA_real_ in SYSTOLIC
  # dplyr::filter(!is.na(SYSTOLIC)) %>% 
  dplyr::select(ID, SYSTOLIC) %>% 
  dplyr::filter(ID %in% included_patients$ID)


dbp_lookback <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre103_vital.RDS")) %>% 
  dplyr::select(ID,MEASURE_DATE,DIASTOLIC) %>%
  mutate(SYSTOLIC = case_when(DIASTOLIC > dbp_max_possible | DIASTOLIC < dbp_min_possible ~ NA_real_,
                              TRUE ~ abs(DIASTOLIC))) %>% 
  dplyr::filter(!is.na(DIASTOLIC)) %>% 
  left_join(index_date %>% 
              dplyr::select(ID,COHORT,index_date,index_date_minus365),
            by = "ID") %>% 
  dplyr::filter(MEASURE_DATE < index_date, MEASURE_DATE >= index_date_minus365) %>% 
  arrange(ID,MEASURE_DATE)  %>% 
  group_by(ID) %>% 
  dplyr::filter(MEASURE_DATE == max(MEASURE_DATE)) %>% 
  ungroup() %>% 
  # It's nook even if there are NA_real_ in SYSTOLIC
  # dplyr::filter(!is.na(SYSTOLIC)) %>% 
  dplyr::select(ID, DIASTOLIC) %>% 
  dplyr::filter(ID %in% included_patients$ID)


# Smoking status (current, not current) ---------
# vital.RDS is from preprocessing/pcpre_vital.R
smoking_status <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre103_vital.RDS")) %>% 
  dplyr::select(ID,MEASURE_DATE,smoking) %>% 
  left_join(index_date %>% 
              dplyr::select(ID,COHORT,index_date,index_date_minus365),
            by = "ID") %>% 
  dplyr::filter(MEASURE_DATE < index_date, MEASURE_DATE >= index_date_minus365) %>% 
  arrange(ID,MEASURE_DATE) %>% 
  group_by(ID) %>% 
  summarize(smoking = max(smoking)) %>% 
  dplyr::filter(ID %in% included_patients$ID)
table(smoking_status$smoking)
# Not everyone has vitals in last 1 year --> need to impute
# 0      1 
# 157052 125153 

# Comorbidities (obesity, cardiovascular disease, chronic lung disease, cerebrovascular diseae, hypertension, hyperlipidemia): Use grouped ICD-10 codes --------
icd10_obesity <- readxl::read_excel("data/PASC CMR Variable List.xlsx",sheet="comorbidities") %>% 
  dplyr::filter(Bowe_2022_Outcome == "Obesity") %>% 
  dplyr::select(Bachmann_2020_ICD10CM) %>% 
  pull() %>% 
  map(.,function(s){str_split(s,";\\s")}) %>% 
  unlist() %>% 
  str_replace_all(.,pattern="(\r\n|\\.\\*)",replacement="\\.") %>% 
  str_replace_all(.,pattern="(^\\.|\\*)","") %>% 
  na.omit()  

icd10_cardiovascular <- readxl::read_excel("data/PASC CMR Variable List.xlsx",sheet="comorbidities") %>% 
  dplyr::filter(Bowe_2022_Outcome == "Cardiovascular") %>% 
  dplyr::select(Bachmann_2020_ICD10CM) %>% 
  pull() %>% 
  map(.,function(s){str_split(s,";\\s")}) %>% 
  unlist() %>% 
  str_replace_all(.,pattern="(\r\n|\\.\\*)",replacement="\\.") %>% 
  str_replace_all(.,pattern="(^\\.|\\*)","") %>% 
  na.omit()

icd10_cerebrovascular <- readxl::read_excel("data/PASC CMR Variable List.xlsx",sheet="comorbidities") %>% 
  dplyr::filter(Bowe_2022_Outcome == "Cerebrovascular") %>% 
  dplyr::select(Bachmann_2020_ICD10CM) %>% 
  pull() %>% 
  map(.,function(s){str_split(s,";\\s")}) %>% 
  unlist() %>% 
  str_replace_all(.,pattern="(\r\n|\\.\\*)",replacement="\\.") %>% 
  str_replace_all(.,pattern="(^\\.|\\*)","") %>% 
  na.omit()

icd10_hypertension <- readxl::read_excel("data/PASC CMR Variable List.xlsx",sheet="comorbidities") %>% 
  dplyr::filter(Bowe_2022_Outcome == "Hypertension") %>% 
  dplyr::select(Bachmann_2020_ICD10CM) %>% 
  pull() %>% 
  map(.,function(s){str_split(s,";\\s")}) %>% 
  unlist() %>% 
  str_replace_all(.,pattern="(\r\n|\\.\\*)",replacement="\\.") %>% 
  str_replace_all(.,pattern="(^\\.|\\*)","") %>% 
  na.omit()  

icd10_pulmonary <- readxl::read_excel("data/PASC CMR Variable List.xlsx",sheet="comorbidities") %>% 
  dplyr::filter(Bowe_2022_Outcome == "Pulmonary") %>% 
  dplyr::select(Bachmann_2020_ICD10CM) %>% 
  pull() %>% 
  map(.,function(s){str_split(s,";\\s")}) %>% 
  unlist() %>% 
  str_replace_all(.,pattern="(\r\n|\\.\\*)",replacement="\\.") %>% 
  str_replace_all(.,pattern="(^\\.|\\*)","") %>% 
  na.omit()  

icd10_hyperlipidemia <- readxl::read_excel("data/PASC CMR Variable List.xlsx",sheet="comorbidities") %>% 
  dplyr::filter(Bowe_2022_Outcome == "Lipid disorders") %>% 
  dplyr::select(Bachmann_2020_ICD10CM) %>% 
  pull() %>% 
  map(.,function(s){str_split(s,";\\s")}) %>% 
  unlist() %>% 
  str_replace_all(.,pattern="(\r\n|\\.\\*)",replacement="\\.") %>% 
  str_replace_all(.,pattern="(^\\.|\\*)","") %>% 
  na.omit()  


comorbidity_diagnosis <- open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/diagnosis_",version,".parquet")) %>% 
  mutate(ID = as.character(ID)) %>% 
  right_join(index_date %>% 
               dplyr::select(ID,index_date,index_date_minus730,COHORT),
             by = c("ID")) %>% 
  dplyr::filter(DX_DATE >= index_date_minus730,DX_DATE < index_date, ENC_TYPE %in% permissible_enc_type)  %>% 
  
  mutate(obesity = as.numeric(str_detect(DX, paste0("(",paste0(icd10_obesity,collapse="|"),")"))),
         cardiovascular = as.numeric(str_detect(DX, paste0("(",paste0(icd10_cardiovascular,collapse="|"),")"))),
         cerebrovascular = as.numeric(str_detect(DX, paste0("(",paste0(icd10_cerebrovascular,collapse="|"),")"))),
         hypertension = as.numeric(str_detect(DX, paste0("(",paste0(icd10_hypertension,collapse="|"),")"))),
         pulmonary = as.numeric(str_detect(DX, paste0("(",paste0(icd10_pulmonary,collapse="|"),")"))),
         hyperlipidemia = as.numeric(str_detect(DX, paste0("(",paste0(icd10_hyperlipidemia,collapse="|"),")"))),
         
         ) %>% 
  # USED group_by --> summarize ---------

  group_by(ID) %>% 
  summarize(across(one_of("obesity","cardiovascular","cerebrovascular",
                          "hypertension","pulmonary","hyperlipidemia"), ~max(.,na.rm=TRUE))) %>% 
  collect()  %>% 
  dplyr::filter(ID %in% included_patients$ID)



# Medication classes (immunosuppressants, antihypertensives, statins, antipsychotics) -------------
rxcui_antidepressants <- readxl::read_excel("data/PASC CMR Variable List.xlsx",sheet="medication") %>% 
  dplyr::filter(Section == "ANTIDEPRESSANTS") %>% 
  dplyr::select(RXCUI) %>% 
  pull() %>% 
  na.omit()

rxcui_antipsychotics <- readxl::read_excel("data/PASC CMR Variable List.xlsx",sheet="medication") %>% 
  dplyr::filter(Section == "ANTIPSYCHOTICS") %>% 
  dplyr::select(RXCUI) %>% 
  pull() %>% 
  na.omit()

rxcui_antihypertensives <- readxl::read_excel("data/PASC CMR Variable List.xlsx",sheet="medication") %>% 
  dplyr::filter(Section == "BP MEDICATION") %>% 
  dplyr::select(RXCUI) %>% 
  pull() %>% 
  na.omit()

rxcui_statins <- readxl::read_excel("data/PASC CMR Variable List.xlsx",sheet="medication") %>%
  rename(drug_class = 'Drug class') %>% 
  dplyr::filter(Section == "CHOLESTEROL MEDICATION",drug_class == "STATINS") %>% 
  dplyr::select(RXCUI) %>% 
  pull() %>% 
  na.omit()

rxcui_immunosuppresants <- readxl::read_excel("data/PASC CMR Variable List.xlsx",sheet="medication") %>% 
  dplyr::filter(Section == "IMMUNOSUPPRESANTS") %>% 
  dplyr::select(RXCUI) %>% 
  pull() %>% 
  na.omit()

medication_history <- open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/prescribing_",version,".parquet")) %>% 
  mutate(ID = as.character(ID))   %>% 
  # Limit to permissible encounters
  left_join(open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/encounter_",version,".parquet")) %>% 
              dplyr::select(ID,ENCOUNTERID, ENC_TYPE),
            by = c("ID","ENCOUNTERID")) %>% 
  dplyr::filter(ENC_TYPE %in% permissible_enc_type) %>% 
  right_join(index_date %>% 
               dplyr::select(ID,index_date,index_date_minus365,COHORT),
             by = c("ID")) %>% 
  dplyr::filter(RX_ORDER_DATE >= index_date_minus365,RX_ORDER_DATE < index_date) %>% 
  mutate(antidepressants = case_when(RXNORM_CUI %in% rxcui_antidepressants ~ 1,
                                    TRUE ~ 0),
         antipsychotics = case_when(RXNORM_CUI %in% rxcui_antipsychotics ~ 1,
                                     TRUE ~ 0),
         antihypertensives = case_when(RXNORM_CUI %in% rxcui_antihypertensives ~ 1,
                                  TRUE ~ 0),
         statins = case_when(RXNORM_CUI %in% rxcui_statins ~ 1,
                               TRUE ~ 0),
         immunosuppresants = case_when(RXNORM_CUI %in% rxcui_immunosuppresants ~ 1,
                                    TRUE ~ 0)
         
  ) %>% 
  # USED group_by --> summarize ---------
  group_by(ID) %>% 
  summarize(across(one_of("antidepressants","antipsychotics",
                          "antihypertensives","statins","immunosuppresants"), ~max(.,na.rm=TRUE))) %>% 
  collect() %>% 
  dplyr::filter(ID %in% included_patients$ID)


# Laboratory parameters (blood glucose, serum creatinine, HbA1c, LDLc, HDLc, AST, ALT)
loinc_glucose <- readxl::read_excel("data/PASC CMR Variable List.xlsx",sheet="labs") %>% 
  dplyr::filter(search == "Glucose",include == "Yes") %>% 
  dplyr::select(LOINC_NUM) %>% 
  pull() %>% 
  na.omit()

loinc_serumcreatinine <- readxl::read_excel("data/PASC CMR Variable List.xlsx",sheet="labs") %>% 
  dplyr::filter(search == "Creatinine",include == "Yes") %>% 
  dplyr::select(LOINC_NUM) %>% 
  pull() %>% 
  na.omit()

loinc_hba1c <- readxl::read_excel("data/PASC CMR Variable List.xlsx",sheet="labs") %>% 
  dplyr::filter(search == "HbA1c",include == "Yes") %>% 
  dplyr::select(LOINC_NUM) %>% 
  pull() %>% 
  na.omit()

loinc_ldl <- readxl::read_excel("data/PASC CMR Variable List.xlsx",sheet="labs") %>% 
  dplyr::filter(search == "LDL",include == "Yes") %>% 
  dplyr::select(LOINC_NUM) %>% 
  pull() %>% 
  na.omit()

loinc_hdl <- readxl::read_excel("data/PASC CMR Variable List.xlsx",sheet="labs") %>% 
  dplyr::filter(search == "HDL",include == "Yes") %>% 
  dplyr::select(LOINC_NUM) %>% 
  pull() %>% 
  na.omit()

loinc_alt <- readxl::read_excel("data/PASC CMR Variable List.xlsx",sheet="labs") %>% 
  dplyr::filter(search == "ALT",include == "Yes") %>% 
  dplyr::select(LOINC_NUM) %>% 
  pull() %>% 
  na.omit()

loinc_ast <- readxl::read_excel("data/PASC CMR Variable List.xlsx",sheet="labs") %>% 
  dplyr::filter(search == "AST",include == "Yes") %>% 
  dplyr::select(LOINC_NUM) %>% 
  pull() %>% 
  na.omit()

lab_history <- open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/lab_",version,".parquet")) %>% 
  dplyr::select(ID,ENCOUNTERID,RAW_LAB_NAME,LAB_LOINC,
                LAB_ORDER_DATE,SPECIMEN_DATE, 
                RESULT_NUM,RESULT_QUAL,RESULT_UNIT,
                NORM_MODIFIER_LOW,NORM_RANGE_LOW,NORM_MODIFIER_HIGH,NORM_RANGE_HIGH,
                RAW_RESULT) %>% 
  mutate(ID = as.character(ID)) %>% 
  
  right_join(index_date %>% 
               dplyr::select(ID,index_date,index_date_minus365,COHORT),
             by = c("ID")) %>% 
  dplyr::filter(SPECIMEN_DATE >= index_date_minus365,SPECIMEN_DATE < index_date) %>% 
  
  mutate(glucose = case_when(LAB_LOINC %in% loinc_glucose ~ 1,
                                     TRUE ~ 0),
         serumcreatinine = case_when(LAB_LOINC %in% loinc_serumcreatinine ~ 1,
                                    TRUE ~ 0),
         hba1c = case_when(str_detect(RAW_LAB_NAME,"TROPONIN") ~ 0,
                           LAB_LOINC %in% loinc_hba1c ~ 1,
                                       TRUE ~ 0),
         ldl = case_when(LAB_LOINC %in% loinc_ldl ~ 1,
                         LAB_LOINC == "" & RAW_LAB_NAME %in% c("LDL CHOLESTEROL, DIRECT","LDL Chol Calc (NIH)") ~ 1,
                             TRUE ~ 0),
         hdl = case_when(LAB_LOINC %in% loinc_hdl ~ 1,
                                       TRUE ~ 0),
         alt = case_when(LAB_LOINC %in% loinc_alt ~ 1,
                         LAB_LOINC == "" & RAW_LAB_NAME %in% c("ALT") ~ 1,
                          TRUE ~ 0),
         ast = case_when(LAB_LOINC %in% loinc_ast ~ 1,
                         LAB_LOINC == "" & RAW_LAB_NAME %in% c("AST","AST (SGOT)","AST (SGOT) P5P") ~ 1,
                          TRUE ~ 0)
         
  ) %>% 
  mutate(variable = case_when(glucose == 1 ~ "glucose",
                              serumcreatinine == 1 ~ "serum_creatinine",
                              hba1c == 1 ~ "hba1c",
                              ldl == 1 ~ "ldl",
                              hdl == 1 ~ "hdl",
                              alt == 1 ~ "alt",
                              ast == 1 ~ "ast",
                              TRUE ~ NA_character_))   %>% 
  dplyr::filter(!is.na(variable), !is.na(RESULT_NUM) & RESULT_NUM > 0) %>% 
  collect() %>% 
  group_by(ID,variable) %>% 
  dplyr::filter(SPECIMEN_DATE == max(SPECIMEN_DATE)) %>% 
  ungroup() %>% 
  # USED distinct --------
  distinct(ID, variable, SPECIMEN_DATE,.keep_all=TRUE) %>% 
  dplyr::filter(ID %in% included_patients$ID)

lab_history_wide = lab_history  %>% 
  dplyr::select(ID, variable, RESULT_NUM) %>% 
  pivot_wider(names_from=variable,values_from=RESULT_NUM)

# Clinical characteristics prior to index date -----------

(prior_clinical <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre201_index date.RDS")) %>% 
  dplyr::select(ID, index_date, COHORT) %>% 
   left_join(smoking_status,
             by = "ID") %>% 
   left_join(bmi_lookback,
             by = "ID") %>% 
   left_join(sbp_lookback,
             by = "ID") %>% 
   left_join(dbp_lookback,
             by = "ID") %>% 
  left_join(comorbidity_diagnosis,
            by = "ID") %>% 
  left_join(medication_history,
            by = "ID") %>% 
  left_join(lab_history_wide,
            by = "ID")) %>% 
  saveRDS(.,paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre204_lookback clinical characteristics.RDS"))
 


