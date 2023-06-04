rm(list=ls());gc();source(".Rprofile")
index_date <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/index date.RDS"))

# Systolic Blood Pressure and BMI -----------

anthro_followup <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/vital.RDS")) %>% 
  dplyr::select(ID,MEASURE_DATE,SYSTOLIC,HT,WT) %>% 
  left_join(index_date %>% 
              dplyr::select(ID,COHORT,index_date,origin_date),
            by = "ID")  %>% 
  dplyr::filter(MEASURE_DATE >= origin_date) %>% 
  mutate(valid_date = case_when(COHORT %in% c("exposed","unexposed") & MEASURE_DATE %in% c(exposed_followup_start:exposed_followup_stop) ~ 1,
                                COHORT %in% c("historical") & MEASURE_DATE %in% c(historical_followup_start:historical_followup_stop) ~ 1,
                                TRUE ~ 0
  )) %>% 
  dplyr::filter(valid_date == 1) %>% 

  arrange(ID,MEASURE_DATE) %>% 
  mutate(t = MEASURE_DATE - origin_date) %>% 
  group_by(ID) %>% 
  mutate(HT = zoo::na.locf(HT,na.rm=FALSE)) %>% 
  mutate(HT = zoo::na.locf(HT,na.rm=FALSE,fromLast=TRUE)) %>% 
  ungroup() %>% 
  # https://www.cdc.gov/nccdphp/dnpao/growthcharts/training/bmiage/page5_2.html
  
  mutate(bmi = case_when(HT == 0 ~ NA_real_,
                         !is.na(HT) ~ WT*703/(HT^2),
                         TRUE ~ NA_real_)) %>% 
  mutate(HT = case_when(HT > 7.5*12 | HT < 4*12 ~ NA_real_,
                        TRUE ~ HT),
         WT = case_when(WT > 500 | WT < 50 ~ NA_real_,
                        TRUE ~ WT),
         bmi = case_when(bmi < 12 | bmi > 50 ~ NA_real_,
                         TRUE ~ bmi)) %>% 
  dplyr::filter(!is.na(bmi) | !is.na(SYSTOLIC))

saveRDS(anthro_followup,paste0(path_pasc_cmr_folder,"/working/cleaned/anthro followup.RDS"))

# Low density lipoprotein -----------

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

lab_followup <- open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/lab_",version,".parquet")) %>% 
  dplyr::select(ID,ENCOUNTERID,RAW_LAB_NAME,LAB_LOINC,
                LAB_ORDER_DATE,SPECIMEN_DATE, 
                RESULT_NUM,RESULT_QUAL,RESULT_UNIT,
                NORM_MODIFIER_LOW,NORM_RANGE_LOW,NORM_MODIFIER_HIGH,NORM_RANGE_HIGH,
                RAW_RESULT) %>% 
  mutate(ID = as.character(ID)) %>% 
  
  right_join(index_date %>% 
               dplyr::select(ID,index_date,origin_date,COHORT),
             by = c("ID")) %>% 
  mutate(valid_date = case_when(COHORT %in% c("exposed","unexposed") & SPECIMEN_DATE <= exposed_followup_stop ~ 1,
                                COHORT %in% c("historical") & SPECIMEN_DATE <= historical_followup_stop ~ 1,
                                TRUE ~ 0
  )) %>% 
  dplyr::filter(SPECIMEN_DATE >= origin_date,valid_date == 1) %>% 
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
  # USED distinct ---------
  distinct(ID, variable, SPECIMEN_DATE,.keep_all=TRUE)

lab_followup_wide = lab_followup  %>% 
  dplyr::select(ID, SPECIMEN_DATE, COHORT, origin_date,variable, RESULT_NUM) %>% 
  pivot_wider(names_from=variable,values_from=RESULT_NUM) %>% 
  arrange(ID, SPECIMEN_DATE) %>% 
  mutate(t = SPECIMEN_DATE - origin_date)

saveRDS(lab_followup_wide,paste0(path_pasc_cmr_folder,"/working/cleaned/lab_followup_wide.RDS"))
