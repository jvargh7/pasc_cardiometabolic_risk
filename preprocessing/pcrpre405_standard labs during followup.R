rm(list=ls());gc();source(".Rprofile")
index_date <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre201_index date.RDS"))


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

lab_followup <- open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/lab_",version,".parquet")) %>% 
  dplyr::select(ID,ENCOUNTERID,RAW_LAB_NAME,LAB_LOINC,
                LAB_ORDER_DATE,SPECIMEN_DATE, 
                RESULT_NUM,RESULT_QUAL,RESULT_UNIT,
                NORM_MODIFIER_LOW,NORM_RANGE_LOW,NORM_MODIFIER_HIGH,NORM_RANGE_HIGH,
                RAW_RESULT) %>% 
  mutate(ID = as.character(ID)) %>% 
  
  right_join(index_date %>% 
               dplyr::select(ID,index_date,origin_date,max_followup_date,COHORT),
             by = c("ID")) %>% 
  dplyr::filter(SPECIMEN_DATE >= origin_date,SPECIMEN_DATE <= max_followup_date) %>% 
  
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
  # USED distinct --------
  distinct(ID, variable, SPECIMEN_DATE,.keep_all=TRUE) %>% 
  dplyr::filter(ID %in% included_patients$ID)

lab_followup_wide = lab_followup  %>% 
  dplyr::select(ID, variable,SPECIMEN_DATE, RESULT_NUM) %>% 
  pivot_wider(names_from=variable,values_from=RESULT_NUM)

saveRDS(lab_followup_wide,paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre405_standard labs during followup_wide.RDS"))
