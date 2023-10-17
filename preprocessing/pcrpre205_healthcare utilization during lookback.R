rm(list=ls());gc();source(".Rprofile")


index_date <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre201_index date.RDS"))

# Number of outpatient visits ---------
# Number of telehealth encounters -------
# Number of hospitalizations -------
lb_n_hospitalization <- open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/encounter_",version,".parquet")) %>% 
  dplyr::select(ID,ENCOUNTERID,FACILITY_LOCATION,ENC_TYPE,ADMIT_DATE,DISCHARGE_DATE) %>% 
  right_join(index_date %>% 
               dplyr::select(ID,index_date,index_date_minus365),
             by = c("ID")) %>% 
  mutate(lb_hospitalized = case_when(ENC_TYPE %in% c("EI","IP","IS","OS") ~ 1,
                                  TRUE ~ 0),
         lb_not_hospitalized = case_when(ENC_TYPE %in% c("AV","ED","IC","TH","OA","NI","UN","OT") ~ 1,
                                      TRUE ~ 0),
         lb_telehealth = case_when(ENC_TYPE %in% c("TH") ~ 1,
                                TRUE ~ 0),
         lb_outpatient = case_when(ENC_TYPE %in% c("AV","OA","IC") ~ 1,
                                TRUE ~ 0)) %>% 
  dplyr::filter(ADMIT_DATE  >= index_date_minus365,ADMIT_DATE < index_date)  %>% 
  # USED group_by --> summarize ---------
  group_by(ID) %>% 
  # Counts of encounters when hospitalized
  dplyr::summarize(across(one_of("lb_hospitalized","lb_telehealth","lb_outpatient"),~sum(.))) %>% 
  ungroup() %>% 
  collect() %>% 
  dplyr::filter(ID %in% included_patients$ID)

# Number of lab visits -------

lb_n_labvisits <- open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/lab_",version,".parquet")) %>% 
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
  dplyr::filter(!is.na(RESULT_QUAL) | (RESULT_QUAL !="") |!is.na(RESULT_NUM)|!is.na(RAW_RESULT)) %>% 
  distinct(ID,SPECIMEN_DATE) %>% 
  group_by(ID) %>% 
  tally() %>% 
  collect() %>% 
  rename(lb_n_labvisits = n) %>% 
  dplyr::filter(ID %in% included_patients$ID)

# Number of HbA1c measurements -------

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

lb_n_labs <- open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/lab_",version,".parquet")) %>% 
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
  mutate(variable = case_when(glucose == 1 ~ "lb_n_glucose",
                              serumcreatinine == 1 ~ "lb_n_serum_creatinine",
                              hba1c == 1 ~ "lb_n_hba1c",
                              ldl == 1 ~ "lb_n_ldl",
                              hdl == 1 ~ "lb_n_hdl",
                              alt == 1 ~ "lb_n_alt",
                              ast == 1 ~ "lb_n_ast",
                              TRUE ~ NA_character_))   %>% 
  dplyr::filter(!is.na(variable), !is.na(RESULT_NUM) & RESULT_NUM > 0) %>% 
  collect() %>% 
  group_by(ID,variable) %>% 
  tally() %>% 
  ungroup() %>% 
  pivot_wider(names_from=variable,values_from=n) %>% 
  dplyr::filter(ID %in% included_patients$ID)


(lb_healthcare_utilization <- index_date %>% 
    dplyr::select(ID, COHORT) %>% 
    left_join(lb_n_hospitalization ,
              by = "ID") %>% 
    left_join(lb_n_labs ,
              by = "ID") %>% 
    left_join(lb_n_labvisits,
              by = "ID"))  

lb_healthcare_utilization[is.na(lb_healthcare_utilization)] <- 0

saveRDS(lb_healthcare_utilization,paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre205_lookback healthcare utilization.RDS"))
