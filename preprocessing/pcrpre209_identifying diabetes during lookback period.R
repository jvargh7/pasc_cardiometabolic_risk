rm(list=ls());gc();source(".Rprofile")
index_date <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre201_index date.RDS"))

hba1c <- open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/lab_",version,".parquet")) %>% 
  mutate(ID = as.character(ID)) %>% 
  dplyr::filter(str_detect(RAW_LAB_NAME,"(A1C|A1c)")) %>% 
  dplyr::filter(LAB_LOINC %in% hba1c_loinc |
                  # The below have LAB_LOINC == ""
                  RAW_LAB_NAME %in% c("(HEMOGLOBIN A1C|Hemoglobin A1c|Hemoglobin A1C|POCT HBA1C|HM HBA1C)")) %>% 
  dplyr::select(ID,ENCOUNTERID,RAW_LAB_NAME,LAB_LOINC,
                LAB_ORDER_DATE,SPECIMEN_DATE, 
                RESULT_NUM,RESULT_QUAL,RESULT_UNIT,
                NORM_MODIFIER_LOW,NORM_RANGE_LOW,NORM_MODIFIER_HIGH,NORM_RANGE_HIGH,
                RAW_RESULT) %>% 
  right_join(index_date %>% 
               dplyr::select(ID,index_date,index_date_minus730,COHORT),
             by = c("ID"))  %>% 
  dplyr::filter(SPECIMEN_DATE >= index_date_minus730,SPECIMEN_DATE < index_date)  %>% 
  # Correction
  mutate(value = case_when(RESULT_NUM > 20 ~ NA_real_,
                           TRUE ~ RESULT_NUM)) %>% 
  
  # NI: No information
  mutate(high_hba1c = case_when(
    # RESULT_QUAL %in% c("HIGH")  --> cannot use this since it counts >=6% as HIGH 
    value >= 6.5 ~ 1,
    RESULT_QUAL %in% c("LOW","NEGATIVE","NI","OT","UNDETECTABLE") | (value>0 & value < 6.5) ~ 0,
    TRUE ~ NA_real_)) %>% 
  collect() 

length(unique(hba1c$ID))
summary(hba1c$RESULT_NUM)


dm_diagnosis <- open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/diagnosis_",version,".parquet")) %>% 
  mutate(ID = as.character(ID)) %>% 
  right_join(index_date %>% 
               dplyr::select(ID,index_date,index_date_minus730,COHORT),
             by = c("ID")) %>% 
  dplyr::filter(DX_DATE >= index_date_minus730,DX_DATE  < index_date)  %>% 
  dplyr::filter(!str_detect(DX,paste0("(",paste0(c(icd10_otherdm_excluding,
                                                   icd10_t1dm,icd10_gdm),collapse="|"),")"))) %>% 
  dplyr::filter(DX %in% icd10_dm_qualifying) %>%
  collect()

rxcui_list <- readxl::read_excel("data/PASC CMR Variable List.xlsx",sheet="medication") %>% 
  rename(drug_class = 'Drug class',
         drug_name = 'Drug name') %>% 
  dplyr::filter(drug_class %in% c("INSULIN","METFORMIN","SGLT2 INHIBITORS",
                                  "GLP1 RA","DPP4 INHIBITOR","THIAZOLIDINEDIONES",
                                  "SULFONYLUREAS","MEGLITINIDES","AGI")) %>% 
  dplyr::select(RXCUI) %>% 
  pull() %>% 
  na.omit()

dm_medication <- open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/prescribing_",version,".parquet")) %>% 
  mutate(ID = as.character(ID)) %>% 
  right_join(index_date %>% 
               dplyr::select(ID,index_date,index_date_minus730,COHORT),
             by = c("ID")) %>% 
  dplyr::filter(RX_ORDER_DATE >= index_date_minus730,RXNORM_CUI %in% rxcui_list,RX_ORDER_DATE < index_date) %>% 
  collect()

# CP1 --------------

cp1 <- dm_diagnosis %>% 
  left_join(dm_medication %>% 
              dplyr::select(ID,RX_ORDER_DATE,RXNORM_CUI),
            by = "ID") %>% 
  dplyr::filter(DX_DATE <= (RX_ORDER_DATE + 90), DX_DATE >= (RX_ORDER_DATE-90)) %>% 
  mutate(criterion1_date = pmin(DX_DATE,RX_ORDER_DATE),
         criterion2_date = pmax(DX_DATE,RX_ORDER_DATE)) %>% 
  distinct(ID,criterion2_date,.keep_all=TRUE) %>% 
  group_by(ID) %>% 
  dplyr::filter(criterion2_date == min(criterion2_date)) %>% 
  ungroup()


# CP2 -------------

cp2 <- dm_diagnosis %>% 
  left_join(hba1c %>% 
              dplyr::select(ID,SPECIMEN_DATE,RESULT_NUM,RESULT_QUAL,RESULT_UNIT,high_hba1c) %>% 
              dplyr::filter(high_hba1c == 1),
            by = "ID") %>% 
  dplyr::filter(DX_DATE <= (SPECIMEN_DATE + 90), DX_DATE >= (SPECIMEN_DATE-90)) %>% 
  mutate(criterion1_date = pmin(DX_DATE,SPECIMEN_DATE),
         criterion2_date = pmax(DX_DATE,SPECIMEN_DATE)) %>% 
  distinct(ID,criterion2_date,.keep_all=TRUE) %>% 
  group_by(ID) %>% 
  dplyr::filter(criterion2_date == min(criterion2_date)) %>% 
  ungroup()

# CP3 -------------

cp3 <- hba1c %>% 
  dplyr::select(ID,SPECIMEN_DATE,RESULT_NUM,RESULT_QUAL,RESULT_UNIT,high_hba1c,COHORT) %>% 
  dplyr::filter(high_hba1c == 1) %>% 
  left_join(dm_medication %>% 
              dplyr::select(ID,RX_ORDER_DATE,RXNORM_CUI),
            by = "ID") %>% 
  dplyr::filter(RX_ORDER_DATE <= (SPECIMEN_DATE + 90), RX_ORDER_DATE >= (SPECIMEN_DATE-90)) %>% 
  mutate(criterion1_date = pmin(SPECIMEN_DATE,RX_ORDER_DATE),
         criterion2_date = pmax(SPECIMEN_DATE,RX_ORDER_DATE)) %>% 
  distinct(ID,criterion2_date,.keep_all=TRUE) %>% 
  group_by(ID) %>% 
  dplyr::filter(criterion2_date == min(criterion2_date)) %>% 
  ungroup()


# New onset diabetes ---------

cpit2dm <- bind_rows(cp1 %>% 
                       mutate(
                         CP = "CP1"),
                     
                     cp2 %>% 
                       mutate(
                         CP = "CP2"),
                     cp3 %>% 
                       mutate(
                         CP = "CP3")) %>% 
  group_by(ID) %>% 
  dplyr::filter(criterion2_date == min(criterion2_date)) %>% 
  slice(1) %>% 
  ungroup() 

table(cpit2dm$COHORT)

cpit2dm %>% 
  # dplyr::select() %>% 
  saveRDS(.,paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre209_cpit2dm diabetes during lookback period.RDS"))