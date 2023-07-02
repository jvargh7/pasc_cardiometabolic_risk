rm(list=ls());gc();source(".Rprofile")

index_date <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/index date.RDS"))

prccsr_reference <- readxl::read_excel(paste0(path_pasc_cmr_folder,"/working/dictionaries/PRCCSR-Reference-File-v2023-1.xlsx"),
                                       sheet = "PR_to_CCSR_Mapping",skip = 1) %>% 
  dplyr::rename_at(vars(everything()),~c("PX","px_description","ccsr_category","ccsr_category_description")) %>% 
  dplyr::select(PX,ccsr_category,ccsr_category_description) %>% 
  mutate(PX_TYPE = "10")

# https://www.medicalbillingandcoding.org/intro-to-cpt/ --> Not used
# Evaluation and Management: 99201 – 99499
# Anesthesia: 00100 – 01999; 99100 – 99140
# Surgery: 10021 – 69990
# Radiology: 70010 – 79999
# Pathology and Laboratory: 80047 – 89398
# Medicine: 90281 – 99199; 99500 – 99607
# https://www.allzonems.com/cpt-codes-the-three-categories-of-cpt-codes/
cpt_cat1_reference <- data.frame(PX = seq(0:99999)) %>% 
  mutate(px_category = case_when(PX %in% c(0:9999) ~ "CH1_ANESTHESIA",
                                 PX %in% c(10000:19999) ~ "CH1_INTERAUGMENTARY",
                                 PX %in% c(20000:29999) ~ "CH1_MUSCULOSKELETAL",
                                 PX %in% c(30000:39999) ~ "CH1_RESP_CARDIOVASC",
                                 PX %in% c(40000:49999) ~ "CH1_DIGESTIVE",
                                 PX %in% c(50000:59999) ~ "CH1_URINARY_GENIT",
                                 PX %in% c(60000:69999) ~ "CH1_ENDO_NERV_EYE",
                                 PX %in% c(70000:79999) ~ "CH1_RADIOLOGY",
                                 PX %in% c(80000:89999) ~ "CH1_PATH_LAB",
                                 PX %in% c(90000:99999) ~ "CH1_EVAL_MGT",
                                 TRUE ~ NA_character_
                                 )) %>% 
  mutate(PX = sprintf("%05d",PX),
         PX_TYPE = "CH")
# https://www.allzonems.com/cpt-codes-the-three-categories-of-cpt-codes/
cpt_cat2_reference <- data.frame(PX = seq(0:7025)) %>% 
  mutate(px_category = case_when(PX %in% c(1:15) ~ "CH2_COMPOSITE",
                                 PX %in% c(500:575) ~ "CH2_PAT_MGT",
                                 PX %in% c(1000:1220) ~ "CH2_PATIENT_HISTORY",
                                 PX %in% c(2000:2050) ~ "CH2_PHYSICAL_EXAM",
                                 PX %in% c(3006:3573) ~ "CH2_DIAGNOSTIC_RESULTS",
                                 PX %in% c(4000:4306) ~ "CH2_THER_PREV_INTERVENTIONS",
                                 PX %in% c(5005:5100) ~ "CH2_FOLLOWUP",
                                 PX %in% c(6005:6045) ~ "CH2_PATIENT_SAFETY",
                                 PX %in% c(7010:7025) ~ "CH2_STRUCTURAL_MEASURES",
                                 TRUE ~ NA_character_
  )) %>% 
  dplyr::filter(!is.na(px_category)) %>% 
  mutate(PX = paste0(sprintf("%04d",PX),"F"),
         PX_TYPE = "CH")


unique_procedures <- open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/procedures_",version,".parquet")) %>% 
  group_by(PX,PX_TYPE) %>% 
  tally() %>% 
  collect() 
table(unique_procedures$PX_TYPE)

# 09    10    CH    NI    OT 
# 394 16246 18297  1914  5936 

write_csv(unique_procedures,paste0(path_pasc_cmr_folder,"/working/pcrpre302_summary high dimensional procedures.csv"))





lb_hd_procedures <- open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/procedures_",version,".parquet"))  %>% 
  mutate(ID = as.character(ID)) %>% 
  right_join(index_date %>% 
               dplyr::select(ID,index_date,index_date_minus730,COHORT),
             by = c("ID")) %>% 
  dplyr::filter(PX_DATE >= index_date_minus730,PX_DATE < index_date) %>% 
  left_join(
    bind_rows(prccsr_reference %>% rename(px_category = ccsr_category),
              cpt_cat1_reference,
              cpt_cat2_reference
              ),
    by = c("PX","PX_TYPE")) %>% 
  group_by(COHORT,ID,px_category) %>% 
  tally() %>% 
  dplyr::filter(!is.na(px_category)) %>% 
  collect() %>% 
  pivot_wider(names_from="px_category",values_from="n")

saveRDS(lb_hd_procedures,paste0(path_pasc_cmr_folder,"/working/cleaned/lookback high dimensional procedures.RDS"))
