rm(list=ls());gc();source(".Rprofile")
index_date <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre201_index date.RDS"))

source("preprocessing/pcrpre300_encounter type.R")

unique_labs <- open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/lab_",version,".parquet")) %>% 
  group_by(LAB_LOINC,RAW_LAB_NAME) %>% 
  tally() %>% 
  collect() 

unique_labs %>% 
  write_csv(.,"preprocessing/pcrpre305_lab LAB_LOINC counts.csv") 


lb_hd_lab <- open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/lab_",version,".parquet"))  %>% 
  mutate(ID = as.character(ID),
         ENCOUNTERID = as.character(ENCOUNTERID)) %>% 
  right_join(index_date %>% 
               dplyr::select(ID,origin_date,index_date,index_date_minus365,index_date_minus730, COHORT),
             by = c("ID")) %>% 
  left_join(unique_labs,
            by=c("LAB_LOINC","RAW_LAB_NAME")) %>% 
  # Filters only frequent LOINCs ----
  dplyr::filter(n > n_hd_pro_min) %>% 
  dplyr::select(-n) %>% 
  mutate(date_type = case_when(SPECIMEN_DATE >= origin_date ~ "p4",
                               SPECIMEN_DATE >= index_date ~ "p3",
                               SPECIMEN_DATE >= index_date_minus365 ~ "p2",
                               SPECIMEN_DATE >= index_date_minus730 ~ "p1",
                               TRUE ~ NA_character_)) %>% 
  dplyr::filter(!is.na(date_type),
                # PX_TYPE %in% c("CH"),
                date_type == "p2"
  ) %>% 
  mutate(lab_loinc = paste0("LOINC_",LAB_LOINC),
         abnormal = case_when(ABN_IND %in% c("AB","AH","AL","CH","CL","CR") ~ 1,
                              ABN_IND %in% c("IN","NL") ~ 0,
                              # Others --> NI: No information, UN: Unknown, OT: Other
                              TRUE ~ NA_real_)) %>% 
  left_join(encounter_type,
            by=c("ID","ENCOUNTERID")) %>% 
  group_by(COHORT,ID,enc_inpatient, lab_loinc) %>% 
  # Number of abnormal value counts -----
  # Could have used number of measurements or proportion of abnormal value counts among conclusive/total measurements
  summarize(abnormal = sum(abnormal,na.rm=TRUE)) %>% 
  ungroup() %>% 
  collect() %>%
  arrange(ID) %>% 
  # group_by(ID) %>% 
  pivot_wider(names_from=lab_loinc,values_from="abnormal") %>% 
  dplyr::filter(ID %in% included_patients$ID)


saveRDS(lb_hd_lab,paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre305_high dimensional lab.RDS"))
