
encounter_type = open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/encounter_",version,".parquet")) %>% 
  dplyr::select(ID,ENCOUNTERID,FACILITY_LOCATION,ENC_TYPE,ADMIT_DATE,DISCHARGE_DATE) %>% 
  mutate(enc_inpatient = case_when(ENC_TYPE %in% c("EI","IP","IS","OS") ~ "Inpatient",
                                   ENC_TYPE %in% c("AV","ED","IC","TH","OA","NI","UN","OT") ~ "Outpatient",
                                   TRUE ~ NA_character_)) %>% 
  dplyr::select(ID, ENCOUNTERID, enc_inpatient) %>% 
  collect()
