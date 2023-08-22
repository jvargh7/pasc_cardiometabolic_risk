

encounter_followup_long <- open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/encounter_",version,".parquet")) %>% 
  dplyr::select(ID,ENCOUNTERID,FACILITY_LOCATION,ENC_TYPE,
                ADMIT_DATE,DISCHARGE_DATE)  %>% 
  right_join(index_date %>% 
               dplyr::select(ID,index_date,origin_date,max_followup_date,COHORT),
             by = c("ID")) %>% 
  dplyr::filter(ADMIT_DATE >= origin_date, ADMIT_DATE <= max_followup_date)  %>% 
  mutate(t = ADMIT_DATE - origin_date) %>% 
  mutate(m = month(ADMIT_DATE),
         y = year (ADMIT_DATE)) %>% 
  group_by(ID,ENC_TYPE,y,m) %>% 
  tally()  %>% 
  collect()

saveRDS(encounter_followup_long,paste0(path_pasc_cmr_folder,"/working/cleaned/encounter_followup_long.RDS"))
