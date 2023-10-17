rm(list=ls());gc();source(".Rprofile")

index_date <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre201_index date.RDS"))

source("preprocessing/pcrpre300_encounter type.R")


unique_procedures <- open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/procedures_",version,".parquet")) %>% 
  group_by(PX,PX_TYPE) %>% 
  tally() %>% 
  collect() 
table(unique_procedures$PX_TYPE)

# 09    10    CH    NI    OT 
# 394 16246 18297  1914  5936 

summary(unique_procedures$n)

write_csv(unique_procedures,paste0(path_pasc_cmr_folder,"/working/pcrpre302_summary high dimensional procedures.csv"))





lb_hd_procedures <- open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/procedures_",version,".parquet"))  %>% 
  mutate(ID = as.character(ID)) %>% 
  right_join(index_date %>% 
               dplyr::select(ID,origin_date,index_date,index_date_minus365,index_date_minus730, COHORT),
             by = c("ID")) %>% 
  left_join(unique_procedures,
            by=c("PX","PX_TYPE")) %>% 
  dplyr::filter(n > n_hd_pro_min) %>% 
  dplyr::select(-n) %>% 
  mutate(date_type = case_when(PX_DATE >= origin_date ~ "p4",
                               PX_DATE >= index_date ~ "p3",
                               PX_DATE >= index_date_minus365 ~ "p2",
                               PX_DATE >= index_date_minus730 ~ "p1",
                               TRUE ~ NA_character_)) %>% 
  dplyr::filter(!is.na(date_type),
                # PX_TYPE %in% c("CH"),
                date_type == "p2"
                ) %>% 
  mutate(PX_TYPE = case_when(PX_TYPE == "10" ~ "ICD10",
                             TRUE ~ "CH")) %>% 
  left_join(encounter_type,
            by=c("ID","ENCOUNTERID")) %>% 
  group_by(COHORT,ID,enc_inpatient, PX_TYPE, PX) %>% 
  tally() %>% 
  ungroup() %>% 
  collect() %>%
  arrange(ID) %>% 
  # group_by(ID) %>% 
  pivot_wider(names_from=c("PX_TYPE","PX"),names_sep = "_",values_from="n") %>% 
  dplyr::filter(ID %in% included_patients$ID)


saveRDS(lb_hd_procedures,paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre302_high dimensional procedures.RDS"))
