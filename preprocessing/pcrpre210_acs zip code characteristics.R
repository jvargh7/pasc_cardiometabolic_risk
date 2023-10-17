rm(list=ls());gc();source(".Rprofile")

index_date <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre201_index date.RDS"))

ACSST5Y2020_S1903 <- read_csv(paste0(path_pasc_cmr_folder,"/working/raw/ACSST5Y2020.S1903_2023-10-09T091656/ACSST5Y2020.S1903-Data.csv")) %>% 
  dplyr::select(GEO_ID,NAME,S1903_C01_001E) %>% 
  mutate(ADDRESS_ZIP5 = str_replace(NAME,"ZCTA5\\s",""))

zcta5_residence <- read_parquet(paste0(path_pasc_cmr_folder,"/working/raw/address_history_",version,".parquet")) %>% 
  mutate(ADDRESS_ZIP5 = as.character(ADDRESS_ZIP5)) %>% 
  left_join(index_date %>% 
              dplyr::select(ID,index_date),
            by = "ID") %>% 
  mutate(living_index_date = case_when(is.na(ADDRESS_PERIOD_START) & is.na(ADDRESS_PERIOD_END) ~ NA_real_,
                                       is.na(ADDRESS_PERIOD_START) & (ADDRESS_PERIOD_END <= index_date) ~ 1,
                                       is.na(ADDRESS_PERIOD_END) & (ADDRESS_PERIOD_START > index_date) ~ 1, 
                                       (ADDRESS_PERIOD_START > index_date) & (ADDRESS_PERIOD_END <= index_date) ~ 1,
                                       TRUE ~ 0
  )) %>% 
  
  
  # dplyr::filter(is.na(living_index_date) | living_index_date == 1) %>% 
  left_join(ACSST5Y2020_S1903,
            by = c("ADDRESS_ZIP5")) %>% 
  dplyr::filter(!is.na(ADDRESS_ZIP5)) %>% 
  arrange(ID,desc(living_index_date),ADDRESS_PERIOD_START,ADDRESS_PERIOD_END) %>% 
  group_by(ID) %>%
  slice(1) %>%
  collect() %>% 
  dplyr::filter(ID %in% included_patients$ID)

length(unique(zcta5_residence$ID))
saveRDS(zcta5_residence,paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre210_zcta5 of residence on index date.RDS"))
