rm(list=ls());gc();source(".Rprofile")

index_date <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre201_index date.RDS"))
# https://mcdc.missouri.edu/data/georef/zcta_master.Metadata.html
dexter_zip_county <- read_csv(paste0(path_pasc_cmr_folder,"/working/raw/dexter_2321900090_extract.csv"))


library(usa)

zcs <- usa::zipcodes 

county_residence <- read_parquet(paste0(path_pasc_cmr_folder,"/working/raw/address_history_",version,".parquet")) %>% 
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
  left_join(zcs,
            by = c("ADDRESS_ZIP5" = "zip")) %>% 
  dplyr::filter(!is.na(ADDRESS_ZIP5)) %>% 
  arrange(ID,desc(living_index_date),ADDRESS_PERIOD_START,ADDRESS_PERIOD_END) %>% 
  group_by(ID) %>%
  slice(1) %>%
  left_join(dexter_zip_county %>% 
              dplyr::select(zcta5,County,Fipco) %>% 
              rename(county_name = County,
                     county_fips = Fipco),
            by = c("ADDRESS_ZIP5" = "zcta5")) %>% 
  ungroup() %>%
  collect()

length(unique(county_residence$ID))
saveRDS(county_residence,paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre207_county of residence on index date.RDS"))
