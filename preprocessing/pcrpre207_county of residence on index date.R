rm(list=ls());gc();source(".Rprofile")

library(tidygeocoder)

index_date <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre201_index date.RDS"))
# https://mcdc.missouri.edu/data/georef/zcta_master.Metadata.html
dexter_zip_county <- read_csv(paste0(path_pasc_cmr_folder,"/working/raw/dexter_2321900090_extract.csv"))

path_cms_mdpp_folder <- "C:/Cloud/OneDrive - Emory University/Papers/CMS MDPP Access"
county_characteristics <- readRDS(paste0(path_cms_mdpp_folder,"/working/analytic dataset.RDS"))



library(usa)

zcs <- usa::zipcodes 

zip_residence <- read_parquet(paste0(path_pasc_cmr_folder,"/working/raw/address_history_",version,".parquet")) %>% 
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
  ungroup() 

county_residence_dexter <- zip_residence %>%
  inner_join(dexter_zip_county %>% 
              dplyr::select(zcta5,County,Fipco) %>% 
              rename(county_name = County,
                     county_fips = Fipco),
            by = c("ADDRESS_ZIP5" = "zcta5")) %>%
  left_join(county_characteristics,
            by = "county_fips") 


county_residence_geocode <- zip_residence %>% 
  dplyr::filter(!ID %in% county_residence_dexter$ID) %>% 
  dplyr::filter(!is.na(lat),!is.na(long)) %>%
  reverse_geocode(lat=lat,long = long)   



county_residence <- bind_rows(county_residence_dexter,
          county_residence_geocode %>%  
            mutate(county_name = str_trim(str_extract(address,"[A-Za-z\\s\\-]+\\sCounty"))) %>% 
            dplyr::select(ID,address,county_name) %>%
            mutate(county_name = case_when(county_name == "Saint Lucie County" ~ "St. Lucie County",
                                           county_name == "Saint Johns County" ~ "St. Johns County",
                                           TRUE ~ county_name)) %>% 
            left_join(county_characteristics %>% 
                        dplyr::filter(state_name == "Florida"),
                      by = "county_name") %>% 
            dplyr::filter(!is.na(state_name))) %>% 
  dplyr::filter(ID %in% included_patients$ID)

length(unique(county_residence$ID))
saveRDS(county_residence,paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre207_county of residence on index date.RDS"))
