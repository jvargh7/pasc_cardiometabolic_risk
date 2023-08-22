rm(list=ls());gc();source(".Rprofile")
# county_cpr <- readRDS(paste0(path_community_profile_reports,"/counties_df_clean.RDS")) %>% 
#   dplyr::select(V01,V02,V08,V33,V34,V35,VH12,date_of_file) %>% 
#   rename(county_name = V01,
#          county_fips = V02,
#          county_population = V08,
#          viral_positivity_rate_last7days = V33,
#          viral_tests_total_last7days = V34,
#          viral_tests_per100k_last7days = V35,
#          last7days = VH12)
library(zoo)

cdc_county_transmission <- read_csv(paste0(path_pasc_cmr_folder,"/working/raw/United_States_COVID-19_County_Level_of_Community_Transmission_Historical_Changes_-_ARCHIVED.csv")) %>% 
  mutate(date = mdy(date)) %>% 
  arrange(county_name,date) %>% 
  dplyr::filter(date >= pandemic_start) %>% 
  mutate(cases_per_100K_7_day_count_change = case_when(cases_per_100K_7_day_count_change == "suppressed" ~ runif(1,1,9)/100000,
                                                       TRUE ~ as.numeric(cases_per_100K_7_day_count_change))) %>% 
  group_by(county_name) %>% 
  mutate(Cases_per_100k_7_day_count_sum = rollapply(cases_per_100K_7_day_count_change,7,sum,partial=TRUE,align="r")) %>% 
  ungroup()

saveRDS(cdc_county_transmission,paste0(path_pasc_cmr_folder,"/working/cleaned/cdc_county_transmission.RDS"))
