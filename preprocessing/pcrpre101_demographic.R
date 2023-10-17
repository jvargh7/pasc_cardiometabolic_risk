rm(list=ls());gc();source(".Rprofile")

# Used on 31 May to save .parquet files
source("functions/month_replace.R")
index_date <- read_csv(paste0(path_pasc_proposal_folder,"/working/source/Demographic_chakkalakal_v2.csv")) %>% 
  mutate(across(ends_with("_date"), ~ymd(month_replace(.))))


demographic <- read_parquet(paste0(path_pasc_cmr_folder,"/working/raw/demographic_",version,".parquet")) %>% 
  left_join(index_date %>% 
              dplyr::select(ID,index_date),
            by = "ID") %>% 
  dplyr::mutate(nhwhite = case_when(HISPANIC %in% c("N","NI","OT","R","UN") & RACE == "05" ~ 1,
                             TRUE ~ 0),
         nhblack = case_when(HISPANIC %in% c("N","NI","OT","R","UN") & RACE == "03" ~ 1,
                             TRUE ~ 0),
         hispanic = case_when(HISPANIC %in% c("Y") ~ 1,
                              TRUE ~ 0),
         nhother = case_when(HISPANIC %in% c("N","NI","OT","R","UN") & !RACE %in% c("05","03") ~ 1,
                             TRUE ~ 0),
         female = case_when(SEX == "F" ~ 1,
                            TRUE ~ 0),
         age = case_when(AGE > 99 ~ 99,
                         TRUE ~ as.numeric(AGE))) %>% 
  dplyr::select(ID,female,
                nhwhite,nhblack,hispanic,
                nhother,age,COHORT,matchid,index_date) %>% 
  collect() %>% 
  dplyr::filter(ID %in% included_patients$ID)


saveRDS(demographic,paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre101_demographic.RDS"))
