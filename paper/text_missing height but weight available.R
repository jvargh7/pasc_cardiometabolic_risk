rm(list=ls());gc();source(".Rprofile")


index_date <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/index date.RDS"))


vitals_data_availability <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/vital.RDS")) %>% 
  dplyr::select(ID,MEASURE_DATE,HT,WT,ORIGINAL_BMI) %>% 
  mutate(HT = case_when(HT > ht_max_possible | HT < ht_min_possible ~ NA_real_,
                        TRUE ~ HT)) %>% 
  group_by(ID) %>% 
  mutate(HT = zoo::na.locf(HT,na.rm=FALSE)) %>% 
  mutate(HT = zoo::na.locf(HT,na.rm=FALSE,fromLast=TRUE)) %>% 
  ungroup()  %>% 
  left_join(index_date %>% 
              dplyr::select(ID,COHORT,index_date,index_date_minus365),
            by = "ID") %>% 
  arrange(ID,MEASURE_DATE) %>% 
  # https://www.cdc.gov/nccdphp/dnpao/growthcharts/training/bmiage/page5_2.html
  mutate(WT = case_when(WT > wt_max_possible | WT < wt_min_possible ~ NA_real_,
                        TRUE ~ WT)) %>% 
  
  mutate(bmi = case_when(!is.na(ORIGINAL_BMI) ~ ORIGINAL_BMI,
                         HT == 0 ~ NA_real_,
                         !is.na(HT) ~ WT*703/(HT^2),
                         TRUE ~ NA_real_)) %>% 
  mutate(bmi = case_when(bmi < bmi_min_possible | bmi > bmi_max_possible ~ NA_real_,
                         TRUE ~ bmi)) %>% 
  group_by(is.na(bmi),is.na(HT),is.na(WT)) %>% 
  tally()




height_not_available <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/vital.RDS")) %>% 
  dplyr::select(ID,MEASURE_DATE,HT,WT,ORIGINAL_BMI) %>% 
  mutate(HT = case_when(HT > ht_max_possible | HT < ht_min_possible ~ NA_real_,
                        TRUE ~ HT)) %>% 
  group_by(ID) %>% 
  mutate(HT = zoo::na.locf(HT,na.rm=FALSE)) %>% 
  mutate(HT = zoo::na.locf(HT,na.rm=FALSE,fromLast=TRUE)) %>% 
  ungroup()  %>% 
  left_join(index_date %>% 
              dplyr::select(ID,COHORT,index_date,index_date_minus365),
            by = "ID") %>% 
  arrange(ID,MEASURE_DATE) %>% 
  # https://www.cdc.gov/nccdphp/dnpao/growthcharts/training/bmiage/page5_2.html
  mutate(WT = case_when(WT > wt_max_possible | WT < wt_min_possible ~ NA_real_,
                        TRUE ~ WT)) %>% 
  
  mutate(bmi = case_when(!is.na(ORIGINAL_BMI) ~ ORIGINAL_BMI,
                         HT == 0 ~ NA_real_,
                         !is.na(HT) ~ WT*703/(HT^2),
                         TRUE ~ NA_real_)) %>% 
  mutate(bmi = case_when(bmi < bmi_min_possible | bmi > bmi_max_possible ~ NA_real_,
                         TRUE ~ bmi)) %>% 
  dplyr::filter(is.na(bmi),is.na(HT),!is.na(WT))

weight_not_available <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/vital.RDS")) %>% 
  dplyr::select(ID,MEASURE_DATE,HT,WT,ORIGINAL_BMI) %>% 
  mutate(HT = case_when(HT > ht_max_possible | HT < ht_min_possible ~ NA_real_,
                        TRUE ~ HT)) %>% 
  group_by(ID) %>% 
  mutate(HT = zoo::na.locf(HT,na.rm=FALSE)) %>% 
  mutate(HT = zoo::na.locf(HT,na.rm=FALSE,fromLast=TRUE)) %>% 
  ungroup()  %>% 
  left_join(index_date %>% 
              dplyr::select(ID,COHORT,index_date,index_date_minus365),
            by = "ID") %>% 
  arrange(ID,MEASURE_DATE) %>% 
  # https://www.cdc.gov/nccdphp/dnpao/growthcharts/training/bmiage/page5_2.html
  mutate(WT = case_when(WT > wt_max_possible | WT < wt_min_possible ~ NA_real_,
                        TRUE ~ WT)) %>% 
  
  mutate(bmi = case_when(!is.na(ORIGINAL_BMI) ~ ORIGINAL_BMI,
                         HT == 0 ~ NA_real_,
                         !is.na(HT) ~ WT*703/(HT^2),
                         TRUE ~ NA_real_)) %>% 
  mutate(bmi = case_when(bmi < bmi_min_possible | bmi > bmi_max_possible ~ NA_real_,
                         TRUE ~ bmi)) %>% 
  dplyr::filter(is.na(bmi),!is.na(HT),is.na(WT))
