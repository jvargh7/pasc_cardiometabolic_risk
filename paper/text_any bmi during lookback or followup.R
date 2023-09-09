rm(list=ls());gc();source(".Rprofile")
index_date <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/index date.RDS"))

# Systolic Blood Pressure and BMI -----------



any_bmi_in_vitals <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/vital.RDS")) %>% 
  dplyr::select(ID,MEASURE_DATE,HT,WT, SYSTOLIC) %>% 
  mutate(HT = case_when(HT > 7.5*12 | HT < 4*12 ~ NA_real_,
                        TRUE ~ HT),
         SYSTOLIC = case_when(SYSTOLIC > sbp_max_possible | SYSTOLIC < sbp_min_possible ~ NA_real_,
                              TRUE ~ abs(SYSTOLIC))) %>% 
  group_by(ID) %>% 
  mutate(HT = zoo::na.locf(HT,na.rm=FALSE)) %>% 
  mutate(HT = zoo::na.locf(HT,na.rm=FALSE,fromLast=TRUE)) %>% 
  ungroup()  %>% 
  left_join(index_date %>% 
              dplyr::select(ID,COHORT,index_date,origin_date,max_followup_date),
            by = "ID") %>% 
  arrange(ID,MEASURE_DATE) %>% 
  # https://www.cdc.gov/nccdphp/dnpao/growthcharts/training/bmiage/page5_2.html
  mutate(WT = case_when(WT > 500 | WT < 50 ~ NA_real_,
                        TRUE ~ WT)) %>% 
  
  mutate(bmi = case_when(HT == 0 ~ NA_real_,
                         !is.na(HT) ~ WT*703/(HT^2),
                         TRUE ~ NA_real_)) %>% 
  mutate(bmi = case_when(bmi < 12 | bmi > 50 ~ NA_real_,
                         TRUE ~ bmi)) %>% 
  dplyr::filter(!is.na(bmi)) %>% 
  mutate(t = MEASURE_DATE - origin_date)


any_bmi_in_vitals %>% 
  distinct(COHORT,ID) %>% 
  group_by(COHORT) %>% 
  tally() %>% 
  summarize(n = sum(n))
