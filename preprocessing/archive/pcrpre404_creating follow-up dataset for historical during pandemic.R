rm(list=ls());gc();source(".Rprofile")
index_date <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre201_index date.RDS"))

# BMI -----------



anthro_followup <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre103_vital.RDS")) %>% 
  dplyr::select(ID,MEASURE_DATE,HT,WT, SYSTOLIC) %>% 
  mutate(HT = case_when(HT > ht_max_possible | HT < ht_min_possible ~ NA_real_,
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
  dplyr::filter(COHORT == "historical") %>% 
  arrange(ID,MEASURE_DATE) %>% 
  # https://www.cdc.gov/nccdphp/dnpao/growthcharts/training/bmiage/page5_2.html
  mutate(WT = case_when(WT > wt_max_possible | WT < wt_min_possible ~ NA_real_,
                        TRUE ~ WT)) %>% 
  
  mutate(bmi = case_when(HT == 0 ~ NA_real_,
                         !is.na(HT) ~ WT*703/(HT^2),
                         TRUE ~ NA_real_)) %>% 
  mutate(bmi = case_when(bmi < bmi_min_possible | bmi > bmi_max_possible ~ NA_real_,
                         TRUE ~ bmi)) %>% 
  group_by(ID) %>% 
  dplyr::filter(MEASURE_DATE >= exposed_followup_start, MEASURE_DATE <= exposed_followup_stop) %>% 
  ungroup() %>% 
  dplyr::filter(!is.na(bmi) | !is.na(SYSTOLIC)) %>% 
  mutate(t = MEASURE_DATE - origin_date)

saveRDS(anthro_followup,paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre404_anthro followup for historical during pandemic.RDS"))