rm(list=ls());gc();source(".Rprofile")

source("functions/month_replace.R")


vital_parquet <- open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/vital_",version,".parquet")) %>% 
  dplyr::select(-ENCOUNTERID,-VITALID,-VITAL_SOURCE) %>% 
  dplyr::filter(!is.na(HT)|!is.na(WT)|!is.na(DIASTOLIC)|!is.na(SYSTOLIC)|!is.na(SMOKING)) %>% 
  # Using -1 so that min() can be applied across 
  mutate(smoking = case_when(SMOKING %in% c("01","02","03","04","05","06","07","08") ~ -1,
                             TRUE ~ 0)) %>% 
  
  # USED group_by --> summarize ---------
  group_by(ID,MEASURE_DATE) %>% 
  dplyr::summarize(across(one_of("HT","WT","DIASTOLIC","SYSTOLIC","smoking","ORIGINAL_BMI"),~min(.,na.rm=TRUE))) %>% 
  collect() %>% 
  mutate(smoking = smoking*-1) %>% 
  ungroup() 

# Refer: Email from Katie Shaw on 25-August >> HT_WT_LOINCS.docx
obsclin_parquet_units <- open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/obsclin_",version,".parquet")) %>% 
  dplyr::select(ID, OBSCLIN_START_DATE, OBSCLIN_CODE, OBSCLIN_RESULT_TEXT, OBSCLIN_RESULT_NUM,
                OBSCLIN_RESULT_UNIT) %>% 
  dplyr::filter(OBSCLIN_CODE %in% HT_LOINCS | OBSCLIN_CODE %in% WT_LOINCS | OBSCLIN_CODE %in% BMI_LOINCS ) %>% 
  group_by(OBSCLIN_CODE,OBSCLIN_RESULT_UNIT) %>% 
  summarize(n = n(),
            min = min(OBSCLIN_RESULT_NUM,na.rm=TRUE),
            max = max(OBSCLIN_RESULT_NUM,na.rm=TRUE)) %>% 
  collect()


obsclin_parquet <- open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/obsclin_",version,".parquet")) %>% 
  dplyr::select(ID, OBSCLIN_START_DATE, OBSCLIN_CODE, OBSCLIN_RESULT_TEXT, OBSCLIN_RESULT_NUM,OBSCLIN_RESULT_UNIT,
                RAW_OBSCLIN_NAME, RAW_OBSCLIN_RESULT) %>% 
  dplyr::filter(OBSCLIN_CODE %in% HT_LOINCS | OBSCLIN_CODE %in% WT_LOINCS | OBSCLIN_CODE %in% BMI_LOINCS | 
                  str_detect(RAW_OBSCLIN_NAME,"(HEIGHT|height|Height|WEIGHT|Weight|weight|body mass|BMI|Body Mass|Body mass)")) %>% 
  mutate(variable = case_when(OBSCLIN_CODE %in% HT_LOINCS ~ "HT",
                              OBSCLIN_CODE %in% WT_LOINCS ~ "WT",
                              OBSCLIN_CODE %in% BMI_LOINCS ~ "ORIGINAL_BMI",
                              str_detect(RAW_OBSCLIN_NAME,"(HEIGHT|height|Height)") ~ "HT",
                              str_detect(RAW_OBSCLIN_NAME,"(WEIGHT|Weight|weight)") ~ "WT",
                              str_detect(RAW_OBSCLIN_NAME,"(body mass|BMI|Body Mass|Body mass)") ~ "ORIGINAL_BMI",
                              TRUE ~ NA_character_),
         value = case_when(
                           OBSCLIN_CODE %in% c(HT_LOINCS,WT_LOINCS,BMI_LOINCS) ~ as.numeric(OBSCLIN_RESULT_NUM),
                           # str_detect(RAW_OBSCLIN_NAME,"(HEIGHT|height|Height|WEIGHT|Weight|weight|body mass|BMI|Body Mass|Body mass)") ~ as.numeric(RAW_OBSCLIN_RESULT),
                           TRUE ~ NA_real_)
         ) %>% 
  mutate(value = case_when(OBSCLIN_RESULT_UNIT == "[lb_av]" ~ value,
                           OBSCLIN_RESULT_UNIT == "[in_i]" ~ value,
                           OBSCLIN_RESULT_UNIT == "[ft_i]" ~ value*12,
                           OBSCLIN_RESULT_UNIT == "kg" ~ value*2.20462,
                           OBSCLIN_RESULT_UNIT == "cm" ~ value*0.393701,
                           OBSCLIN_RESULT_UNIT == "OT" ~ value,
                           OBSCLIN_RESULT_UNIT == "kg/m2" ~ value,
                           OBSCLIN_RESULT_UNIT == "g" & value < 300 ~ value*2.20462,
                           OBSCLIN_RESULT_UNIT == "g" & value >= 300 & value < 300*(10^3) ~ value*2.20462/(10^3),
                           OBSCLIN_RESULT_UNIT == "g" & value >= 300*(10^3) ~ value/(10^3),
                           OBSCLIN_CODE == "3142-7" & OBSCLIN_RESULT_UNIT == "" ~ value, #lbs?
                           OBSCLIN_CODE == "39156-5" & OBSCLIN_RESULT_UNIT == "" ~ value, #kg/m2
                           OBSCLIN_CODE == "8302-2" & OBSCLIN_RESULT_UNIT == "" ~ value, #inches
                           OBSCLIN_CODE == "3137-7" & OBSCLIN_RESULT_UNIT == "" ~ value, #inches
                           TRUE ~ NA_real_
                           )) %>% 
  
  dplyr::select(ID, OBSCLIN_START_DATE,variable,value) %>% 
  group_by(ID,OBSCLIN_START_DATE,variable) %>% 
  dplyr::summarize(value = mean(value,na.rm = TRUE)) %>% 
  collect() %>% 
  pivot_wider(names_from=variable,values_from=value,values_fill = NA_real_) %>% 
  rename(MEASURE_DATE = OBSCLIN_START_DATE)


(vital = left_join(vital_parquet,
          obsclin_parquet %>% rename(ORIGINAL_BMI_obsclin = ORIGINAL_BMI,
                                     HT_obsclin = HT,
                                     WT_obsclin = WT), 
  by = c("ID","MEASURE_DATE")) %>% 
    mutate(ORIGINAL_BMI = case_when(is.na(ORIGINAL_BMI) ~ ORIGINAL_BMI_obsclin,
                                    TRUE ~ ORIGINAL_BMI),
           HT = case_when(is.na(HT) ~ HT_obsclin,
                          HT > ht_max_possible*2.54 ~ HT*0.393701,
                          TRUE ~ HT),
           WT = case_when(is.na(WT) ~ WT_obsclin,
                          TRUE ~ WT))) %>%
  dplyr::select(-matches("obsclin")) %>% 
  saveRDS(.,paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre103_vital.RDS"))

summary(vital_parquet)
summary(vital)
