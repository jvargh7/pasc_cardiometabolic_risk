rm(list=ls());gc();source(".Rprofile")

demographic <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/demographic.RDS")) %>% 
  dplyr::select(ID, COHORT)


# From preprocessing/pcpre_lab.R ---------
# Other codes: "INDETERMINATE", "INVALID", "NI": No Information, "OT" : Other, "UN" : Unknown,"<BLANK>"
lab_RESULT_covid <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/lab_RESULT_covidtests.RDS")) %>% 
  mutate(positive = case_when(RESULT_QUAL %in% c("DETECTED","ABNORMAL","POSITIVE") ~ 1,
                              RESULT_NUM > 0 ~ 1,
                              TRUE ~ 0),
         negative = case_when(RESULT_QUAL %in% c("NEGATIVE","NOT DETECTED","UNDETECTABLE") ~ 1,
                              RESULT_NUM == 0 ~ 1,
                              TRUE ~ 0))


# From preprocessing/pcpre_diagnosis.R ---------
diagnosis_covid <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/diagnosis_covid.RDS")) %>% 
  mutate(positive = 1)

all_cohorts_dx = bind_rows(
  lab_RESULT_covid %>% 
    dplyr::select(ID,ENCOUNTERID,lab_order_date,RESULT_QUAL,RESULT_NUM,positive,negative) %>% 
    rename(index_date = lab_order_date) %>% 
    distinct(ID,index_date,positive,negative,.keep_all=TRUE),
  diagnosis_covid %>% 
    dplyr::select(ID,ENCOUNTERID,DX,admit_date,dx_date,positive) %>% 
    mutate(index_date = case_when(!is.na(dx_date) ~ dx_date,
                                  TRUE ~ admit_date)) %>% 
    distinct(ID,index_date,DX,.keep_all=TRUE)
) %>% 
  arrange(ID,index_date) 

# Patients 18-100 years of age with at least 2 encounters in the 24 months preceding the index date 
# and at least 1 encounter in the follow-up period who also met eligibility criteria specific to any of the 3 cohorts:

# Exposed cohort ---------
# patients with a positive COVID test or COVID-related illness between March 1, 2020 and January 29, 2022
# from 'AGE' definition --> Exposed cohort: first positive COVID test (LAB_ORDER_DATE) or record of COVID-related illness
exposed_cohort = all_cohorts_dx %>% 
  dplyr::filter(ID %in% demographic[demographic$COHORT == "exposed",]$ID) %>% 
  dplyr::filter(positive == 1) %>% 
  ungroup() %>% 
  group_by(ID) %>% 
  dplyr::filter(index_date == min(index_date)) %>% 
  dplyr::distinct(ID,index_date)

missing_exposed = all_cohorts_dx %>% 
  dplyr::filter(ID %in% demographic[demographic$COHORT == "exposed",]$ID) %>% 
  anti_join(exposed_cohort,
            by="ID") 

# Unexposed cohort ----------- 
# patients with at least 1 negative COVID test and no COVID-related illness between March 1, 2020 and January 29, 2022, 
# and no positive COVID test during the follow-up period
# from 'AGE' definition --> unexposed cohort: first negative COVID test (LAB_ORDER_DATE)
unexposed_cohort = all_cohorts_dx %>% 
  dplyr::filter(ID %in% demographic[demographic$COHORT == "unexposed",]$ID) %>% 
  dplyr::filter(negative == 1) %>%
  group_by(ID) %>% 
  dplyr::filter(index_date == min(index_date)) %>% 
  dplyr::distinct(ID,index_date)

missing_unexposed = all_cohorts_dx %>% 
  dplyr::filter(ID %in% demographic[demographic$COHORT == "unexposed",]$ID) %>% 
  anti_join(unexposed_cohort,
            by="ID") 

# Historical control cohort ------------
# patients with at least 2 encounters 2 years before the index date between March 2, 2018 and January 30, 2020 
# who were not subsequently part of the exposed or unexposed cohorts.
# from 'AGE' definition --> historical cohort: first clinic visit within identification period
# From preprocessing/pcpre_encounter.R ---------
historical_cohort <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/encounter.RDS")) %>% 
  dplyr::filter(ID %in% demographic[demographic$COHORT == "historical",]$ID,ENC_TYPE %in% admissible_encounter_types) %>% 
  dplyr::filter(admit_date >= historical_identification_start) %>% 
  group_by(ID) %>% 
  dplyr::filter(admit_date == min(admit_date)) %>% 
  dplyr::distinct(ID,admit_date) %>% 
  rename(index_date = admit_date)
  

# Identified index dates ---------

bind_rows(exposed_cohort %>% mutate(COHORT = "exposed"),
          unexposed_cohort %>% mutate(COHORT = "unexposed"),
          historical_cohort %>% mutate(COHORT = "historical")) %>% 
  saveRDS(.,paste0(path_pasc_cmr_folder,"/working/cleaned/index date.RDS"))

bind_rows(missing_exposed %>% mutate(COHORT = "exposed"),
          missing_unexposed %>% mutate(COHORT = "unexposed")) %>% 
  saveRDS(.,paste0(path_pasc_cmr_folder,"/working/issues/pcpre02_missing exposed and unexposed in all_cohorts_dx.RDS")) 
