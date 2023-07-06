rm(list=ls());gc();source(".Rprofile")

index_date <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/index date.RDS"))
source("preprocessing/pcrpre_encounter type.R")


dxccsr_reference <- readxl::read_excel(paste0(path_pasc_cmr_folder,"/working/dictionaries/DXCCSR-Reference-File-v2023-1.xlsx"),
                                       sheet = "DX_to_CCSR_Mapping",skip = 1) %>% 
  dplyr::rename_at(vars(everything()),~c("DX","dx_description","ccsr_category","ccsr_category_description","ip_default","op_default")) %>% 
  dplyr::select(DX,ccsr_category,ccsr_category_description)

unique_icd10 <- open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/diagnosis_",version,".parquet")) %>% 
  group_by(DX) %>% 
  tally() %>% 
  collect() %>% 
  mutate(DX_nodec = str_replace(DX,"\\.","")) %>% 
  left_join(dxccsr_reference,
            by=c("DX_nodec"="DX")) %>% 
  mutate(across(.cols=one_of("ccsr_category","ccsr_category_description"),.funs=function(x) case_when(is.na(x) ~ "Unknown",
                                                                                                      TRUE ~ x)))
write_csv(unique_icd10,paste0(path_pasc_cmr_folder,"/working/pcrpre301_summary high dimensional comorbidities.csv"))

# QC: Missingness in index date and encounter type -------
# missing_comorbidity_encounters <- open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/diagnosis_",version,".parquet"))  %>% 
#   mutate(ID = as.character(ID)) %>% 
#   right_join(index_date %>% 
#                dplyr::select(ID,origin_date,index_date,index_date_minus365,index_date_minus730, COHORT),
#              by = c("ID")) %>% 
#   mutate(missing_date_type = case_when(!is.na(index_date) ~ "index date available",
#                                TRUE ~ "index date missing")) %>% 
#   left_join(encounter_type,
#             by=c("ID","ENCOUNTERID")) %>% 
#   mutate(missing_enc_inpatient = case_when(!is.na(enc_inpatient) ~ "enc_inpatient available",
#                                            TRUE ~ "enc_inpatient missing")) %>% 
#   group_by(missing_date_type,missing_enc_inpatient) %>% 
#   tally() %>% 
#   collect()
# ~1% of diagnosis have enc_inpatient (ENCOUNTER TYPE: IP/OP) missing


lb_hd_comorbidities <- open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/diagnosis_",version,".parquet"))  %>% 
  mutate(ID = as.character(ID)) %>% 
  right_join(index_date %>% 
               dplyr::select(ID,origin_date,index_date,index_date_minus365,index_date_minus730, COHORT),
             by = c("ID")) %>% 
  mutate(date_type = case_when(DX_DATE >= origin_date ~ "p4",
                               DX_DATE >= index_date ~ "p3",
                               DX_DATE >= index_date_minus365 ~ "p2",
                               DX_DATE >= index_date_minus730 ~ "p1",
                               TRUE ~ NA_character_)) %>% 
  dplyr::filter(!is.na(date_type)) %>% 
  left_join(encounter_type,
            by=c("ID","ENCOUNTERID")) %>% 
  # dplyr::filter(DX_DATE >= index_date_minus730,DX_DATE < index_date) %>% 
  mutate(DX_nodec = str_replace(DX,"\\.","")) %>% 
  left_join(dxccsr_reference,
            by=c("DX_nodec"="DX")) %>% 
  group_by(COHORT,ID,enc_inpatient,date_type,ccsr_category) %>% 
  tally() %>% 
  collect() %>% 
  pivot_wider(names_from="ccsr_category",values_from="n")

saveRDS(lb_hd_comorbidities,paste0(path_pasc_cmr_folder,"/working/cleaned/high dimensional comorbidities.RDS"))
# lb_hd_comorbidities <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/high dimensional comorbidities.RDS"))
