rm(list=ls());gc();source(".Rprofile")

id_variables = c("date_type","enc_inpatient","COHORT")

index_date <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/index date.RDS"))

ip_diagnosis <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/high dimensional comorbidities.RDS")) %>% 
  dplyr::filter(date_type == "p2",enc_inpatient == "Inpatient") %>% 
  ungroup() %>% 
  dplyr::select(-one_of(id_variables)) %>% 
  rename_at(vars(-one_of(c("ID"))), ~paste0("ipd_",.))

ip_diagnosis_availability <- index_date %>% 
  left_join(
    ip_diagnosis,
    by = "ID") %>%
  mutate_all(~case_when(is.na(.) ~ 0,
                        TRUE ~ 1)) %>% 
  summarize(across(matches("^(ipd|opd|ipp|opp|pre|lab)"),.f=~mean(.)))


op_diagnosis <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/high dimensional comorbidities.RDS")) %>% 
  dplyr::filter(date_type == "p2",enc_inpatient == "Outpatient") %>% 
  ungroup() %>% 
  dplyr::select(-one_of(id_variables)) %>% 
  rename_at(vars(-one_of(c("ID"))), ~paste0("opd_",.))

op_diagnosis_availability <- index_date %>% 
  left_join(
    op_diagnosis,
    by = "ID"
  ) %>%
  mutate_all(~case_when(is.na(.) ~ 0,
                        TRUE ~ 1)) %>% 
  summarize(across(matches("^(ipd|opd|ipp|opp|pre|lab)"),.f=~mean(.)))

ip_procedures <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/high dimensional procedures.RDS")) %>% 
  dplyr::filter(
    # date_type == "p2",  
    enc_inpatient == "Inpatient") %>% 
  ungroup() %>% 
  dplyr::select(-one_of(id_variables)) %>% 
  rename_at(vars(-one_of(c("ID"))), ~paste0("ipp_",.))

ip_procedures_availability <- index_date %>% 
  left_join(
    ip_procedures,
  by = "ID" ) %>%
  mutate_all(~case_when(is.na(.) ~ 0,
                        TRUE ~ 1)) %>% 
  summarize(across(matches("^(ipd|opd|ipp|opp|pre|lab)"),.f=~mean(.)))

op_procedures <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/high dimensional procedures.RDS")) %>% 
  dplyr::filter(
    # date_type == "p2",  
    enc_inpatient == "Outpatient") %>% 
  ungroup() %>% 
  dplyr::select(-one_of(id_variables)) %>% 
  rename_at(vars(-one_of(c("ID"))), ~paste0("opp_",.))

op_procedures_availability <- index_date %>% 
  left_join(
    op_procedures,
    by = "ID" ) %>%
  mutate_all(~case_when(is.na(.) ~ 0,
                        TRUE ~ 1)) %>% 
  summarize(across(matches("^(ipd|opd|ipp|opp|pre|lab)"),.f=~mean(.)))

prescribing <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/high dimensional prescribing.RDS")) %>% 
  # Prescribing has both inpatient and outpatient for enc_inpatient
  dplyr::filter(date_type == "p2"
                # enc_inpatient == "Outpatient"
                ) %>% 
  group_by(ID) %>% 
  summarize(across(-one_of(c("ID",id_variables)),~sum(.,na.rm=TRUE))) %>% 
  ungroup() %>% 
  dplyr::select(-one_of(id_variables)) %>% 
  rename_at(vars(-one_of(c("ID"))), ~paste0("pre_",.))

prescribing_availability <- index_date %>% 
  left_join(
    prescribing ,
    by = "ID" ) %>%
  mutate_all(~case_when(is.na(.) ~ 0,
                        TRUE ~ 1)) %>% 
  summarize(across(matches("^(ipd|opd|ipp|opp|pre|lab)"),.f=~mean(.)))



lab <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/high dimensional lab.RDS")) %>% 
  group_by(ID) %>% 
  summarize(across(-one_of(c("ID",id_variables)),~sum(.,na.rm=TRUE))) %>% 
  ungroup() %>% 
  dplyr::select(-one_of(id_variables)) %>% 
  rename_at(vars(-one_of(c("ID"))), ~paste0("lab_",.))

lab_availability <- index_date %>% 
  left_join(
    lab ,
    by = "ID" ) %>%
  mutate_all(~case_when(is.na(.) ~ 0,
                        TRUE ~ 1)) %>% 
  summarize(across(matches("^(ipd|opd|ipp|opp|pre|lab)"),.f=~mean(.)))



hd_dataset_availability <- bind_rows(
  pivot_longer(ip_diagnosis_availability,cols=everything(),names_to="variable",values_to="availability"),
  pivot_longer(op_diagnosis_availability,cols=everything(),names_to="variable",values_to="availability"),
  pivot_longer(ip_procedures_availability,cols=everything(),names_to="variable",values_to="availability"),
  pivot_longer(op_procedures_availability,cols=everything(),names_to="variable",values_to="availability"),
  pivot_longer(prescribing_availability,cols=everything(),names_to="variable",values_to="availability"),
  pivot_longer(lab_availability,cols=everything(),names_to="variable",values_to="availability")) %>% 
  mutate(var_group = str_extract(variable,"^[a-z]+")) %>% 
  group_by(var_group) %>% 
  arrange(desc(availability))

write_csv(hd_dataset_availability,paste0(path_pasc_cmr_folder,"/working/pcrpre403_availability high dimensional dataset.csv"))  

selected_vars <- hd_dataset_availability %>% 
  group_by(var_group) %>% 
  slice(1:200) %>% 
  ungroup()

source("functions/hdPS functions.R")

# Temporary fix:
# selected_vars <- selected_vars %>%
#   mutate(variable = str_replace(variable,"(lab|pre)_","")) 


hd_dataset <- index_date %>% 
  dplyr::select(ID,COHORT) %>% 
  left_join(ip_diagnosis %>% 
              dplyr::select(ID,one_of(selected_vars$variable)),
            by = "ID")  %>% 
  left_join(op_diagnosis %>% 
              dplyr::select(ID,one_of(selected_vars$variable)),
            by = "ID") %>% 
  left_join(ip_procedures %>% 
              dplyr::select(ID,one_of(selected_vars$variable)),
            by = "ID") %>% 
  left_join(op_procedures %>% 
              dplyr::select(ID,one_of(selected_vars$variable)),
            by = "ID") %>% 
  left_join(prescribing %>% 
              dplyr::select(ID,one_of(selected_vars$variable)),
            by = "ID") %>% 
  left_join(lab %>% 
              dplyr::select(ID,one_of(selected_vars$variable)),
            by = "ID") %>%
  mutate(across(-one_of(c("ID","COHORT")),.f=function(x) case_when(is.na(x) ~ 0,
                        TRUE ~ as.numeric(x)))) %>% 
  
  mutate(across(-one_of(c("ID","COHORT")),list(gtOne = gtOne, gtMedian = gtMedian, gtQ3 = gtQ3),
                .names="{.col}_{.fn}"))
  
saveRDS(hd_dataset,paste0(path_pasc_cmr_folder,"/working/cleaned/high dimensional dataset for analysis.RDS"))

