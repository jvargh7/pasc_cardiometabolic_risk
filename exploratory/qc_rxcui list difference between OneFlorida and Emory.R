rxcui_list <- readxl::read_excel("data/PASC CMR Variable List.xlsx",sheet="medication") %>% 
  rename(drug_class = 'Drug class',
         drug_name = 'Drug name') %>% 
  dplyr::filter(drug_class %in% c("INSULIN","METFORMIN","SGLT2 INHIBITORS",
                                  "GLP1 RA","DPP4 INHIBITOR","THIAZOLIDINEDIONES",
                                  "SULFONYLUREAS","MEGLITINIDES","AGI"))

unavailable_rxcui <- read_csv(paste0(path_pasc_cmr_folder,"/working/dictionaries/diabetes_medications_20230915.csv")) %>% 
  left_join(rxcui_list,
            by = c("RXNORM_CUI"="RXCUI"))

write_csv(unavailable_rxcui,paste0(path_pasc_cmr_folder,"/working/dictionaries/diabetes_medications_20230915_JV.csv"))
