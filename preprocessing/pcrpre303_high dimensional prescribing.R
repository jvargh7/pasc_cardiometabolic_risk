rm(list=ls());gc();source(".Rprofile")

index_date <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre201_index date.RDS"))

source("preprocessing/pcrpre300_encounter type.R")

# unique_prescribing <- open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/prescribing_",version,".parquet")) %>% 
#   group_by(RXNORM_CUI) %>% 
#   tally() %>% 
#   collect() %>% 
#   dplyr::filter(!is.na(RXNORM_CUI))
# 
# # library(furrr)
# # options(future.globals.maxSize= (6*1024*1024)^3) #6GB
# # # https://stackoverflow.com/questions/40536067/how-to-adjust-future-global-maxsize
# # plan(multisession, workers = 8)
# atc_output =  map_dfr(.x = unique_prescribing$RXNORM_CUI,
#       .f = function(x){
#         
#         # out =  tryCatch({
#           print(x)
#           resp = jsonlite::fromJSON(paste0("https://rxnav.nlm.nih.gov/REST/rxclass/class/byRxcui.json?rxcui=",x,"&relaSource=ATC"));
#           df = tryCatch({
#             resp$rxclassDrugInfoList$rxclassDrugInfo$rxclassMinConceptItem %>% 
#             data.frame() %>% 
#             dplyr::mutate(RXNORM_CUI = x)
#           },
#                  error = function(e){
#                    data.frame(classId = NA,
#                               className = NA,
#                               classType = NA,
#                               RXNORM_CUI = x)
#                  }) 
#           
#         
#         
#         return(df)
#         
#         
#       })
# 
# write_csv(atc_output,paste0(path_pasc_cmr_folder,"/working/pcrpre303_summary high dimensional prescribing.csv"))


# ATC-5 CODES:
# A: Alimentary tract and metabolism (anatomical main group)
# A10: Drugs used in diabetes (therapeutic subgroup)
# A10B: Blood glucose lowering drugs, excl. insulin (pharmacological subgroup)
# A10BA: Biguanides (chemical subgroup)
# A10BA02: Metformin (chemical substance)

# Missingness in index date and encounter type
missing_prescribing_encounters <- open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/prescribing_",version,".parquet"))  %>% 
  mutate(ID = as.character(ID)) %>% 
  right_join(index_date %>% 
               dplyr::select(ID,origin_date,index_date,index_date_minus365,index_date_minus730, COHORT),
             by = c("ID")) %>% 
  mutate(missing_date_type = case_when(!is.na(index_date) ~ "index date available",
                                       TRUE ~ "index date missing")) %>% 
  left_join(encounter_type,
            by=c("ID","ENCOUNTERID")) %>% 
  mutate(missing_enc_inpatient = case_when(!is.na(enc_inpatient) ~ "enc_inpatient available",
                                           TRUE ~ "enc_inpatient missing")) %>% 
  group_by(missing_date_type,missing_enc_inpatient) %>% 
  tally() %>% 
  collect()


lb_hd_prescribing <- open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/prescribing_",version,".parquet")) %>%
  mutate(RXNORM_CUI = as.numeric(RXNORM_CUI)) %>% 
  right_join(index_date %>% 
               dplyr::select(ID,origin_date,index_date,index_date_minus365,index_date_minus730, COHORT),
             by = c("ID")) %>% 
  mutate(date_type = case_when(RX_ORDER_DATE >= origin_date ~ "p4",
                               RX_ORDER_DATE >= index_date ~ "p3",
                               RX_ORDER_DATE >= index_date_minus365 ~ "p2",
                               RX_ORDER_DATE >= index_date_minus730 ~ "p1",
                               TRUE ~ NA_character_)) %>% 
  dplyr::filter(!is.na(date_type)) %>% 
  left_join(encounter_type,
            by=c("ID","ENCOUNTERID")) %>% 
  # dplyr::filter(RX_ORDER_DATE >= index_date_minus365,RX_ORDER_DATE < index_date) %>% 
  left_join(
    read_csv(paste0(path_pasc_cmr_folder,"/working/pcrpre303_summary high dimensional prescribing.csv")) %>% 
      mutate(atc3 = stringr::str_sub(classId,1,3)),
    by = c("RXNORM_CUI")
  ) %>% 
  dplyr::filter(!is.na(atc3)) %>% 
  group_by(ID,enc_inpatient,date_type,atc3) %>% 
  tally() %>% 
  collect() %>% 
  pivot_wider(names_from="atc3",values_from="n")  %>% 
  dplyr::filter(ID %in% included_patients$ID)

saveRDS(lb_hd_prescribing,paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre303_high dimensional prescribing.RDS"))
