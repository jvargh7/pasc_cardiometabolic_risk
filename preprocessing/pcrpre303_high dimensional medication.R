rm(list=ls());gc();source(".Rprofile")

index_date <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/index date.RDS"))

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

lb_hd_prescribing <- open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/prescribing_",version,".parquet")) %>%
  mutate(RXNORM_CUI = as.numeric(RXNORM_CUI)) %>% 
  right_join(index_date %>% 
               dplyr::select(ID,index_date,index_date_minus365,COHORT),
             by = c("ID")) %>% 
  dplyr::filter(RX_ORDER_DATE >= index_date_minus365,RX_ORDER_DATE < index_date) %>% 
  left_join(
    read_csv(paste0(path_pasc_cmr_folder,"/working/pcrpre303_summary high dimensional prescribing.csv")) %>% 
      mutate(atc3 = stringr::str_sub(classId,1,3)),
    by = c("RXNORM_CUI")
  ) %>% 
  dplyr::filter(!is.na(atc3)) %>% 
  group_by(ID,atc3) %>% 
  tally() %>% 
  collect() %>% 
  pivot_wider(names_from="atc3",values_from="n") 

saveRDS(lb_hd_prescribing,paste0(path_pasc_cmr_folder,"/working/cleaned/lookback high dimensional prescribing.RDS"))
