encounter_check_cpit2dm <- function(cp_df){
  
  cp_encounter_check <- open_dataset(paste0(path_pasc_cmr_folder,"/working/raw/encounter_",version,".parquet")) %>% 
    dplyr::select(ID,ENCOUNTERID,FACILITY_LOCATION,ENC_TYPE,
                  ADMIT_DATE,DISCHARGE_DATE)  %>% 
    dplyr::filter(ENC_TYPE %in% permissible_enc_type) %>% 
    left_join(cp_df %>% 
                dplyr::select(ID,criterion1_date,criterion1_date_minus365,criterion1_date_minus730),
              by = c("ID")) %>% 
    dplyr::filter(ADMIT_DATE >= criterion1_date_minus730,ADMIT_DATE <= criterion1_date) %>% 
    mutate(year_before_criterion1 = case_when(ADMIT_DATE >= criterion1_date_minus365 ~ "Ym1",
                                              TRUE ~ "Ym2")) %>% 
    group_by(ID,year_before_criterion1) %>% 
    tally() %>% 
    collect() %>% 
    pivot_wider(names_from=year_before_criterion1,values_from=n)
  return(cp_encounter_check)
}
