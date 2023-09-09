
matchid_df = bind_rows(
    readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre402_lookback dataset for analysis.RDS")) %>% 
      dplyr::filter(COHORT == "historical") %>% 
      dplyr::select(ID,COHORT) %>% 
      left_join(readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre402_lookback dataset for analysis.RDS")) %>% 
                  dplyr::filter(COHORT == "exposed") %>% 
                  dplyr::select(ID,matchid) %>% 
                  rename(historical_ID = matchid,
                         exposed_ID = ID),
                by = c("ID" = "historical_ID")) %>% 
      rename(matchid = exposed_ID),
    
    readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre402_lookback dataset for analysis.RDS")) %>% 
      dplyr::filter(COHORT != "historical") %>% 
      dplyr::select(ID,COHORT,matchid)
  ) 
