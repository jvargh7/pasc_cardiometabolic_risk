source(paste0(path_pasc_cmr_repo,"/analysis bmi/pcrab005_matchid dataset.R"))


before_matchid <- readRDS(paste0(path_pasc_cmr_folder,"/working/models pcrab/pcrab004_imputed lookback dataset.RDS")) %>% 
  dplyr::select(-matchid) %>% 
  left_join(matchid_df %>% dplyr::select(-COHORT),
            by = "ID")

analytic_dataset = before_matchid

# Use if we want to restrict to just the exposed and historical who are matched
# analytic_dataset <- bind_rows(before_matchid %>%
#                                 dplyr::filter(COHORT == "unexposed"),
#                               before_matchid %>%
#                                 dplyr::filter(COHORT %in% c("exposed","historical"), matchid %in% before_matchid$ID))
