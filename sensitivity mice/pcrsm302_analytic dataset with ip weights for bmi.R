# rm(list=ls());gc();source(".Rprofile")

source(paste0(path_pasc_cmr_repo,"/analysis bmi/pcrab004_matchid dataset.R"))


before_matchid <- imputed_dataset %>% 
  dplyr::select(-matchid) %>% 
  left_join(matchid_df,
            by = "ID")

tx_weights_df <- readRDS(paste0(path_pasc_cmr_folder,"/working/models pcrab/pcrab301_ip weights for cohort membership.RDS"))
mo_weights_df <- readRDS(paste0(path_pasc_cmr_folder,"/working/models pcrab/pcrab301_ip weights for missing outcomes.RDS"))

# Rename back into used variable names
analytic_dataset_lookback = before_matchid %>% 
  rename(nhblack = raceeth_category_2,
         hispanic = raceeth_category_3,
         nhother = raceeth_category_4,
         female = sex_category_2) %>% 
  mutate(site = case_when(is.na(site) ~ "Source 1",
                          TRUE ~ site))

rm(before_matchid)

bmi_df = readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre401_anthro followup.RDS")) %>% 
  dplyr::filter(!is.na(bmi),ID %in% analytic_dataset_lookback$ID) %>% 
  mutate(t = as.numeric(t)) %>% 
  left_join(tx_weights_df %>%
              dplyr::select(ID,sipw,sipw_sex,sipw_age,sipw_raceeth,sipw_hospitalization,sex_category,age_category,raceeth_category),
            by = "ID") %>%
  left_join(mo_weights_df %>%
              dplyr::select(ID,ltfu_weights),
            by = "ID") %>%
  left_join(analytic_dataset_lookback %>% 
              dplyr::select(ID,one_of(imbalanced_variables),bmi) %>% 
              rename(lb_bmi = bmi),
            by = "ID") %>% 
  mutate(w = sipw*ltfu_weights,
         w_sex = sipw_sex*ltfu_weights,
         w_age = sipw_age*ltfu_weights,
         w_raceeth = sipw_raceeth*ltfu_weights,
         w_hospitalization = sipw_hospitalization*ltfu_weights) %>%
  arrange(ID,t) %>% 
  mutate(ID = factor(ID),
         COHORT = factor(COHORT,levels=c("historical","unexposed","exposed")))
