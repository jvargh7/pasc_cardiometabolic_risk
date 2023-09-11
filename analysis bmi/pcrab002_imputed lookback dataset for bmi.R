rm(list=ls());gc();source(".Rprofile")
library(tidymodels)
tidymodels_prefer()

source(paste0(path_pasc_cmr_repo,"/analysis bmi/pcrab001_processing before imputation and lookback bmi exclusion.R"))
lookback_cpit2dm <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre209_cpit2dm diabetes during lookback period.RDS"))

lookback_df <- lookback_df %>% 
  dplyr::filter(!is.na(bmi), !ID %in% lookback_cpit2dm$ID)

lookback_processed <- recipe(COHORT ~ .,
                             data = lookback_df) %>% 
  
  update_role(ID,matchid,index_date, new_role="ID") %>% 
  # https://recipes.tidymodels.org/reference/step_impute_knn.html
  # step_impute_knn creates a specification of a recipe step that will impute missing data using nearest neighbors.
  # step_impute_knn(all_predictors(),neighbors = 5) %>% 
  step_impute_mode(all_nominal()) %>%
  # https://github.com/tidymodels/recipes/issues/756 
  # There might be scenarios where it is more reasonable to impute missing values per group/subsample.
  step_impute_linear(all_numeric(),impute_with = imp_vars(one_of(c("female","nhblack","hispanic","site","age","COHORT")))) %>%
  # prep(): For a recipe with at least one preprocessing operation, estimate the required parameters from a training set that can be later applied to other data sets.
  prep(.,training = lookback_df) %>% 
  # bake(): For a recipe with at least one preprocessing operation that has been trained by prep(), apply the computations to new data.
  bake(.,new_data=lookback_df) %>% 
  mutate(EXPOSED = case_when(COHORT == "exposed" ~ 1,
                             TRUE ~ 0),
         UNEXPOSED = case_when(COHORT == "unexposed" ~ 1,
                               TRUE ~ 0),
         HISTORICAL = case_when(COHORT == "historical" ~ 1,
                                TRUE ~ 0)) 


saveRDS(lookback_processed,paste0(path_pasc_cmr_folder,"/working/models pcrab/pcrab002_imputed lookback dataset.RDS"))
