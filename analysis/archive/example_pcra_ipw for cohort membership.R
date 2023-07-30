rm(list=ls());gc();source(".Rprofile")

source("analysis/pcra_analytic dataset for data availability.R")
rm(anthro_followup,demographic,index_date,lab_followup); gc()

source("analysis/pcra_selecting high dimensional variables based on fdr.R")

lookback_processed <- readRDS(paste0(path_pasc_cmr_folder,"/working/models/pcra102_imputed lookback dataset.RDS"))

hd_dataset_bmi <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/high dimensional dataset for analysis.RDS")) %>% 
  dplyr::select(ID,ends_with("gtOne"),ends_with("gtMedian"),ends_with("gtQ3")) %>% 
  rename_all(~str_replace(.,"\\-","_")) %>% 
  dplyr::select(ID,one_of(selected_hdvars %>% dplyr::filter(outcome == "bmi") %>% dplyr::select(variable) %>% pull()))

bmi_df <- lookback_processed %>% 
  dplyr::filter(ID %in% bmi_ID) %>% 
  left_join(hd_dataset_bmi,
            by = "ID")

rm(hd_dataset_bmi,lookback_processed); gc()

# Multinomial penalized regression -------
library(tidymodels)
tidymodels_prefer()
set.seed(501)
# https://www.tmwr.org/splitting.html
bmi_split = initial_split(bmi_df,prop=0.5,strata = COHORT)
bmi_train = training(bmi_split)
bmi_test = testing(bmi_split)
rm(bmi_df,bmi_split);gc()

bmi_df_resamples <- vfold_cv(bmi_train,v=5,repeats=1)

# recipe() is for how to process a single dataset/fold -----------
bmi_rec <- recipe(COHORT ~ .,data=bmi_train)  %>% 
  update_role(ID,matchid,index_date, new_role="ID") %>% 
  # https://recipes.tidymodels.org/reference/step_dummy.html
  step_dummy(site,calendar_month,payer_type_primary,payer_type_secondary)

# model() https://parsnip.tidymodels.org/articles/Examples.html#multinom_reg-models ----------
# https://parsnip.tidymodels.org/articles/Examples.html#rand_forest-models
# Each tuning parameter argument has a corresponding function in the dials package. 

mr_model = multinom_reg(penalty = tune()) %>%
  set_engine("glmnet")

# If penalty = tune()
mr_param = mr_model %>% extract_parameter_set_dials()

# https://www.tmwr.org/resampling.html: let’s save the predictions in order to visualize the model fit and residuals
keep_pred <- control_resamples(save_pred = TRUE, save_workflow = TRUE)

# registerDoParallel() https://www.tmwr.org/resampling.html#parallel  -------
# All operating systems
# Create a cluster object and then register: 
# Parallel processing with the tune package tends to provide linear speed-ups for the first few cores. 
# This means that, with two cores, the computations are twice as fast. 
# Depending on the data and model type, the linear speed-up deteriorates after four to five cores. 
# Using more cores will still reduce the time it takes to complete the task; 
# there are just diminishing returns for the additional cores.
library(doParallel)
cl <- makePSOCKcluster(5)
registerDoParallel(cl)

# workflow() ---------
ipw_workflow <- workflow() %>%
  add_model(mr_model) %>%
  # https://www.tmwr.org/grid-search.html 
  # feature engineering recipe with the model
  add_recipe(bmi_rec) %>% 
  #  add_formula() recipe() defines the formula - so no need for ----
# add_formula(COHORT ~ .-ID-matchid) %>%
# tune_grid identifies the best combination of hyperparameters 
tune_grid(bmi_df_resamples,
          ## Only if penalty = tune() and mr_param are extracted
          grid = crossing(penalty = c(0.001,0.1,1.0))
)
# Use fit() instead if it's just one dataset
# https://community.rstudio.com/t/tidymodels-fastest-way-to-extract-a-model-object-from-fit-resamples-results/73301
# >> There are no fitted models in this output.
# >> The important thing to realize about fit_resamples() is that its purpose is to measure performance. 
# >> The models that you train in fit_resamples() are not kept or used later.
# fit_resamples() computes a set of performance metrics across one or more resamples. 
# It does not perform any tuning (see tune_grid() and tune_bayes() for that), and 
# is instead used for fitting a single model+recipe or model+formula combination across many resamples.
# fit_resamples(bmi_df_resamples, control = keep_pred)
stopCluster(cl)
# collect_metrics() -------
# https://www.tmwr.org/resampling.html: These are the resampling estimates averaged over the individual replicates.
collect_metrics(ipw_workflow)

# collect_predictions() ---------
# The prediction column names follow the conventions discussed for parsnip models in Chapter 6, 
# for consistency and ease of use. The observed outcome column always uses the original column name from the source data. 
# The .row column is an integer that matches the row of the original training set so that these results can be properly arranged and joined with the original data.
assess_res <- collect_predictions(ipw_workflow)

predict(lm_fit, new_data = ames_test %>% select(-Sale_Price))

# For some resampling methods, such as the bootstrap or repeated cross-validation, 
# there will be multiple predictions per row of the original training set.
# To obtain summarized values (averages of the replicate predictions) use collect_predictions(object, summarize = TRUE).

# 15.3 Tuning and evaluating models https://www.tmwr.org/workflow-sets.html
# There are a few convenience functions for examining results such as grid_results. 
# The rank_results() function will order the models by some performance metric.
# An option, called select_best, can be used to rank the models using their best tuning parameter combination.

