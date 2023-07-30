# Random Forest: Ranger -------

# https://www.tmwr.org/splitting.html
data_train = training(data_split)
data_test = testing(data_split)
# rm(bmi_df,bmi_split);gc()

df_resamples <- vfold_cv(data_train,v=5,repeats=1)

# recipe() is for how to process a single dataset/fold -----------

if(outcome == "COHORT"){
  outcome_rec <- recipe(COHORT ~ .,data=data_train)  
} else if(outcome == "in_bmi_ID"){
  outcome_rec <- recipe(in_bmi_ID ~ .,data=data_train)
} else if(outcome == "in_sbp_ID"){
  outcome_rec <- recipe(in_sbp_ID ~ .,data=data_train)
} else if(outcome == "in_ldl_ID"){
  outcome_rec <- recipe(in_ldl_ID ~ .,data=data_train)
  
}

outcome_rec <- outcome_rec %>% 
  update_role(ID,matchid,index_date, new_role="ID") %>% 
  # https://recipes.tidymodels.org/reference/step_dummy.html
  step_dummy(site,calendar_month,payer_type_primary,payer_type_secondary)


# model() https://parsnip.tidymodels.org/articles/Examples.html#multinom_reg-models ----------
rf_model = rand_forest(trees = tune(), min_n = tune())  %>% # <- main arguments,
  # The default in ranger::ranger() is mtry = floor(sqrt(ncol(x))).
  set_engine("ranger", regularization.factor = 0.5) %>%         # <- engine-specific
  set_mode(mode = "classification")


# If penalty = tune()
rf_param = rf_model %>% extract_parameter_set_dials()

# https://www.tmwr.org/resampling.html: let’s save the predictions in order to visualize the model fit and residuals

# registerDoParallel() https://www.tmwr.org/resampling.html#parallel  -------
keep_pred <- control_resamples(save_pred = TRUE, save_workflow = TRUE)

# library(doParallel)
# cl <- makePSOCKcluster(2,outfile="analysis/pcra201_out.txt")
# registerDoParallel(cl)

# workflow() ---------
ipw_tuning_workflow <- workflow() %>%
  add_model(rf_model) %>%
  add_recipe(outcome_rec) %>% 
  tune_grid(df_resamples,
            grid = crossing(trees = c(500,1000),min_n = c(100,1000)),
            # grid = crossing(trees = c(10,20),min_n = c(100,1000)),
            control = keep_pred
  )

print("Completed ipw_tuning_workflow")
# stopCluster(cl)
# collect_metrics() -------
# https://towardsdatascience.com/dials-tune-and-parsnip-tidymodels-way-to-create-and-tune-model-parameters-c97ba31d6173
# not a model but a tuning grid, so we need to finalise with the best arguments
tune_grid_metrics <- ipw_tuning_workflow %>% 
  collect_metrics()

best_tune_grid_params <-
  ipw_tuning_workflow %>%
  select_by_pct_loss(metric = "roc_auc",limit=5,desc(min_n))
# For percent loss, suppose the best model has an RMSE of 0.75 and a simpler model has an RMSE of 1. 
# The percent loss would be (1.00 - 0.75)/1.00 * 100, or 25 percent. Note that loss will always be non-negative.


# collect_predictions() ---------
tune_grid_predictions <- ipw_tuning_workflow %>% 
  collect_predictions()


# Evaluating performance ---------
# https://www.tmwr.org/performance.html
# sensitivity(tune_grid_predictions[tune_grid_predictions$id=="Fold1",], COHORT, .pred_class, estimator = "macro_weighted")
# sensitivity(tune_grid_predictions, COHORT, .pred_class, estimator = "macro_weighted")

# Evaluating test set: the last fit using best parameters ------

# https://www.tmwr.org/workflow-sets.html >> different from workflow()
best_params_wflow <- workflow() %>%
  add_model(rf_model) %>%
  add_recipe(outcome_rec) %>% 
  finalize_workflow(best_tune_grid_params)
print("Completed best_params_wflow")

set.seed(501)
assess_test_fit <-
  best_params_wflow %>%
  # https://community.rstudio.com/t/where-how-can-i-apply-last-fit-after-tuning-then-how-do-i-apply-extract-workflow-to-new-data-without-y-values/126062/3
  # last_fit() emulates the process where, after determining the best model, 
  # the final fit on the entire training set is needed and is then evaluated on the test set.
  # https://tune.tidymodels.org/reference/fit_resamples.html
  # If split is 60/40, it provides predictions only on 40
  last_fit(split = data_split)

get_test_predictions <- 
  assess_test_fit %>% 
  collect_predictions() %>% 
  as.data.frame()
print("Completed get_test_predictions")


get_train_predictions <- ipw_tuning_workflow %>% 
  collect_predictions(parameters = best_tune_grid_params)

final_model <- assess_test_fit$.workflow[[1]]

all_predictions <- bind_rows(
  augment(final_model, new_data = data_train) %>% 
    mutate(type = "train"),
  augment(final_model, new_data = data_test) %>% 
    mutate(type = "test")
) %>% 
  dplyr::select(ID,type,one_of(names(tune_grid_predictions)))
