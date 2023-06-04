rm(list=ls());gc();source(".Rprofile")


lookback_df <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/lookback dataset for analysis.RDS"))

library(mice)

id_vars = c("ID","matchid")
excluded_vars = c("nhwhite")

lookback_df %>% 
  dplyr::select(-one_of(c(id_vars,excluded_vars))) %>% 
  mutate(any_missing = rowSums(is.na(.))) %>% 
  summarize(prop = mean(any_missing>0))

mi_null <- mice(lookback_df,
                maxit = 0)

method = mi_null$method
pred = mi_null$predictorMatrix

pred[c(id_vars,excluded_vars),] <- 0
pred[,c(id_vars,excluded_vars)] <- 0
method[c(id_vars,excluded_vars)] <- ""

# Takes ~4h
mi_dfs <- mice(lookback_df,
               method = method,
               pred = pred,
               m=5,maxit=30,seed=500)

saveRDS(mi_dfs, paste0(path_pasc_cmr_folder,"/working/cleaned/lookback mi_dfs.RDS"))
