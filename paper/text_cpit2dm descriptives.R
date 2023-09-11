
rm(list=ls());gc();source(".Rprofile")

source(paste0(path_pasc_cmr_repo,"/analysis bmi/pcrab001_processing before imputation and lookback bmi exclusion.R"))


cpit2dm <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre202_cpit2dm new onset diabetes.RDS"))
lookback_cpit2dm <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre209_cpit2dm diabetes during lookback period.RDS"))
landmark_cpit2dm <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre208_cpit2dm new onset diabetes during period till origin date.RDS"))

table(cpit2dm$ID %in% c(lookback_cpit2dm$ID,landmark_cpit2dm$ID))
