rm(list=ls());gc();source(".Rprofile")

source("functions/month_replace.R")

death <- read_parquet(paste0(path_pasc_cmr_folder,"/working/raw/death_",version,".parquet")) %>% 
  # mutate(death_date = ymd(month_replace(DEATH_DATE))) %>%
  dplyr::select(ID,DEATH_DATE) %>%
  collect()

saveRDS(death,paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre102_death.RDS"))

death %>% 
  group_by(ID) %>% 
  dplyr::filter(n() > 1) %>% 
  summarize(min = min(DEATH_DATE),
            max = max(DEATH_DATE),
            n = sum(!is.na(DEATH_DATE))) %>%
  mutate(diff_max = difftime(max,min,units="days")) %>% 
  dplyr::filter(diff_max > 1) %>% 
  write_csv(.,paste0(path_pasc_cmr_folder,"/working/cleaned/pcrpre102_QC death multiple records.csv"))
