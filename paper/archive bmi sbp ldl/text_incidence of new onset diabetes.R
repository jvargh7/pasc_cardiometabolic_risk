rm(list=ls());gc();source(".Rprofile")
cpit2dm <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/cpit2dm new onset diabetes.RDS"))
noncpit2dm <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/noncpit2dm last followup.RDS"))

index_date <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/index date.RDS"))

followup_df <- index_date %>% 
  ungroup() %>% 
  left_join(cpit2dm %>% 
              dplyr::select(ID,criterion1_date,criterion2_date,CP),
            by = "ID") %>% 
  left_join(noncpit2dm,
            by = "ID") %>% 
  mutate(max_followup_t = case_when(!is.na(criterion2_date) ~ (criterion2_date - origin_date),
                                    TRUE ~ (last_followup_date - origin_date))) %>% 
  mutate(max_followup_t = as.numeric(max_followup_t)) %>% 
  dplyr::filter(!is.na(max_followup_t))


followup_df %>% 
  mutate(case = case_when(!is.na(criterion2_date) ~ 1,
                          TRUE ~ 0)) %>% 
  group_by(COHORT) %>%
  summarize(case_count = sum(case),
            time_count = sum(max_followup_t),
            median_followup_t = median(max_followup_t)) %>% 
  mutate(case_per_100py = case_count/(time_count/(100*365)),
         time_per_100py = time_count/(100*365),
         )
  

followup_df %>% 
  mutate(case = case_when(!is.na(criterion2_date) ~ 1,
                          TRUE ~ 0)) %>% 
  group_by(COHORT, case) %>%
  summarize(case_count = sum(case),
            time_count = sum(max_followup_t),
            median_followup_t = median(max_followup_t)) %>% 
  mutate(case_per_100py = case_count/(time_count/(100*365)),
         time_per_100py = time_count/(100*365),
  )

followup_df %>% 
  mutate(case = case_when(!is.na(criterion2_date) ~ 1,
                          TRUE ~ 0)) %>% 
  group_by(COHORT, CP) %>%
  summarize(case_count = sum(case),
            time_count = sum(max_followup_t),
            median_followup_t = median(max_followup_t)) %>% 
  mutate(case_per_100py = case_count/(time_count/(100*365)),
         time_per_100py = time_count/(100*365),
  )

followup_df %>% 
  mutate(case = case_when(!is.na(criterion2_date) ~ 1,
                          TRUE ~ 0)) %>% 
  group_by(CP) %>%
  summarize(case_count = sum(case),
            time_count = sum(max_followup_t),
            median_followup_t = median(max_followup_t)) %>% 
  mutate(case_per_100py = case_count/(time_count/(100*365)),
         time_per_100py = time_count/(100*365),
  )
