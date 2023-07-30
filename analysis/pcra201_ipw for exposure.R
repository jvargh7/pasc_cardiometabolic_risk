rm(list=ls());gc();source(".Rprofile")
index_date <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/index date.RDS"))
prop_COHORT = prop.table(table(index_date$COHORT))


predicted_probability <- bind_cols(
  read_csv(paste0(path_pasc_cmr_folder,"/working/models/pcra201_predicted probability for exposed_min10_ntree2000.csv")),
  read_csv(paste0(path_pasc_cmr_folder,"/working/models/pcra201_predicted probability for unexposed_min10_ntree2000.csv")),
  read_csv(paste0(path_pasc_cmr_folder,"/working/models/pcra201_predicted probability for historical_min10_ntree2000.csv"))) %>% 
  dplyr::select(exposed,historical,unexposed) %>%
  bind_cols(index_date %>% 
              dplyr::select(ID,COHORT))

tx_n_p = map(1:nrow(predicted_probability),function(x){prop_COHORT[predicted_probability$COHORT[x]]}) %>% as.numeric()
tx_d_p = map(1:nrow(predicted_probability),function(x){predicted_probability[x,predicted_probability$COHORT[x]]%>% as.numeric()}) %>% as.numeric()



# predicted_probability$sipw = tx_n_p/tx_d_p
# predicted_probability$denominator = tx_d_p

# hist(predicted_probability$sipw)
# summary(predicted_probability$sipw)

source("C:/code/external/functions/causality/trim_probabilities.R")

# https://www2.stat.duke.edu/~fl35/OW.html
predicted_probability <- predicted_probability %>% 
  mutate(numerator = tx_n_p,
         denominator = tx_d_p) %>% 
  mutate(trimmed_denominator = trim_probabilities(denominator)) %>% 
  mutate(sipw = numerator/trimmed_denominator) %>% 
  mutate(overlap_weight = 1-denominator) %>% 
  group_by(COHORT) %>% 
  mutate(overlap_weight = overlap_weight/sum(overlap_weight)) %>% 
  ungroup()


fig_sipw <- predicted_probability %>% 
  ggplot(data=.,aes(x=sipw,fill=COHORT,group=COHORT)) +
  geom_density(alpha=0.3) +
  theme_bw() +
  xlab("Stabilized IPW") +
  theme(legend.position = "bottom")

ggsave(fig_sipw,file=paste0(path_pasc_cmr_folder,"/figures/distribution of SIPW.png"),width=10,height=4)


fig_ow <- predicted_probability %>% 
  ggplot(data=.,aes(x=overlap_weight,fill=COHORT,group=COHORT)) +
  geom_density(alpha=0.3) +
  theme_bw() +
  xlab("Overlap weights") +
  theme(legend.position = "bottom")

ggsave(fig_ow,file=paste0(path_pasc_cmr_folder,"/figures/distribution of Overlap Weight.png"),width=10,height=4)

# Adding demographic numerator ---------
demographic <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/demographic.RDS")) %>% 
  mutate(raceeth_category = case_when(nhwhite == 1 ~ "NH White",
                                      nhblack == 1 ~ "NH Black",
                                      hispanic == 1 ~ "Hispanic",
                                      TRUE ~ "NH Other"),
         sex_category = case_when(female == 1 ~ "Female",
                                  TRUE ~ "Male"),
         age_category = case_when(age %in% c(18:39) ~ "18 to 39",
                                  age %in% c(40:64) ~ "40 to 64",
                                  age >= 65 ~ "65 plus",
                                  TRUE ~ NA_character_))

demographic_numerator <- bind_rows(
  
  demographic %>% 
    group_by(COHORT,raceeth_category) %>% 
    tally() %>% 
    # rename(category = raceeth_category) %>% 
    mutate(prop = n/sum(n)) %>% 
    mutate(var = "raceeth"),
  demographic %>% 
    group_by(COHORT,sex_category) %>% 
    tally() %>% 
    # rename(category = sex_category) %>% 
    mutate(prop = n/sum(n)) %>% 
    mutate(var = "sex"),
  demographic %>% 
    group_by(COHORT,age_category) %>% 
    tally() %>% 
    # rename(category = age_category) %>% 
    mutate(prop = n/sum(n)) %>% 
    mutate(var = "age")
)



# Save ------------


demographic %>% 
  dplyr::select(ID,COHORT,raceeth_category,sex_category,age_category) %>% 
  left_join(demographic_numerator %>% 
              dplyr::select(COHORT,raceeth_category,prop) %>% 
              rename(raceeth_numerator = prop),
            by = c("raceeth_category","COHORT")) %>% 
  left_join(demographic_numerator %>% 
              dplyr::select(COHORT,sex_category,prop) %>% 
              rename(sex_numerator = prop),
            by = c("sex_category","COHORT")) %>% 
  left_join(demographic_numerator %>% 
              dplyr::select(COHORT,age_category,prop) %>% 
              rename(age_numerator = prop),
            by = c("age_category","COHORT")) %>% 
  right_join(predicted_probability,
             by = c("ID","COHORT")) %>% 
  mutate(sipw_raceeth = raceeth_numerator/trimmed_denominator,
         sipw_sex = sex_numerator/trimmed_denominator,
         sipw_age = age_numerator/trimmed_denominator) %>% 
  
  saveRDS(.,paste0(path_pasc_cmr_folder,"/working/cleaned/ip weights for COHORT.RDS"))

