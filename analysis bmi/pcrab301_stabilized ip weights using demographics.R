rm(list=ls());gc();source(".Rprofile")
# index_date <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/index date.RDS"))
lookback_processed <- readRDS(paste0(path_pasc_cmr_folder,"/working/models pcrab/pcrab002_imputed lookback dataset.RDS"))
prop_COHORT = prop.table(table(lookback_processed$COHORT))


predicted_probability <- bind_cols(
  read_csv(paste0(path_pasc_cmr_folder,"/working/models pcrab/pcrab106_predicted probability for exposed_min10_ntree2000.csv")),
  read_csv(paste0(path_pasc_cmr_folder,"/working/models pcrab/pcrab106_predicted probability for unexposed_min10_ntree2000.csv")),
  read_csv(paste0(path_pasc_cmr_folder,"/working/models pcrab/pcrab106_predicted probability for historical_min10_ntree2000.csv"))) %>% 
  dplyr::select(exposed,historical,unexposed) %>%
  bind_cols(lookback_processed %>% 
              dplyr::select(ID,COHORT))

tx_n_p = map(1:nrow(predicted_probability),function(x){prop_COHORT[predicted_probability$COHORT[x]]}) %>% as.numeric()

tx_d_p = predicted_probability %>% 
  mutate(w = case_when(COHORT == "exposed" ~ exposed,
                       COHORT == "unexposed" ~ unexposed,
                       COHORT == "historical" ~ historical)) %>% 
  dplyr::select(w) %>% 
  pull()


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
  xlab("Stabilized IPW (BMI)") +
  theme(legend.position = "bottom")

ggsave(fig_sipw,file=paste0(path_pasc_cmr_folder,"/figures/distribution of SIPW.png"),width=10,height=4)


fig_ow <- predicted_probability %>% 
  ggplot(data=.,aes(x=overlap_weight,fill=COHORT,group=COHORT)) +
  geom_density(alpha=0.3) +
  theme_bw() +
  xlab("Overlap weights (BMI)") +
  theme(legend.position = "bottom")

ggsave(fig_ow,file=paste0(path_pasc_cmr_folder,"/figures/distribution of Overlap Weight.png"),width=10,height=4)


# PS overlap across cohort groups ------

(fig_ps_overlap <- predicted_probability %>% 
   dplyr::select(ID,COHORT,exposed,unexposed,historical) %>% 
   pivot_longer(cols=c("exposed","unexposed","historical"),names_to="estimator",values_to = "prob") %>% 
   mutate(across(one_of(c("estimator","COHORT")),~factor(.,levels=c("exposed","unexposed","historical"),labels=c("Exposed","Unexposed","Historical")))) %>% 
   ggplot(data=.,aes(x=COHORT,y=prob)) +
   geom_boxplot() +
   facet_grid(~estimator) +
   theme_bw() +
   xlab("Exposure Group") +
   ylab("Probability of being assigned to facet group (BMI)"))

ggsave(fig_ps_overlap,file=paste0(path_pasc_cmr_folder,"/figures/probability of being assigned to facet group bmi sample.png"),width=8,height=4)









# Adding demographic numerator ---------
demographic <- lookback_processed %>% 
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
    mutate(var = "age"),
  demographic %>% 
    group_by(COHORT,hospitalization) %>% 
    tally() %>% 
    mutate(prop = n/sum(n)) %>% 
    mutate(var = "hospitalization")
)



# Save ------------


demographic %>% 
  dplyr::select(ID,COHORT,raceeth_category,sex_category,age_category,hospitalization) %>% 
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
  left_join(demographic_numerator %>% 
              dplyr::select(COHORT,hospitalization,prop) %>% 
              rename(hospitalization_numerator = prop),
            by = c("hospitalization","COHORT")) %>% 
  right_join(predicted_probability,
             by = c("ID","COHORT")) %>% 
  mutate(sipw_raceeth = raceeth_numerator/trimmed_denominator,
         sipw_sex = sex_numerator/trimmed_denominator,
         sipw_age = age_numerator/trimmed_denominator,
         sipw_hospitalization= hospitalization_numerator/trimmed_denominator) %>% 
  
  saveRDS(.,paste0(path_pasc_cmr_folder,"/working/models pcrab/pcrab301_ip weights for cohort membership.RDS"))


# LOSS TO FOLLOW-UP -------------
source("analysis bmi/pcrab003_analytic dataset for data availability.R")
source("C:/code/external/functions/causality/trim_probabilities.R")

lookback_processed <- readRDS(paste0(path_pasc_cmr_folder,"/working/models pcrab/pcrab002_imputed lookback dataset.RDS")) %>% 
  # analytic_sample is coming from pcrab003_analytic dataset for data availability.R
  left_join(analytic_sample %>% 
              dplyr::select(ID, in_bmi_followup_ID),
            by = "ID")

predicted_probability <-read_csv(paste0(path_pasc_cmr_folder,"/working/models pcrab/pcrab206_predicted probability for loss to followup_min10_ntree2000.csv")) %>% 
  dplyr::select(available,missing) %>%
  bind_cols(lookback_processed %>% 
              dplyr::select(ID,COHORT)) %>% 
  mutate(across(contains("available"),~trim_probabilities(.),.names = "{.col}_trimmed")) %>%  
  
  mutate(ltfu_weights = 1/available_trimmed)


saveRDS(predicted_probability, paste0(path_pasc_cmr_folder,"/working/models pcrab/pcrab301_ip weights for missing outcomes.RDS"))

