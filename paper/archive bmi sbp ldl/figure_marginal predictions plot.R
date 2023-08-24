rm(list=ls());gc();source(".Rprofile")


overall <- read_csv(file="analysis/pcra301_marginal predictions at t by COHORT.csv") 
stratified <- read_csv(file="analysis/pcra302_marginal predictions at t by COHORT and modifier.csv")

label_order <- c("Historical","Unexposed","Exposed")
color_order <- c("blue","darkgreen","red")


source("functions/marginal_predictions_plot.R")

fig_df = bind_rows(overall,
                   stratified) %>% 
  rename(t = x) %>% 
  dplyr::filter(t %in% c(0,100)) %>% 
  mutate(facet = case_when(is.na(facet) ~ "Overall",
                           TRUE ~ facet),
         t = paste0("Time = ",t)) %>% 
  mutate(facet = factor(facet,levels=c("Overall",
                                       "Female",
                                       "Male",
                                       "18 to 39",
                                       "40 to 64",
                                       "65 plus",
                                       "Hispanic",
                                       "NH White",
                                       "NH Black")),
         group = factor(group,levels=c("historical","unexposed","exposed"),
                        labels=c("Historical","Unexposed","Exposed")))

(fig_bmi = fig_df %>% 
  dplyr::filter(outcome == "bmi") %>% 
    marginal_predictions_plot(.,x_lab="Body mass index (kg/m2)")) %>% 
  ggsave(.,filename=paste0(path_pasc_cmr_folder,"/figures/marginal predictions bmi.png"),width=8,height=6)

(fig_sbp = fig_df %>% 
  dplyr::filter(outcome == "sbp") %>% 
    marginal_predictions_plot(.,x_lab="Systolic Blood Pressure (mm Hg)") ) %>% 
  ggsave(.,filename=paste0(path_pasc_cmr_folder,"/figures/marginal predictions sbp.png"),width=8,height=6)

(fig_ldl = fig_df %>% 
  dplyr::filter(outcome == "ldl") %>% 
    marginal_predictions_plot(.,x_lab="LDL cholesterol (mg/dL)")) %>% 
  ggsave(.,filename=paste0(path_pasc_cmr_folder,"/figures/marginal predictions ldl.png"),width=8,height=6)


