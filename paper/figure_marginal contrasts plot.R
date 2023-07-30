rm(list=ls());gc();source(".Rprofile")

overall <- read_csv(file="analysis/pcra301_contrasts of change in cardiometabolic indicators.csv") 
stratified <- read_csv(file="analysis/pcra303_contrasts of change in cardiometabolic indicators sociodemographic.csv")

label_order <- c("Historical","Unexposed","Exposed")
color_order <- c("blue","darkgreen","red")

fig_df = bind_rows(overall,
                   stratified) %>% 
  mutate(t = case_when(is.na(modifier2_value) ~ modifier_value,
                       TRUE ~ modifier2_value)) %>% 
  mutate(group = str_replace(exposure,"COHORT",""),
         facet = str_replace(modifier1,modifier_var,"")) %>% 
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
                        labels=c("Historical","Unexposed","Exposed"))) %>% 
  rename(predicted = Estimate,
         conf.low = LCI,
         conf.high = UCI)

source("functions/marginal_predictions_plot.R")

(fig_bmi = fig_df %>% 
    dplyr::filter(outcome == "bmi") %>% 
    marginal_predictions_plot(.,x_lab="Change from Historical T= 0\nBody mass index (kg/m2)") +
    geom_vline(xintercept = 0,linetype = 2, col ="red")) %>% 
  ggsave(.,filename=paste0(path_pasc_cmr_folder,"/figures/marginal contrasts bmi.png"),width=8,height=6)

(fig_sbp = fig_df %>% 
    dplyr::filter(outcome == "sbp") %>% 
    marginal_predictions_plot(.,x_lab="Change from Historical T= 0\nSystolic Blood Pressure (mm Hg)") +
    geom_vline(xintercept = 0,linetype = 2, col ="red")) %>% 
  ggsave(.,filename=paste0(path_pasc_cmr_folder,"/figures/marginal contrasts sbp.png"),width=8,height=6)

(fig_ldl = fig_df %>% 
    dplyr::filter(outcome == "ldl") %>% 
    marginal_predictions_plot(.,x_lab="Change from Historical T= 0\nLDL cholesterol (mg/dL)") +
    geom_vline(xintercept = 0,linetype = 2, col ="red")) %>% 
  ggsave(.,filename=paste0(path_pasc_cmr_folder,"/figures/marginal contrasts ldl.png"),width=8,height=6)

