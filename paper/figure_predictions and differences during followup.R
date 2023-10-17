rm(list=ls());gc();source(".Rprofile")


predictions <- bind_rows(read_csv(file="analysis bmi/pcrab401_marginal predictions at t by COHORT.csv"),
                         read_csv(file="analysis bmi/pcrab402_marginal predictions at t by COHORT and modifier.csv")) %>% 
  rename(t = x) %>% 
  dplyr::filter(t %in% c(0)) %>% 
  mutate(facet = case_when(
                           facet == "hospitalization = 0" ~ "Not Hospitalized",
                           facet == "hospitalization = 1" ~ "Hospitalized",
                           is.na(facet) ~ "Overall",
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
                                       "NH Black",
                                       "Not Hospitalized",
                                       "Hospitalized")),
         group = factor(group,levels=c("historical","unexposed","exposed"),
                        labels=c("Historical","Unexposed","Exposed")))

difference100 <- read_csv("analysis bmi/pcrab404_difference between 0 and 100 within sociodemographic.csv") %>% 
  dplyr::filter(modifier2_value %in% c(100)) %>% 
  rename(t = modifier2_value) %>% 
  mutate(group = str_replace(exposure,"COHORT",""),
         facet = str_replace(modifier1,modifier_var,"")) %>% 
  mutate(facet = case_when(is.na(modifier1) & modifier_var == "hospitalization_category" ~ "Not Hospitalized",
                           facet == "hospitalization" ~ "Hospitalized",
                           is.na(facet) ~ "Overall",
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
                                       "NH Black",
                                       "Not Hospitalized",
                                       "Hospitalized")),
         group = factor(group,levels=c("historical","unexposed","exposed"),
                        labels=c("Historical","Unexposed","Exposed"))) %>% 
  rename(predicted = Estimate,
         conf.low = LCI,
         conf.high = UCI)


did100 <- read_csv("analysis bmi/pcrab405_difference relative to exposed for difference between 0 and 100.csv") %>% 
  dplyr::filter(modifier2_value %in% c(100)) %>% 
  rename(t = modifier2_value) %>% 
  mutate(group = str_replace(exposure,"COHORT",""),
         facet = str_replace(modifier1,modifier_var,"")) %>% 
  mutate(facet = case_when(is.na(modifier1) & modifier_var == "hospitalization_category" ~ "Not Hospitalized",
                           facet == "hospitalization" ~ "Hospitalized",
                           is.na(facet) ~ "Overall",
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
                                       "NH Black",
                                       "Not Hospitalized",
                                       "Hospitalized")),
         group = factor(group,levels=c("historical","unexposed","exposed"),
                        labels=c("Historical","Unexposed","Exposed"))) %>% 
  rename(predicted = Estimate,
         conf.low = LCI,
         conf.high = UCI) 

did0 <- read_csv("analysis bmi/pcrab406_difference relative to exposed for time 0.csv") %>% 
  dplyr::filter(modifier2_value %in% c(0)) %>% 
  rename(t = modifier2_value) %>% 
  mutate(group = str_replace(exposure,"COHORT",""),
         facet = str_replace(modifier1,modifier_var,"")) %>% 
  mutate(facet = case_when(is.na(modifier1) & modifier_var == "hospitalization_category" ~ "Not Hospitalized",
                           facet == "hospitalization" ~ "Hospitalized",
                           is.na(facet) ~ "Overall",
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
                                       "NH Black",
                                       "Not Hospitalized",
                                       "Hospitalized")),
         group = factor(group,levels=c("historical","unexposed","exposed"),
                        labels=c("Historical","Unexposed","Exposed"))) %>% 
  rename(predicted = Estimate,
         conf.low = LCI,
         conf.high = UCI) 


source("functions/marginal_predictions_plot.R")
label_order <- c("Historical","Unexposed","Exposed")
color_order <- c("blue","darkgreen","red")

(fig_bmi_A = predictions %>% 
    dplyr::filter(outcome == "bmi") %>% 
    marginal_predictions_plot(.,x_lab="Estimated values at T= 0\nBody mass index (kg/m2)",type="column_ci") +
    coord_cartesian(xlim = c(26, 32))) 

(fig_bmi_B = difference100 %>% 
    dplyr::filter(outcome == "bmi") %>% 
    marginal_predictions_plot(.,x_lab="Difference from T = 0 at T = 100\nBody mass index (kg/m2)",type="point_ci",axis_text_y=FALSE) +
    coord_cartesian(xlim = c(-0.25, 0.25)) +
    geom_vline(xintercept=0,col="red",linetype=2))

(fig_bmi_C = did100 %>% 
    dplyr::filter(outcome == "bmi") %>% 
    marginal_predictions_plot(.,x_lab="Difference in Difference relative to Exposed\nBody mass index (kg/m2)",type="point_ci",axis_text_y=FALSE) +
    coord_cartesian(xlim = c(-0.15, 0.15)) +
    geom_vline(xintercept=0,col="red",linetype=2))


library(ggpubr)
ggarrange(fig_bmi_A,
          fig_bmi_B,
          fig_bmi_C,
          nrow=1,ncol=3,
          labels=LETTERS[1:3],
          widths = c(2.2,1.5,1.5),
          common.legend=TRUE) %>% 
  ggsave(.,filename=paste0(path_pasc_cmr_folder,"/figures/predictions and differences during followup bmi.jpg"),width=11,height=8)




# bind_rows(predictions ,
#           difference100,
#           did100 %>% mutate(t = str_replace(t, "Time = ","DID = "))) %>% 
#   dplyr::filter(facet == "Overall") %>% 
#   mutate(conf_ci = case_when(t %in% c("Time = 100","DID = 100") ~ paste0(round(predicted,2)," (",
#                                                                          round(conf.low,2),", ",
#                                                                          round(conf.high,2),")"),
#                              TRUE ~ paste0(round(predicted,1)," (",
#                                            round(conf.low,1),", ",
#                                            round(conf.high,1),")"))) %>% 
#   dplyr::select(t,group,outcome,conf_ci) %>% 
#   pivot_wider(names_from=group,values_from=conf_ci) %>% 
#   dplyr::select(t,outcome,Exposed,Unexposed,Historical) %>% 
#   arrange(outcome) %>% 
#   write_csv(.,file="paper/table_predictions and differences during followup.csv")

bind_rows(predictions ,
          did0 %>% mutate(t = str_replace(t, "Time = ","BaselineDID = ")) ,
          difference100,
          did100 %>% mutate(t = str_replace(t, "Time = ","DID = "))) %>% 
  mutate(conf_ci = case_when(t %in% c("Time = 100","DID = 100","BaselineDID = 0") ~ paste0(round(predicted,2)," (",
                                                                         round(conf.low,2),", ",
                                                                         round(conf.high,2),")"),
                             TRUE ~ paste0(round(predicted,1)," (",
                                           round(conf.low,1),", ",
                                           round(conf.high,1),")"))) %>% 
  # dplyr::filter(facet == "Overall") %>% 
  dplyr::select(t,facet,group,outcome,conf_ci) %>% 
  pivot_wider(names_from=group,values_from=conf_ci) %>% 
  dplyr::select(t,facet,outcome,Exposed,Unexposed,Historical) %>% 
  arrange(facet) %>% 
  write_csv(.,file="paper/table_predictions and differences during followup.csv")
