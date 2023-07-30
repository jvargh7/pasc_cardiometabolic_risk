# Coefficient plot ------------
library(ggpubr)

contrasts_df <- read_csv("analysis/pcra301_contrasts of change in cardiometabolic indicators.csv")
coefs_df <- read_csv("analysis/pcra301_change in cardiometabolic indicators.csv")
source("functions/intercept_slope_plot.R")

bmi_coef_plot <- intercept_slope_plot(
  contrasts_df %>% dplyr::filter(outcome == "BMI"),
  coefs_df %>% dplyr::filter(outcome == "BMI")
) + 
  scale_color_discrete(name="",labels=c("Exposed","Unexposed","Historical"),type = c("red","darkgreen","blue")) +
  theme_bw() +
  xlab("Body mass index (kg/m2)") +
  ylab("") +
  geom_vline(xintercept = 0,linetype=2,col="grey20") +
  theme(legend.text = element_text(size = 12),
        axis.text = element_text(size = 12))

sbp_coef_plot <- intercept_slope_plot(
  contrasts_df %>% dplyr::filter(outcome == "SBP"),
  coefs_df %>% dplyr::filter(outcome == "SBP")
) + 
  scale_color_discrete(name="",labels=c("Exposed","Unexposed","Historical"),type = c("red","darkgreen","blue")) +
  theme_bw() +
  xlab("Systolic Blood Pressure (mm Hg)") +
  ylab("") +
  geom_vline(xintercept = 0,linetype=2,col="grey20")  +
  theme(legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_blank())

ldl_coef_plot <- intercept_slope_plot(
  contrasts_df %>% dplyr::filter(outcome == "LDL"),
  coefs_df %>% dplyr::filter(outcome == "LDL")
) + 
  scale_color_discrete(name="",labels=c("Exposed","Unexposed","Historical"),type = c("red","darkgreen","blue")) +
  theme_bw() +
  xlab("Low Density Lipoprotein (mg/dL)") +
  ylab("") +
  geom_vline(xintercept = 0,linetype=2,col="grey20")  +
  theme(legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_blank())

ggarrange(bmi_coef_plot,
          sbp_coef_plot,
          ldl_coef_plot,
          # ggplot() + theme_blank(),
          labels=c("B","C","D"),
          nrow=1,ncol=3,widths = c(1.8,1,1),common.legend = TRUE) %>% 
  ggsave(.,filename=paste0(path_pasc_cmr_folder,"/figures/marginal plot coefs for change.png"),width=10,height=4)