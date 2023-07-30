# Used for AHA -- using analysis/archive/pcra303_covariate adjustment for change ....

intercept_slope_plot <- function(contrasts_df,coefs_df){
  
  
  
  is_plot_df = contrasts_df %>% 
    dplyr::filter(iv %in% c("Contrast 1","Contrast 2")) %>% 
    mutate(contrast = case_when(iv == "Contrast 1" ~ "Difference at Baseline",
                                iv == "Contrast 2" ~ paste0("Difference at ",modifier_value," days \nfollow-up"))) %>% 
    dplyr::select(contrast, theta_D,lci,uci,outcome,exposure) %>% 
    bind_rows(
      coefs_df %>% 
        dplyr::filter(iv == "t") %>% 
        mutate(exposure = "Exposed",
               contrast = paste0("Difference at ",unique(contrasts_df$modifier_value)," days \nfollow-up"),
               theta_D = theta_D * unique(contrasts_df$modifier_value),
               lci = lci*unique(contrasts_df$modifier_value),
               uci = uci*unique(contrasts_df$modifier_value)
               ) %>% 
        dplyr::select(exposure, contrast, theta_D, lci, uci, outcome)
        
    ) %>% 
    bind_rows(
      data.frame(
        exposure = "Exposed",
        contrast = "Difference at Baseline",
        theta_D = 0,
        lci = NA_real_,
        uci = NA_real_,
        outcome = unique(contrasts_df$outcome))
      
    ) %>% 
    mutate(exposure = factor(exposure,levels=c("Exposed","Unexposed","Historical")))
  
  is_plot <- is_plot_df %>% 
    ggplot(data=.,aes(x=theta_D,xmin=lci,xmax=uci,y=contrast,group=exposure,color=exposure)) +
    geom_point(position = position_dodge(width=0.9)) +
    geom_errorbarh(position = position_dodge(width=0.9),height=0.3)
  
  return(is_plot)
  
  
  
}
  