marginal_predictions_plot <- function(df,x_lab,type = "point_ci",axis_text_y = TRUE){
  
  unique_t = unique(df$t)
  
  if(type == "point_ci"){
    plot = df %>% 
      ggplot(data=.,aes(x=predicted,xmin=conf.low,xmax=conf.high,y=facet,col=group,fill=group)) +
      geom_point(position = position_dodge(width=0.9)) +
      geom_errorbarh(position=position_dodge(width=0.9),height=0.2)
  } else if (type == "column_ci"){
    plot = df %>% 
      ggplot(data=.,aes(x=predicted,xmin=conf.low,xmax=conf.high,y=facet,fill=group,col=group)) +
      geom_col(position = position_dodge(width=0.9)) +
      geom_errorbarh(position=position_dodge(width=0.9),height=0.2,col="grey50")  
    
    
    
  }
  
  if(length(unique_t) > 1){
    plot = plot +
      facet_grid(.~t,scales="free_y")  
    
  }

  
  plot = plot +
    scale_fill_discrete(name="",labels= label_order,type = color_order) +
    scale_color_discrete(name="",labels= label_order,type = color_order) +
    scale_y_discrete(limits=rev) +
    xlab(x_lab) +
    ylab("") +
    theme_bw()   +
    theme(legend.position = "bottom")  +
    theme(axis.text.y = element_text(size=14))
    # https://stackoverflow.com/questions/22458970/how-to-reverse-legend-labels-and-color-so-high-value-starts-at-bottom
    # guides(colour = guide_legend(reverse=T))
  
  if(!axis_text_y){
    plot = plot + 
      theme(axis.text.y = element_blank())
  }
  
  return(plot)
}
