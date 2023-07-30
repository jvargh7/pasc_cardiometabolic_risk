followup_summary <- function(vec){
  
  paste0(round(median(vec),1)," [",
         round(quantile(vec,0.25),1),", ",
         round(quantile(vec,0.75),1),"]") %>% 
    return(.)
  
  
}
