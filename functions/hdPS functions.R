gtOne <- function(x){
  
  x_out = case_when(x > 1 ~ 1,
            TRUE ~ 0)
  
  return(x_out)
  
}

gtMedian <- function(x){
  
  x_out = case_when(x > median(x) ~ 1,
                    TRUE ~ 0)
  
  return(x_out)
  
}

gtQ3 <- function(x){
  
  x_out = case_when(x > quantile(x,probs=0.75) ~ 1,
                    TRUE ~ 0)
  
  return(x_out)
  
}