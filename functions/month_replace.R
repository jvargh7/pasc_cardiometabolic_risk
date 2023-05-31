
month_replace <- function(v){
  
  day_extract = str_extract(v,pattern = "^[0-9]+")
  year_extract = str_extract(v,pattern="[0-9]+$")
  month_extract = str_extract(v,pattern=paste0("(",paste0(str_to_upper(month.abb),collapse="|"),")")) 
  month_replace = sprintf("%02d",match(str_to_title(month_extract),month.abb))
  
  v_replace = paste0(year_extract,"-",month_replace,"-",day_extract)
  
  return(v_replace)
  
}
