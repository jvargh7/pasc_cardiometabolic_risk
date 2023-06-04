rm(list=ls());gc();source(".Rprofile")

# Used on 31 May to save .parquet files
source("functions/month_replace.R")

folder_names = c(
  "demo_other",
  "diagnosis",
  "lab",
  "med_admin","obsclin","prescribing",
  "px_vital")


map(folder_names,
    function(f_n){
      
      unzip_dir = paste0(path_pasc_proposal_folder,"/working/source/",f_n,"_",version)
      
      unzipped_files <- list.files(unzip_dir,pattern="\\.csv$",full.names = TRUE)
      
      paper_dir <- paste0(path_pasc_cmr_folder,"/working/raw/CSV 100rows/")
      if(!dir.exists(paper_dir)){
        dir.create(paper_dir)
      }
      
      map(unzipped_files,
          function(u_f){
            u_f_name = str_extract(u_f,pattern = "[A-Z0-9a-z_]+.csv") %>% 
              str_replace("\\.csv","")
            
            # https://hbs-rcs.github.io/large_data_in_R/
            read_csv(u_f,n_max = 100) %>% 
              write_csv(.,paste0(paper_dir,"/",u_f_name,"_100rows.csv"))
            
          })
      
    })



map(folder_names,
    function(f_n){
      
      unzip_dir = paste0(path_pasc_proposal_folder,"/working/source/",f_n,"_",version)
      
      unzipped_files <- list.files(unzip_dir,pattern="\\.csv$",full.names = TRUE)
      
      paper_dir <- paste0(path_pasc_cmr_folder,"/working/raw/")
      if(!dir.exists(paper_dir)){
        dir.create(paper_dir)
      }
      
      map(unzipped_files,
          function(u_f){
            
            u_f_name = str_extract(u_f,pattern = "[A-Z0-9a-z_]+.csv") %>% 
              str_replace("\\.csv","")
            
            # https://hbs-rcs.github.io/large_data_in_R/
            # https://www.christophenicault.com/post/large_dataframe_arrow_duckdb/ >> Size of different file formats
            # If not using open_dataset but using read_csv
            df = data.table::fread(u_f) %>% 
              mutate(across(ends_with("_DATE"), ~ymd(month_replace(.))))
            write_parquet(df,paste0(paper_dir,"/",u_f_name,".parquet"))
            
            gc();
          })
      
    })

