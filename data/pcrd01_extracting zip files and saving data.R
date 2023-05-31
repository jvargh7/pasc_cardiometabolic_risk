# this script is incomplete ------- DO NOT RUN -----------

list_zip_files <- list.files(paste0(path_pasc_proposal_folder,"/working/source"),pattern="\\.zip$",full.names = TRUE,recursive = TRUE)

map(list_zip_files,
    function(z_f){
      f_name = str_extract(z_f,pattern = "[A-Z0-9a-z_]+.zip") %>% 
        str_replace(.,"_chakkalakal_v[0-9]+\\.zip","")
      
      v_name = str_extract(z_f,pattern = "[A-Z0-9a-z_]+.zip") %>% 
        str_replace_all(.,"([0-9a-z_]+_chakkalakal_|\\.zip)","")
      
      unzip_dir = tempdir()
      unzip(z_f,exdir = unzip_dir)
      
      paper_dir <- paste0(path_pasc_cmr_folder,"/working/raw/")
      if(!dir.exists(paper_dir)){
        dir.create(paper_dir)
      }
      
      unzipped_files <- list.files(unzip_dir,pattern="\\.csv$",full.names = TRUE)
      map(unzipped_files,
          function(u_f){
            u_f_name = str_extract(z_f,pattern = "[A-Z0-9a-z_]+.csv") 
            
            read_csv(u_f) %>% 
              saveRDS(paste0(paper_dir,"/",u_f_name,".csv"))
            
          })
      
      
    })
