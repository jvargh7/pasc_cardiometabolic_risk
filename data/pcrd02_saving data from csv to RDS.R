
# Used on 26 May to save .RDS files

folder_names = c(
                 # "demo_other","diagnosis",
                 # "lab",
                 "med_admin","obsclin","prescribing","px_vital")


map(folder_names,
    function(f_n){
      
      unzip_dir = paste0(path_pasc_proposal_folder,"/working/source/",f_n,"_",version)
      
      unzipped_files <- list.files(unzip_dir,pattern="\\.csv$",full.names = TRUE)
      
      paper_dir <- paste0(path_pasc_cmr_folder,"/working/raw/RDS/")
      if(!dir.exists(paper_dir)){
        dir.create(paper_dir)
      }
      
      map(unzipped_files,
          function(u_f){
            u_f_name = str_extract(u_f,pattern = "[A-Z0-9a-z_]+.csv") %>% 
              str_replace("\\.csv","")
            
            read_csv(u_f) %>% 
              saveRDS(.,paste0(paper_dir,"/",u_f_name,".RDS"))
            
          })
      
    })


rm(list=ls());gc();source(".Rprofile")
data.table::fread(file=paste0(path_pasc_proposal_folder,"/working/source/lab_",version,"/","lab_",version,".csv"),
                  select = c("ID","ENCOUNTERID","LAB_ORDER_DATE","LAB_LOINC","SPECIMEN_DATE","RAW_LAB_NAME","RAW_RESULT","RAW_UNIT")) %>% 
  saveRDS(.,paste0(path_pasc_cmr_folder,"/working/raw/RDS/lab_RAW_",version,".RDS"))


rm(list=ls());gc();source(".Rprofile")
data.table::fread(file=paste0(path_pasc_proposal_folder,"/working/source/lab_",version,"/","lab_",version,".csv"),
                   select = c("ID","ENCOUNTERID","LAB_ORDER_DATE","LAB_LOINC","SPECIMEN_DATE","RESULT_QUAL","RESULT_SNOMED","RESULT_NUM","RESULT_MODIFIER","RESULT_UNIT")) %>% 
saveRDS(.,paste0(path_pasc_cmr_folder,"/working/raw/RDS/lab_RESULT_",version,".RDS"))

rm(list=ls());gc();source(".Rprofile")
data.table::fread(file=paste0(path_pasc_proposal_folder,"/working/source/diagnosis_",version,"/","diagnosis_",version,".csv")) %>% 
  saveRDS(.,paste0(path_pasc_cmr_folder,"/working/raw/RDS/diagnosis_",version,".RDS"))
