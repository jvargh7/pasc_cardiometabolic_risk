rm(list=ls());gc();source(".Rprofile")

readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/demographic.RDS")) %>% 
  group_by(COHORT) %>% 
  summarize_at(vars(female,age),~mean(.)) %>% 
  write_csv("paper/table_unweighted summary by cohort.csv")


lookback_df <- readRDS(paste0(path_pasc_cmr_folder,"/working/cleaned/lookback dataset for analysis.RDS"))
library(compareGroups)
compareGroups(data=lookback_df,
              formula = COHORT ~ female + nhwhite + nhblack + hispanic + nhother +
                age + site + payer_type_primary + payer_type_secondary + 
                hospitalization + p_hyperglycemia + 
                bmi + HT + SYSTOLIC + smoking + 
                obesity + cardiovascular + cerebrovascular + hypertension +
                pulmonary + hyperlipidemia + antidepressants + antipsychotics +
                antihypertensives + statins + immunosuppresants + 
                hba1c + glucose + alt + ast + serum_creatinine + hdl + ldl,
              method = c(3,3,3,3,3,
                         1, 3, 3,3,
                         3,3,
                         1,1,1,3,
                         3,3,3,3,
                         3,3,3,3,
                         3,3,3,
                         1,1,2,2,2,1,1
                         ),include.miss = TRUE) %>% 
  createTable(.,digits=1,show.all=TRUE,q.type = c(2,2),sd.type = 2,show.n = TRUE) %>% 
  export2xls(.,file="paper/table_unweighted summary characteristics by cohort.xlsx")
