lrt_est_censoring <- readRDS(paste0(path_pasc_cmr_folder,"/working/models pcrab/pcrab201_additional high dimensional covariates with loss to followup.RDS")) %>% 
  mutate(across(starts_with("pval"),.f=list(padj = ~p.adjust(.,method="BH")))) %>% 
  mutate(var_group = str_extract(variable,"^(ipd|opd|ipp|opp|pre|lab)")) %>% 
  mutate(var_group = case_when(str_detect(variable,"LOINC") ~ "lab",
                               is.na(var_group) ~ "pre",
                               TRUE ~ var_group))

selected_hdvars_censoring = lrt_est_censoring %>%
                                        dplyr::filter(pval_bmi_padj < fdr_hd_pvalue) %>% 
                                        dplyr::select(var_group,variable,pval_bmi_padj) %>% 
                                        mutate(outcome = "bmi")



selected_hdvars_censoring %>% 
  group_by(var_group,outcome) %>% 
  tally() %>% 
  pivot_wider(names_from=outcome,values_from=n) %>% 
  mutate(addl_vars = paste0("BMI: ",bmi))
