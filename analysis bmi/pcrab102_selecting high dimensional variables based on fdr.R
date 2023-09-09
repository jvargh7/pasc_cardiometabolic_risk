
lrt_est <- readRDS(paste0(path_pasc_cmr_folder,"/working/models pcrab/pcrab101_high dimensional covariates with outcome.RDS")) %>% 
  mutate(across(starts_with("pval"),.f=list(padj = ~p.adjust(.,method="BH")))) %>% 
  mutate(var_group = str_extract(variable,"^(ipd|opd|ipp|opp|pre|lab)")) %>% 
  mutate(var_group = case_when(str_detect(variable,"LOINC") ~ "lab",
                               is.na(var_group) ~ "pre",
                               TRUE ~ var_group))

selected_hdvars = bind_rows(lrt_est %>%
                              dplyr::filter(pval_bmi_padj < fdr_hd_pvalue) %>% 
                              dplyr::select(var_group,variable,pval_bmi_padj) %>% 
                              mutate(outcome = "bmi"),
                            lrt_est %>%
                              dplyr::filter(pval_sbp_padj < fdr_hd_pvalue) %>% 
                              dplyr::select(var_group,variable,pval_sbp_padj) %>% 
                              mutate(outcome = "sbp"),
                            lrt_est %>%
                              dplyr::filter(pval_ldl_padj < fdr_hd_pvalue) %>% 
                              dplyr::select(var_group,variable,pval_ldl_padj) %>% 
                              mutate(outcome = "ldl")
)

