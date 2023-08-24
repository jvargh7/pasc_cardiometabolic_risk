demo_covariates = "+ female + nhblack + hispanic + nhother + age"
index_date_covariates <- "+ site + payer_type_primary + payer_type_secondary + hospitalization + p_hyperglycemia + HT + smoking" 
comorbidity_covariates <- "+ obesity + cardiovascular + cerebrovascular + hypertension + pulmonary + hyperlipidemia"
medication_covariates <- "+ antidepressants + antipsychotics + antihypertensives + statins + immunosuppresants"
labs_covariates <- "+ hba1c + glucose + alt + ast + serum_creatinine + hdl"
lb_hc_covariates <- "+ lb_hospitalized + lb_telehealth + lb_outpatient + lb_n_glucose + lb_n_hdl + lb_n_ldl + lb_n_serum_creatinine + lb_n_alt + lb_n_ast + lb_n_hba1c + lb_n_labvisits"
ipw_multiple_exposure = paste0("COHORT ~ 1 ",demo_covariates,index_date_covariates,comorbidity_covariates,medication_covariates,labs_covariates,lb_hc_covariates)

ipw_missing_index = paste0("in_index_date ~ COHORT ",demo_covariates)

ipw_missing_bmi = paste0("in_bmi_ID ~ COHORT ",demo_covariates,index_date_covariates,comorbidity_covariates,medication_covariates,labs_covariates,lb_hc_covariates)
ipw_missing_sbp = paste0("in_sbp_ID ~ COHORT ",demo_covariates,index_date_covariates,comorbidity_covariates,medication_covariates,labs_covariates,lb_hc_covariates)
ipw_missing_ldl = paste0("in_ldl_ID ~ COHORT ",demo_covariates,index_date_covariates,comorbidity_covariates,medication_covariates,labs_covariates,lb_hc_covariates)

