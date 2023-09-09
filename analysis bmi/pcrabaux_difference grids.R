difference_grid_sex = expand.grid(cohort = c("COHORThistorical","COHORTexposed","COHORTunexposed"),
                                  modifier1 = c("sex_categoryMale","sex_categoryFemale"),
                                  modifier2_value = c(0,100))

difference_grid_overall = expand.grid(cohort = c("COHORThistorical","COHORTexposed","COHORTunexposed"),
                                  # modifier1 = c("sex_categoryMale","sex_categoryFemale"),
                                  modifier2_value = c(0,100))



difference_grid_age = expand.grid(cohort = c("COHORThistorical","COHORTexposed","COHORTunexposed"),
                                  modifier1 = c("age_category18 to 39","age_category40 to 64","age_category65 plus"),
                                  modifier2_value = c(0,100))

difference_grid_raceeth = expand.grid(cohort = c("COHORThistorical","COHORTexposed","COHORTunexposed"),
                                      modifier1 = c("raceeth_categoryHispanic","raceeth_categoryNH White","raceeth_categoryNH Black"),
                                      modifier2_value = c(0,100))

difference_grid_hospitalization = expand.grid(cohort = c("COHORThistorical","COHORTexposed","COHORTunexposed"),
                                      modifier1 = c("hospitalization",""),
                                      modifier2_value = c(0,100))