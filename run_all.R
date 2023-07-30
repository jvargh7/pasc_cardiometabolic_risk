# source("preprocessing/pcrpre02_generating index date from diagnosis and lab.R")

source("preprocessing/pcrpre203_identifying new onset diabetes.R")
source("preprocessing/pcrpre204_index date characteristics.R")
source("preprocessing/pcrpre205_clinical characteristics prior to infection.R")
source("preprocessing/pcrpre206_healthcare utilization during lookback.R")


source("preprocessing/pcrpre401_creating follow-up dataset.R")
source("preprocessing/pcrpre402_creating lookback dataset.R")


source("analysis/pcra101_missing data in covariates.R")
source("analysis/pcra102_ipw multiple exposure.R")
# source("analysis/pcra103_ipw missing index date.R")
source("analysis/pcra104_ipw missing outcome.R")

source("analysis/pcra303_covariate adjustment for change in cardiometabolic indicators.R")