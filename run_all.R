source("preprocessing/pcrpre02_generating index date from diagnosis and lab.R")

source("preprocessing/pcrpre03_identifying new onset diabetes.R")
source("preprocessing/pcrpre04_index date characteristics.R")
source("preprocessing/pcrpre05_clinical characteristics prior to infection.R")


source("analysis/pcra101_creating follow-up dataset.R")
source("analysis/pcra102_creating lookback dataset.R")
source("analysis/pcra201_missing data in covariates.R")
source("analysis/pcra202_ipw multiple exposure.R")
source("analysis/pcra203_ipw missing index date.R")
source("analysis/pcra204_ipw missing outcome.R")

source("analysis/pcra303_covariate adjustment for change in cardiometabolic indicators.R")