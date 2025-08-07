//clean admin data with yearly updates 

di as txt "Using an R to compute survey populations..."
rcall: source("code/R/02a_get_survey_populations.R")
di as txt "Using an R to clean chilean admin data..."
rcall: source("code/R/02b_clean_admin_chl.R")
di as txt "Using an R to clean brazilian admin data..."
rcall: source("code/R/02c_clean_admin_bra.R")

do "code/Stata/tax-data/COL-diverse.do"