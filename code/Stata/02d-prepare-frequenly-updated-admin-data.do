//clean admin data with yearly updates 
run _config.do
local list_noquotes : subinstr global all_countries `"""' "" , all

//CHL 
di as txt "Using an R to clean chilean admin data..."
if strpos("`list_noquotes'", "CHL") > 0 {
	rcall: source("code/R/02b_clean_admin_chl.R")
} 

//BRA
di as txt "Using an R to clean brazilian admin data..."
if strpos("`list_noquotes'", "BRA") > 0 {
	rcall: source("code/R/02c_clean_admin_bra.R")
}

//COL
di as txt "Using an R to clean colombian admin data..."
if strpos("`list_noquotes'", "COL") > 0 {
	do "code/Stata/tax-data/COL-diverse.do"
}	