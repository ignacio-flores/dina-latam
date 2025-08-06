///////////////////////////////////////////////////////////////////////////////
//																			 //
//  MORE UNEQUAL OR NOT AS RICH? The Missing Half of Latin American Income	 //
//			          	De Rosa, Flores & Morgan (2020)						 //
//				    Goal: Calls all dofiles preparing admin data	         //
//																		     //
/////////////////////////////////////////////////////////////////////////////// 
di as txt "Using an R call to compute survey populations..."
rcall: source("code/R/02a_get_survey_populations.R")
rcall: source("code/R/02b_clean_admin_chl.R")

//just do it 
foreach dofile in ///
	/*"DOM-diverse"*/ "ARG-import-tabulations-2009-2014" "ARG-diverse-gperc" ///
	"ARG-wages-gperc" /*"CHL-gperc-ded" "CHL-wage-diverse-gperc"*/ /// 
	"COL-wid" "COL-diverse" "BRA-COL-ECU_adults_to_totpop" "MEX-diverse" ///
	"CRI-wage" "MEX-wages" "PER-tabulations-gperc" ///
	"SLV-wage-diverse-gperc" "URY-gperc" { 
	//run with exceptions 	
	if !inlist("`dofile'", "ARG-diverse-gperc") {
		di as result "(01g) Doing `dofile'.do at ($S_TIME)"
	quietly do "code/Stata/tax-data/`dofile'.do"
	}
}

quietly do "code/Do-files/tax-data/format-for-bfm.do"
