///////////////////////////////////////////////////////////////////////////////
//																			 //
//  MORE UNEQUAL OR NOT AS RICH? The Missing Half of Latin American Income	 //
//			          	De Rosa, Flores & Morgan (2020)						 //
//				    Goal: Calls all dofiles preparing admin data	         //
//																		     //
/////////////////////////////////////////////////////////////////////////////// 

//just do it 
foreach dofile in ///
	"DOM-diverse" /*"ARG-import-tabulations-2009-2014" "ARG-diverse-gperc"*/ ///
	"ARG-wages-gperc" /*"COL-wid"*/ /*"BRA-COL-ECU_adults_to_totpop"*/ /*"MEX-diverse"*/ ///
	"CRI-wage" "MEX-wages" "PER-tabulations-gperc" ///
	"SLV-wage-diverse-gperc" "URY-gperc" { 
	//run with exceptions 	
	if !inlist("`dofile'", "ARG-diverse-gperc") {
		di as result "(01g) Doing `dofile'.do at ($S_TIME)"
	quietly do "code/Stata/tax-data/`dofile'.do"
	}
}

//BRA R file should be ran here (2000s are done in adults_to_totpop)

di as result "looks like URY and MEX ran correctly, now finish working on adults_to_totpop and format-for-bfm"
exit 1
quietly do "code/Stata/tax-data/format-for-bfm.do"



//WHERE IS ECU? 