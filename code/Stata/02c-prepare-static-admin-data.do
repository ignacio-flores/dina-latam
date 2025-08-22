///////////////////////////////////////////////////////////////////////////////
//				    Goal: Calls dofiles preparing admin data	         //
/////////////////////////////////////////////////////////////////////////////// 

run _config.do

//just do it 
local list_noquotes : subinstr global all_countries `"""' "" , all
foreach dofile in ///
	"DOM-diverse" "ARG-wages-gperc" /*"COL-wid"*/  ///
	"CRI-wage-diverse" "PER-tabulations-gperc" "SLV-wage-diverse-gperc" { 
		
	local sub = substr("`dofile'", 1, 3)
	if strpos("`list_noquotes'", "`sub'") > 0 {
		//run with exceptions 	
		di as result "(02d) Doing `dofile'.do at ($S_TIME)"
		quietly do "code/Stata/tax-data/`dofile'.do"
	}
}

//adjust populations where needed	
di as result "(02d) Doing `dofile'.do at ($S_TIME)"
quietly do "code/Stata/tax-data/BRA-COL-ECU_adults_to_totpop.do"