////////////////////////////////////////////////////////////////////////////////
// Calculates effective tax rates for all country-years with data. 
////////////////////////////////////////////////////////////////////////////////

clear all
global codes "code/Stata/eff-tax-rates"

// Create directory if it doesnt exist 
local dirpath "output/figures/eff_tax_rates"
mata: st_numscalar("exists", direxists(st_local("dirpath")))
if (scalar(exists) == 0) {
	mkdir "`dirpath'"
	display "Created directory: `dirpath'"
}

//we need to fix BRA
foreach c in  "COL" /*"BRA"*/ "MEX"{
	di as result "running $codes/compute-eff-tax-rate-`c'-alt.do"
	global codes "code/Stata/eff-tax-rates"
	qui do "$codes/compute-eff-tax-rate-`c'-alt.do"
}


*empirical w/o social sec
foreach c in "CRI" /*"ARG"*/ "CHL" "SLV"  {
	di as result "running $codes/compute-eff-tax-rate-`c'.do"
	global codes "code/Stata/eff-tax-rates"
	qui do "$codes/compute-eff-tax-rate-`c'.do"
}

*empirical w/ social sec
foreach c in /*"COL"*/ "ECU" "URY" {
	di as result "running $codes/compute-eff-tax-ss-rate-`c'.do"
	global codes "code/Stata/eff-tax-rates"
	qui do "$codes/compute-eff-tax-ss-rate-`c'.do"
}

*theroetical
di as result "running $codes/compute-eff-tax-ss-rate-PER.do"
global codes "code/Stata/eff-tax-rates"
qui do "$codes/compute-eff-tax-rate-theo-PER.do"

clear

//DOM? 