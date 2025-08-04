////////////////////////////////////////////////////////////////////////////////
//
// 							Title: EFFECTIVE TAX RATES 
// 			 Authors: Mauricio DE ROSA, Ignacio FLORES, Marc MORGAN 
// 									Year: 2020
//
// 			Description:
// 			Calculates effective tax rates for all country-years for which we
//			have data. 
//
////////////////////////////////////////////////////////////////////////////////

clear all
global codes "code/Do-files/eff-tax-rates"


foreach c in  "COL" "BRA" "MEX"{
	di as result "running $codes/compute-eff-tax-rate-`c'-alt.do"
	global codes "code/Do-files/eff-tax-rates"
	qui do "$codes/compute-eff-tax-rate-`c'-alt.do"
}


*empirical w/o social sec
foreach c in "CRI" /*"ARG"*/ "CHL" "SLV"  {
	di as result "running $codes/compute-eff-tax-rate-`c'.do"
	global codes "code/Do-files/eff-tax-rates"
	qui do "$codes/compute-eff-tax-rate-`c'.do"
}

*empirical w/ social sec
foreach c in /*"COL"*/ "ECU" "URY" {
	di as result "running $codes/compute-eff-tax-ss-rate-`c'.do"
	global codes "code/Do-files/eff-tax-rates"
	qui do "$codes/compute-eff-tax-ss-rate-`c'.do"
}

*theroetical
di as result "running $codes/compute-eff-tax-ss-rate-`c'.do"
global codes "code/Do-files/eff-tax-rates"
qui do "$codes/compute-eff-tax-rate-theo-PER.do"
*do "$codes/compute-eff-tax-rate-theo-PRY.do"

clear
