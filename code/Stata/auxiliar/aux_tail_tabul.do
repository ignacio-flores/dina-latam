
qui cap drop country
qui gen country = "`country'"
qui sum average
local 	mean_income = r(mean)
*local   inc  "$inc"

/*
*save infor for later 
if !inlist("`country'", "COL", "ECU","DOM") {
	if inlist("`country'", "SLV", "ARG", "PER", "URY", "MEX") qui sum totalpop 
	else if inlist("`country'", "CRI") qui sum population 
	else qui sum popsize 
	local popsize = r(mean)
	global agg_inc_`country'_`year' = .
	global agg_inc_`country'_`year' = `mean_income' * `popsize'
	if "`country'" == "CRI" global agg_inc_`country'_`year' = ${agg_inc_`country'_`year'} * 13
}
*/

qui gen s_function 		= 1 - p	 
qui gen ln_s 			= ln(1/s_function)

if inlist("`country'","CHL","BRA","CRI","PER","URY","MEX","ARG") {	
	qui cap gen	ln_inc_`year'	= ln(thr / `mean_income')
}
if inlist("`country'","COL","ECU","SLV","ARG","DOM") {	
	qui cap  gen	ln_inc_`year'	= ln(bracketavg / `mean_income')
}

qui keep ln_inc_`year' ln_s country
qui keep if ln_inc_`year' 	> 0
qui keep if ln_s 			< 12 
*if inlist("`country'","ARG","SLV") qui keep if ln_s > ln(1/(0.01))
qui save ``country'_`inc'_`year''
*qui save ``country'_`year''

clear
