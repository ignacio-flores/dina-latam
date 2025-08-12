////////////////////////////////////////////////////////////////////////////////
//
// 							Title: BFM - Extrapolations 
// 			 Authors: Mauricio DE ROSA, Ignacio FLORES, Marc MORGAN 
// 									Year: 2020
//
// 			Description:
// 			Study and analyze theta coefficients of country-years with 
//			both tax data and surveys to then use them for survey adjustments
//			(este documento no toma en cuenta cuando theta se extrapola)
//
////////////////////////////////////////////////////////////////////////////////

//General settings -------------------------------------------------------------
clear all 

//preliminary
global aux_part  ""preliminary"" 
qui do "code/Stata/auxiliar/aux_general.do"

// 1. Get average Theta coefficients -------------------------------------------

//Get list of country-years corrected in 02a
global aux_part  ""tax_svy_overlap"" 
qui do "code/Stata/auxiliar/aux_general.do"

//Build panel with thetas 
local iter = 1 
tempfile tfmp
foreach c in $overlap_countries {
	di as result "`c': " _continue 
	di as text "${`c'_overlap_years}"
	foreach y in ${`c'_overlap_years} {
		local type "bfm_norep_pos" 
		if inlist("`c'", "BRA", "CRI") local type "bfm_norep_pre"
		local mp "output/bfm_summary/`type'/merging_points.xlsx"
		qui import excel "`mp'", sheet("`c'`y'") firstrow clear
		qui gen country = "`c'"
		qui gen year = "`y'"
		qui replace mpoint = mpoint[1]
		if `iter' == 0 qui append using `tfmp'
		qui save `tfmp', replace 
		local iter = 0 
	}
}

//Cosmetics 
qui replace p = round(p * 100, 0.0001)
qui replace mpoint = round(mpoint * 100, 0.0001)
qui order country year p small_t antitonic mpoint 
qui sort country year p

//Average theta
bysort country p: egen avg_t = median(antitonic)
qui sort country year p

//Save data
qui export excel "", firstrow(variables) ///
	sheet("panel") sheetreplace 
	
//call graph parameters 
global aux_part  ""graph_basics"" 
qui do "code/Stata/auxiliar/aux_general.do"		

//graph
cap destring year, replace 
foreach c in $overlap_countries {
	
	//deal with pre/post tax incomes 
	local type "bfm_norep_pos" 
	if inlist("`c'", "BRA", "CRI") local type "bfm_norep_pre"
	
	*prepare locals for figure  
	local c2 = lower("`c'")
	local bcol ${c_`c2'}
	local cnt_y = wordcount("${`c'_overlap_years}")
	local onemore = `cnt_y' * 2 + 1
	
	//cosmetic details
	qui sum p if p >= mpoint & country == "`c'" 
	if `r(min)' >= 95 local minp = 95
	else local minp = int(`r(min)' / 10) * 10
	local v = 5 
	if `minp' == 90 local v = 1
	if `minp' == 95 local v = 1 
	qui sum year if country == "`c'" 
	local max_yr_`c' = `r(max)'
	sort country year p
	local maxy = 3

	*build different graphs for countries 
	local iter = 1
	foreach y in ${`c'_overlap_years} {
		qui sum mpoint if country == "`c'" & year == `y'
		local mpt = r(mean)
		local ncol = `iter'/`cnt_y'
		local thlines`c' `thlines`c'' ///
			(line antitonic p if country == "`c'" & year == `y' ///
			& antitonic <= 3, lcolor(`bcol'*`ncol') lwidth(thick)) ///
			(scatter antitonic mpoint if country == "`c'" & ///
			year == `y' & round(mpoint) == p, mcolor(`bcol'*`ncol') ///
			mfcolor(white))
		if `iter' != 1 local itern = `iter' * 2 - 1
		if `iter' == 1 local itern = `iter' 
		local leg`c' `leg`c'' `itern' "`y'"	
		local iter = `iter' + 1
	}
	
	local dirpath "output/figures/thetas"
	mata: st_numscalar("exists", direxists(st_local("dirpath")))
	if (scalar(exists) == 0) {
		mkdir "`dirpath'"
		display "Created directory: `dirpath'"
	}
	
	*graph 
	graph twoway `thlines`c'' ///
		(line avg_t p if country == "`c'" ///
		& avg_t < `maxy' & p >`minp' & year == `max_yr_`c'', ///
		lpattern(dash) lcolor(black)) `mplines`c'' ///
		if avg_t < `maxy' & p >`minp',  ///
		yline(1, lcolor(black*0.3) lpattern(solid)) ///
		ytitle(Theta coefficient) xtitle("Fractile") ///
		ylabel(0(0.5)`maxy', labsize(medium) angle(horizontal) ///
		format(%2.1f) nogrid) ///
		xlabel(`minp'(`v')100, labsize(medium) angle(horizontal) nogrid) ///
		$graph_scheme ///
		legend(order(`leg`c'' `onemore' "Median") ///
		ring(1) pos(3) col(1) ///
		symxsize(3pt) lcolor(none) region(lstyle(none))) 
	qui graph export "output/figures/thetas/`c'.pdf", replace 
	
	*graph 
	graph twoway `thlines`c'' ///
		(line avg_t p if country == "`c'" ///
		& avg_t < `maxy' & p >`minp' & year == `max_yr_`c'', ///
		lpattern(dash) lcolor(black)) `mplines`c'' ///
		if avg_t < `maxy' & p >`minp',  ///
		yline(1, lcolor(black*0.3) lpattern(solid)) ///
		ytitle() xtitle("") ///
		ylabel(0(0.5)`maxy', labsize(medium) angle(horizontal) ///
		format(%2.1f) nogrid) ///
		xlabel(`minp'(`v')100, labsize(medium) angle(horizontal) nogrid) ///
		$graph_scheme ///
		legend(off)
	qui graph export "output/figures/thetas/`c'_esp.pdf", replace 
	
	
	*graph 
	graph twoway `thlines`c'' ///
		(line avg_t p if country == "`c'" ///
		& avg_t < `maxy' & p >`minp' & year == `max_yr_`c'', ///
		lpattern(dash) lcolor(black)) `mplines`c'' ///
		if avg_t < `maxy' & p >`minp',  ///
		yline(1, lcolor(black*0.3) lpattern(solid)) ///
		ytitle(Theta coefficient) xtitle("Fractile") ///
		ylabel(0(0.5)`maxy', labsize(medium) angle(horizontal) ///
		format(%2.1f) nogrid) ///
		xlabel(`minp'(`v')100, labsize(medium) angle(horizontal) nogrid) ///
		$graph_scheme ///
		legend(order(`leg`c'' `onemore' "Mediana") ///
		ring(1) pos(6) col(3) ///
		symxsize(3pt) lcolor(none) region(lstyle(none))) 
	qui graph export "output/figures/thetas/legend_esp.pdf", replace
}






 
