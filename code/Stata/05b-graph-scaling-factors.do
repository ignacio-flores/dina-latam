////////////////////////////////////////////////////////////////////////////////
//
// 							Title: SCALING FACTORS 
// 			 Authors: Mauricio DE ROSA, Ignacio FLORES, Marc MORGAN 
// 									Year: 2020
//
// 			Description:
// 			Graph scaling factors by concept (to sna) before and after BFM
//			for all countries treated in 03a
//
////////////////////////////////////////////////////////////////////////////////

//General 
clear all
global aux_part  ""preliminary"" 
global types " "raw" "bfm_norep_pre" " // "urb"
do "code/Do-files/auxiliar/aux_general.do"
local lang $lang 

//1. Get data ready ------------------------------------------------------------

local pop "adults"

//merge steps 
local iter = 1 
foreach type in $types {

	//Import file
	qui import excel "${summary}snacompare_`type'_`pop'.xlsx", ///
		sheet("scal_sna") firstrow clear
	
	//Add suffix to varname 
	qui ds country year exception, not 
	foreach var in `r(varlist)' {
		qui rename `var' `var'_`type'
	}
	
	//check duplicates 
	tempvar tag`type'
	qui duplicates tag, gen(`tag`type'')
	cap qui assert `tag`type'' == 0 
	if _rc != 0 {
		di as text "duplicates (step `type') were found for: " _continue 
		qui levelsof country if `tag`type'' == 1, local("list") clean
		di as text "`list'"
		qui duplicates drop	
	}
	
	//merge  
	if (`iter' != 1 ) qui mer 1:1 country year using `tf_aux', nogen
	
	//Save for later
	tempfile tf_aux
	qui save `tf_aux', replace 
	local iter = 0 
	
}

//bring list of extrapolated years by country
preserve
	qui import excel "${w_adj}index.xlsx", ///
		sheet("country_years_03e") firstrow clear
	qui split country_year, parse(_) gen(v)	
	qui levelsof v1, local(countries) 
	foreach c in `countries' {
		*falla aca abajo
		qui levelsof v2 if v1 == "`c'", local(`c'_years) clean s(, )
		di as result "`c': ``c'_years'"
	}	
restore

//2. Compare scaling factors by step 

//loop over variables 
foreach var in $scal_list {
	//loop over countries 
	foreach c in $all_countries {
	
		//prepare main lines for graph 
		local mainlines ""
		local main_leg ""
		local iter = 1
		foreach type in $types {
			//details 
			local s1 = substr("`type'", 1, 3)
			local s2 "c_`s1'"
			//check if series available
			qui count if !missing(`var'_`type') & country == "`c'" 
				if r(N) != 0 {
					local mainlines `mainlines' (connected `var'_`s1' year, ///
						mcolor(${`s2'}) mfcolor(${`s2'}) lcolor(${`s2'}))
					local main_leg `main_leg' label(`iter' "${lab_`s1'_`lang'}")
					local iter = `iter' + 1
				}
		}
		
		//prepare a line to graph extrapolated values 
		local extrap ""
		local extrap_leg ""
		if ("``c'_years'" != "") {
			local extrap (scatter `var'_bfm_norep_pre year ///
				if inlist(year,``c'_years'), ///
				mcolor($c_bfm) mfcolor(${c_bfm}*0.5)) 
			local extrap_leg label(`iter' "Extrap. adjustment")
		}
	
		//cosmetics 
		local limit = 140 
		local byn = 20
		if "`c'" == "BOL" local limit = 300
		if "`c'" == "BOL" local byn = 40
		if "`c'" == "PRY" local limit = 1800
		if "`c'" == "PRY" local byn = 300
		
		//call graph parameters 
		global aux_part  ""graph_basics"" 
		do "code/Do-files/auxiliar/aux_general.do"
		
		if ("`lang'" == "esp") {
			local ylab1 "Agregado / CN"
		} 
		else {
			local ylab2 "Aggregate / NA"
		}
	
		//Graph conditional on having data
		qui sum `var'_raw if country == "`c'" 
		if `r(N)' != 0 {
			graph twoway `mainlines' `extrap' if country == "`c'" ///
				,ytitle("`ylab1'") xtitle("") ///
				yline(100, lpattern(dash) lcolor(black*0.5)) ///
				ylabel(0(`byn')`limit', $ylab_opts) ///
				xlabel($first_y(5)$last_y, $xlab_opts) ///
				legend(`main_leg' `extrap_leg') $graph_scheme 		
		qui graph export ///
			"figures/compare_steps/scaling_factors/`c'_`var'.pdf", replace
	
		}		
	}
}
