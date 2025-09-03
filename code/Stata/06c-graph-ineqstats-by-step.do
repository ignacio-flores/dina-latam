////////////////////////////////////////////////////////////////////////////////
//
// 					Title: INEQUALITY STATISTICS - GRAPH STEPS
// 			 Authors: Mauricio DE ROSA, Ignacio FLORES, Marc MORGAN 
// 									Year: 2020
//
// 			Description:
// 			Graph inequality statistics at each step of the process
//
////////////////////////////////////////////////////////////////////////////////

//General 
clear all

//Highlight bfm-extrapolations? ["YES"] to activate
local highlight "NO"

//Paths to files 
global aux_part  ""preliminary"" 
qui do "code/Stata/auxiliar/aux_general.do"

//define language 
local lang $lang

//use wid command for inflation rates 
quietly wid, ind(${inflation_wid} ${xppp_eur}) ///
	areas(${areas_wid_latam}) clear 
	
//clean and reshape 	
quietly keep country variable year value 	
reshape wide value, i(country year) j(variable) string	
quietly rename (value${inflation_wid} value${xppp_eur} country) ///
	(defl_xxxx xppp_eur countrycode)
qui sum year if defl_xxxx == 1 	
local xppp_yr = r(mean)
qui label var defl_xxxx "GDP deflator year `xppp_yr'"
quietly drop if year < 2000 
//harmonise country names 
quietly kountry countrycode, from(iso2c) to(iso3c)
quietly rename _ISO3C_ country

//save for input in other dofiles 
quietly export excel "input_data/prices_WID/infl_xrates_wid_wb.xlsx", firstrow(variables) ///
	sheet("inflation-xrates") sheetreplace keepcellfmt  	

// 0. Get macro data  ----------------------------------------------------------
tempfile tf_core 
local iter_core = 0
global units " ${all_units} "
foreach unit in $units {
	//General 
	qui use country iso year TOT_B5g_wid TOT_B5n_wid TOT_K1_wid ///
		priceindex npop* using ///
		"intermediary_data/national_accounts/UNDATA-WID-Merged.dta" ///
		if !missing(country, TOT_B5g_wid), clear 	
	duplicates drop 	

	//define current and real values 
	foreach x in "g" "n" {
		qui rename TOT_B5`x'_wid TOT_B5`x'_wid_curr
		qui gen TOT_B5`x'_wid_real = TOT_B5`x'_wid_curr / priceindex
	}

	//save 
	tempfile tf_wid
	qui save `tf_wid', replace 

	*other inflation and xrates
	qui import excel ///
		"input_data/prices_WID/infl_xrates_wid_wb.xlsx", firstrow sheet("inflation-xrates") clear
	//save xrates in memory 
	quietly levelsof country, local(ctries_wid) clean
	foreach c in `ctries_wid' {
		quietly levelsof xppp_eur if country == "`c'" & defl_xxxx == 1 ///
			, local(xppp_`c') clean 
		global xppp_`c' `xppp_`c''	
	}	
	tempfile tf_infl_xrates
	qui save `tf_infl_xrates', replace 

	// 1. Merge all steps ------------------------------------------------------

	//bring list of extrapolated years by country
	if ("`highlight'" == "YES") {
		global aux_part  ""list_bfm_extrap"" 
		qui do "code/Stata/auxiliar/aux_general.do"
	}

	//Merge all steps
	local iter = 1 
	foreach type in $steps_06c {

		//Import file
		qui import excel "output/ineqstats/ineqstats_`type'_`unit'.xlsx", ///
			sheet("Summary") firstrow clear	
			
		//Add suffix 
		qui ds country year, not 
		local vars "`r(varlist)'"
		foreach var in `vars' {
			qui rename `var' `var'_`type'
		}
		
		//merge 
		if (`iter' != 1) qui mer 1:1 country year using `tf_aux', nogen
		
		//Save for later
		tempfile tf_aux
		qui save `tf_aux', replace 
		local iter = 0 
	}
	
	//identify extrapolated years (option)
	if ("`highlight'" == "YES") {
		qui gen extrap = . 
		foreach c in $extrap_countries {
			foreach y in ${`c'_extrap_years} {
				qui replace extrap = 1 if country == "`c'" & year == `y'
			}
		}
		local adv extrap 
		local adcd & !missing(extrap) 
	}
	else {
		local adv 
		local adcd 
	}	

	//Get to percentages 
	qui ds country year `adv' average* adpop*, not 
	foreach var in `r(varlist)' {
		qui replace `var' = `var' * 100
	}	
		
	//get inflation indexes and ppp xrates 	
	qui merge 1:1 country year using `tf_infl_xrates', keep(2 3) nogen 
	qui levelsof country, local(countries) 

	//merge wid data 
	qui merge 1:1 country year using `tf_wid', keep(3) nogen
	
	//convert old currencies 
	global aux_part  ""old_currencies"" 
	qui do "code/Stata/auxiliar/aux_general.do"

	//get averages with survey population 
	foreach x in "n" "g" {
		qui gen a`x'ninc_svypop_adu = TOT_B5`x'_wid_real / adpop_natinc
		qui gen a`x'ninc_widpop_adu = TOT_B5`x'_wid_real / npopul_adults
		qui gen a`x'ninc_widpop_tot = TOT_B5`x'_wid_real / npopul
	}

	*save a variable with current average income 
	qui gen average_natinc_curr = average_natinc 

	foreach s in $steps_06c {

		*adjust old currencies (old to new)
		foreach curr in $old_currencies {
			*identify country 
			local coun = substr("`curr'", 1, 3)
			*exceptions 
			if !inlist("`coun'", "URY"){
				quietly replace average_`s' = average_`s' * ${xr_`curr'} ///
					if year < ${yr_`curr'} & country == "`coun'"	
			}
		}
		*special case (ECU)
		foreach z in `xr_yrs_ecu' {
			*qui replace average_`s' = (average_`s') / `xr_ecu_`z'' ///
			*	if year == `z' & country == "ECU" & year < 2000
		}
			
		//get real 	
		qui replace average_`s' = (average_`s') / priceindex ///
			//if !inlist(country, "ECU")
		
		//get ppp eur 
		if "`s'" == "natinc"{
			qui gen agninc_m_adu = .
			qui gen agninc_m_tot = .
			qui gen anninc_m_adu = .
			qui gen anninc_m_tot = .
		} 
		foreach c in `countries' {	
			if "${xppp_`c'}" != "" {
				qui replace  average_`s' = ///
					(average_`s'/12) / ${xppp_`c'} if country == "`c'" 
				if "`s'" == "natinc" {
					foreach x in "g" "n" {
						*get to real monthly  
						qui replace a`x'ninc_m_adu = ///
							(a`x'ninc_widpop_adu/12) / ${xppp_`c'} ///
							if country == "`c'" 
						qui replace a`x'ninc_m_tot = ///
							(a`x'ninc_widpop_tot/12) / ${xppp_`c'} ///
							if country == "`c'" 	
					}	
				}
			}
		}
	}
	
	*save data for transparency index 
	if "${mode}" != "debug"{
		foreach s in "raw" "bfm${ext}_pre" "rescaled" {
			tempvar aux_`s'
			qui egen `aux_`s'' = rowtotal(*_`s')
			qui gen `s' = 1 if `aux_`s'' != 0 
		}
		qui replace bfm${ext}_pre = . ///
			if !missing(bfm${ext}_pre) `adcd'
		qui rename (raw bfm${ext}_pre rescaled) (Survey AdminData NatAccounts)
		qui export excel country year Survey AdminData NatAccounts using ///
				"output/figures/data_reports/wid_availability_latam.xlsx", ///
				replace firstrow(variables)
	}
	
	//save info on unit 
	qui gen unit = "`unit'"
	qui order unit 
	if `iter_core' != 0 append using `tf_core'
	qui save `tf_core', replace 
	local iter_core = 1 
}
	
//Graph basics  
global aux_part  ""graph_basics""
qui do "code/Stata/auxiliar/aux_general.do"	


//2. Compare estimates -----------------------------------------------------
foreach unit in $units {
	
	//
	qui use `tf_core', clear 
	qui keep if unit == "`unit'"

	//Bring country/years with extrapolated SNA scaling factors 
	merge 1:m country year using "results/extrap_sna.dta", nogen
	qui duplicates drop 

	//Exclude countries with inconsistencies
	qui do "code/Stata/auxiliar/aux_exclude_ctries.do"

	//loop over variables and countries 
	foreach var in `vars' {
		foreach c in $all_countries {
		display as text "working with `var' for `c'"
		
			//max value
			if strpos("`var'", "b50") local maxy = 20 
			if strpos("`var'", "m40") local maxy = 60
			if strpos("`var'", "t1") local maxy = 40
			if strpos("`var'", "average") local maxy = 1600
			if strpos("`var'", "gini") | strpos("`var'", "t10") local maxy = 80 
			
			//mid value
			 local midy = 10 
			 if strpos("`var'", "b50") local midy = 5 
			 if strpos("`var'", "average") local midy = 400 
			
			//min malue 
			local miny = 0
			if strpos("`var'", "gini") local miny = 40
			if strpos("`var'", "t1") | strpos("`var'", "average") {
				local miny = 0 
			}  
			if strpos("`var'", "m40") local miny = 10
			if strpos("`var'", "t10") local miny = 30 
			
			//Legend label for group in loop
			if "`lang'" == "eng" {
				if strpos("`var'", "b50") local labg "Bottom 50% Share"
				if strpos("`var'", "m40") local labg "Middle 40% Share"
				if strpos("`var'", "t1") local labg "Top 1% Share"
				if strpos("`var'", "t10") local labg "Top 10% Share"
				if strpos("`var'", "gini") local labg "Gini coefficient"
				if strpos("`var'", "average") {
					local labg "Average income, EUR PPP /month"	
				} 
			}
			if "`lang'" == "esp" {
				if strpos("`var'", "b50") local labg "Parte del 50% más pobre"
				if strpos("`var'", "m40") local labg "Parte del 40% del medio"
				if strpos("`var'", "t1") local labg "Parte del 1% más rico"
				if strpos("`var'", "t10") local labg "Parte del 10% más rico"
				if strpos("`var'", "gini") local labg "Coeficiente de Gini"
				if strpos("`var'", "average") {
					local labg "Ingreso promedio, EUR PPP /mes"
				}	
			}
			
			//define main attributes of graph 
			local iter = 1 
			//loop over steps
			foreach s in $steps_06c {
				if ("``c'_dum'" != "graph") local `c'_dum "ignore"
				qui count if !missing(`var'_`s') & country == "`c'" 
				if r(N) != 0 {
					local `c'_dum "graph" 
					//details 
					local s1 = substr("`s'", 1, 3)
					local s2 "c_`s1'"
					//graph lines 
					local mlines_`var'_`c' `mlines_`var'_`c'' ///
					(connected `var'_`s' year if exclude==0, ///
					lcolor($`s2') msymbol(O) mfcolor($`s2') mcolor($`s2')) 	
					
				
					//legend lines 	
					local  mlegend_`var'_`c' ///
						`mlegend_`var'_`c'' label(`iter' "${lab_`s1'_`lang'}")
					//Highlight extrapolated corrections 
					cap count if !missing(`var'_bfm${ext}_pre) ///
						& country == "`c'" & extrap == 1 & exclude==0
					if (_rc == 0 & r(N) != 0 & ///
						"`s1'" == "bfm" & "`highlight'" == "YES") {
						local mlines_`var'_`c' `mlines_`var'_`c'' ///
							(scatter `var'_bfm${ext}_pre year ///
							if extrap == 1 & exclude==0, msymbol(O) ///
							mfcolor(${c_bfm}*0.5) mcolor(${c_bfm})) 
						/*local iter = `iter' + 1	
						local  mlegend_`var'_`c' `mlegend_`var'_`c'' ///
							label(`iter' "Extrap. Correction")*/
					}
					if ("`s'" == "rescaled" & "`highlight'" == "YES") {
						local mlines_`var'_`c' `mlines_`var'_`c'' ///
							(scatter `var'_rescaled year ///
							if (extrap_sca == 1 & exclude==0), ///
							lcolor($c_res) msymbol(O) mfcolor($c_res) ///
							mcolor($c_res)  mfcolor(${c_res}*0.5)) 
					}
					if ("`s'" == "uprofits" & "`highlight'" == "YES") {
						local mlines_`var'_`c' `mlines_`var'_`c'' ///
							(scatter `var'_uprofits year ///
							if (extrap_sca == 1 & exclude==0), ///
							lcolor($c_upr2) msymbol(O) mfcolor($c_upr2) ///
							mcolor($c_upr2)  mfcolor(${c_upr2}*0.5))  
					}
					if ("`s'" == "natinc" & "`highlight'" == "YES") {
						local mlines_`var'_`c' `mlines_`var'_`c'' ///
							(scatter `var'_natinc year ///
							if (extrap_sca == 1 & exclude==0), ///
							lcolor($c_nat) msymbol(O) mfcolor($c_nat) ///
							mcolor($c_nat)  mfcolor(${c_nat}*0.5))  
					}
					
					//count iterations 
					local `unit'_ordrstr_`var'_`c' ``unit'_ordrstr_`var'_`c'' `iter'
					local iter = `iter' + 1
				}
			}
			
			if "``c'_dum'" == "graph" {
				
				*exceptions 
				if "`c'" == "DOM" {
					local firsty = 2000
					local midy2 = 2
					local addcon & year >= `firsty'
				}
				else {
					local firsty = $first_y 
					local midy2 = 5 
					local addcon 
				}
				
				//compare with wid data 
				local tline ""
				if "`var'" == "average" {
					if inlist("`unit'", "ind", "esn", "act") {
						local avg`unit' agninc_m_adu 
					}
					if inlist("`unit'", "pch") {
						local avg`unit' agninc_m_tot 
					}
					
					local tline (line `avg`unit'' year, lcolor(black))
				} 
				//Graph 
				di as result "var `var', country `c', unit `unit'"
				graph twoway `mlines_`var'_`c'' `tline' if country == "`c'" ///
					`addcon' ,ytitle("") xtitle("") ///
					ylabel(`miny'(`midy')`maxy', $ylab_opts) ///
					xlabel(`firsty'(`midy2')2020, $xlab_opts) ///
					legend(`mlegend_`var'_`c'' symxsize(4pt)) ///
					${graph_scheme} legend(off)
				qui graph export ///
					"figures/compare_steps/ineqstats/`c'_`var'_`unit'.pdf", ///
					replace	
				
				//legend	
				if ("`c'" == "URY" & "`var'" == "gini") {
					//Graph 
					graph twoway `mlines_`var'_`c'' ///
						if country == "`c'" ///
						,ytitle("") xtitle("") ///
						ylabel(`miny'(`midy')`maxy', $ylab_opts) ///
						xlabel(${first_y}(5)2020, $xlab_opts) ///
						${graph_scheme} legend(`mlegend_`var'_`c'' ///
						order(``unit'_ordrstr_`var'_`c''))				
					qui graph export ///
						"figures/compare_steps/ineqstats/legend_`lang'.pdf", ///
						replace	
				}	
			}
		}
	}
}

