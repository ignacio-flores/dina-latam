////////////////////////////////////////////////////////////////////////////////
//
// 						Title: INEQUALITY STATISTICS - GRAPHS 
// 			 Authors: Mauricio DE ROSA, Ignacio FLORES, Marc MORGAN 
// 									Year: 2020
//
// 			Description:
// 			Graph income shares, composition and ginis for every country-year 
// 			at each step. 
//
////////////////////////////////////////////////////////////////////////////////
 
//General settings 
clear all 

//Options 
global ineqvars " "t10_sh" "m40_sh" "b50_sh" "gini" "t1_sh" " 
global groups " "tot" "t10" "m40" "b50" "t1" "
local graph_avg "NO" // "YES" if we want it 

//Preliminary   
global aux_part  ""preliminary"" 
qui do "code/Do-files/auxiliar/aux_general.do"
local lang $lang 

// PART 0: Inflation (WID.WORLD & World Bank)------------------------------

local last_minus1 = $last_y - 1 
local last_y = $last_y 

//cpi indexes 
qui use "Data/World_Bank/inflation/inflation_latam.dta"
cap drop __*
drop if missing(inflation)
drop if year > $last_y
drop if year < $first_y
qui gen cpi_`last_y' = 1 if year == $last_y
qui gen infl_aux = inflation / 100
forvalues v = `last_minus1'(-1)$first_y {
	qui replace cpi_`last_y' = ///
		cpi_`last_y'[_n+1] * 1/(1 + infl_aux[_n+1]) if year == `v'
}

*
qui keep country year cpi_`last_y'
*qui drop if country == "ARG"
tempfile tf_cpi 
qui save `tf_cpi', replace

//use wid command 
qui wid, ind(${inflation_wid} ${xppp_eur}) ///
	areas(${areas_wid_latam}) clear 
	
//clean and reshape 	
qui keep country variable year value 	
reshape wide value, i(country year) j(variable) string	
qui rename (value${inflation_wid} value${xppp_eur} country) ///
	(defl_`last_y' xppp_eur countrycode)
	
//harmonise country names 
qui kountry countrycode, from(iso2c) to(iso3c)
qui rename _ISO3C_ country

//save xrates from `last_y' in memory 
qui levelsof country, local(ctries_wid) clean 
foreach c in `ctries_wid' {
	qui levelsof xppp_eur if country == "`c'" & year == `last_y' ///
		, local(xppp`last_y'_`c') clean 
}

//save matrix for later 
tempfile tf_infl
qui save `tf_infl', replace 

//Merge CPI and GDP deflator
qui merge 1:1 country year using `tf_cpi', //keep(3) nogen 
qui drop if _merge != 3 & !inlist(country, "ARG", "CRI", "DOM")
qui drop if year < $first_y 
qui drop _merge 
qui save `tf_infl', replace 

//save for input in other dofiles 
qui export excel ${inflation_data}, firstrow(variables) ///
	sheet("inflation-xrates") sheetreplace keepcellfmt  	

// PART 1: INCOME SHARES, AVERAGE AND INDEXES -------------------------------

////bring list of extrapolated years by country
global aux_part  ""list_bfm_extrap"" 
qui do "code/Do-files/auxiliar/aux_general.do"

//loop over steps and groups 
local iter_inf = 1 

global units " $unit_list "
	foreach unit in $units {
	foreach type in $all_steps {
	
		//define short labels 
		local shortlab_type =  substr("`type'", 1, 3)

		//call graph settings 
		global aux_part ""graph_basics"" 
		qui do "code/Do-files/auxiliar/aux_general.do"

		//report activity in log 
		display as text "{hline 65}"
		display as text ///
			"06b: Working with unit `unit' and step `type' at $S_TIME"
		display as text "{hline 65}"
			
		//import summary 	
		tempfile tf_fusion_`type'_`unit' 
		*local iter = 1 
		import excel "${summary}ineqstats_`type'_`unit'.xlsx", ///
			sheet("Summary") firstrow clear
		qui save `tf_fusion_`type'_`unit'', replace 	
		qui drop if missing(average)
		
		//work only with countries in the list 
		qui gen include = 0
		foreach ccc in $all_countries {
			qui replace include = 1 if ///
				country == "`ccc'" & inrange(year, ${first_y}, ${last_y})
		}
		qui keep if include == 1 
		qui drop include 
		
		//tag extrapolated obs 
		if ("`type'" == "bfm_norep_pre") { 
			qui gen extrap = . 
			foreach c in $extrap_countries {
				foreach y in ${`c'_extrap_years} {
					qui replace extrap = 1 if country == "`c'" & year == `y'
				}
			}
		}
		
		//Save to merge later
		tempfile tf_`type'
		qui save `tf_`type''
		
		//merge with wid data 
		qui merge 1:1 country year using `tf_infl', keep(3) nogen
		
		*adjust data for old currencies (old surveys to new currency)
		*wid national income is already in current lcu (1a takes care of it)
		global aux_part  ""old_currencies"" 
		quietly do "code/Do-files/auxiliar/aux_general.do"
		foreach curr in $old_currencies {
			*identify country 
			local coun = substr("`curr'", 1, 3)
			*exceptions 
			if !inlist("`coun'", "URY"){
				*replace variable
				quietly replace average = average * ${xr_`curr'} ///
					if year < ${yr_`curr'} & country == "`coun'"	
			}
		}
		
		*Harmonize special case (ECU)
		/*
		preserve 
			quietly import excel $wb_xrates, sheet("ECU") clear firstrow
			qui destring year, replace 
			quietly levelsof year, local(xr_yrs_ecu) clean 	
			foreach z in `xr_yrs_ecu' {
				quietly sum rate if year == `z' 
				local xr_ecu_`z' = r(mean)
			}
		restore
		*/
		foreach z in `xr_yrs_ecu' {
			qui replace average = average / `xr_ecu_`z'' ///
				if year == `z' /*& country == "ECU"*/ & year < 2000
		}
		
		//real average income in lcu
		qui gen avg_lcu_`last_y' = . 
		*qui replace avg_lcu_`last_y' = (average / 12) / cpi_`last_y' ///
		*	if !inlist(country, "ARG", /*"ECU",*/ "CRI")
		qui replace avg_lcu_`last_y' = (average / 12) / defl_`last_y' /* ///
			if inlist(country, "ARG", /*"ECU",*/ "CRI")*/
			
		//from real lcu to ppp eur 
		qui gen xppp_${last_y} = .	
		qui gen avg_ppp_`last_y' = . 
		qui levelsof country, local(ctrs) clean 
		foreach c in `ctrs' {
			qui replace avg_ppp_`last_y' = ///
				avg_lcu_`last_y' / `xppp`last_y'_`c'' if country == "`c'"
			qui replace xppp_${last_y} = `xppp`last_y'_`c'' if country == "`c'"	
		}
		
		//The exceptions 
		qui do "code/Do-files/auxiliar/aux_exclude_ctries.do"
		qui drop if exclude == 1

		//create variables for average incomes 
		foreach g in ".5" ".4" ".1" ".01" {
			if `g' == .5 local pf "b" 
			if `g' == .4 local pf "m" 
			if `g' <= .1 local pf "t" 
			local gp = `g' * 100
			qui gen `pf'`gp'_ppp_`last_y' = ///
				`pf'`gp'_sh * avg_ppp_`last_y' / `g' 
			//index 
			qui gen `pf'`gp'_avg_idx = . 
			qui levelsof country if !missing(`pf'`gp'_ppp_`last_y') ///
				, local(ctries_idx) clean
			foreach c in `ctries_idx' {
				qui sum year if country == "`c'" & ///
					!missing(`pf'`gp'_ppp_`last_y')
				local min_yr_`c' = r(min) 
				qui levelsof `pf'`gp'_ppp_`last_y' ///
					if (country == "`c'" & year == `min_yr_`c'' ///
					& !missing(`pf'`gp'_ppp_`last_y')), ///
					local(idx_1st_`c') clean	
				qui replace `pf'`gp'_avg_idx = ///
					`pf'`gp'_ppp_`last_y' / `idx_1st_`c'' if country == "`c'"	
			}
		}
		
		*index for total average
		qui gen totavg_idx = .
		lab var totavg_idx "Total population"
		foreach c in `ctries_idx' {
			qui sum year if country == "`c'" & !missing(avg_ppp_`last_y')
				local min_yr_`c' = r(min) 
				qui levelsof avg_ppp_`last_y' if (country == "`c'" ///
					& year == `min_yr_`c'' & !missing(avg_ppp_`last_y')) ///
					, local(idx_1st_`c') clean	
				qui replace totavg_idx = ///
					avg_ppp_`last_y' / `idx_1st_`c'' if country == "`c'"	
		}
		
		//lists of variables 
		foreach g in "b50" "m40" "t10" "t1" {
			local idxvars_`shortlab_type'_`unit' ///
				"`idxvars_`shortlab_type'_`unit'' `g'_avg_idx"
			local pppvars_`shortlab_type'_`unit' ///
				"`pppvars_`shortlab_type'_`unit'' `g'_ppp_`last_y'"
			qui la var `g'_avg_idx "${lname_`g'} Share"
		}

		//Graph indexes of all groups in the same country --------------------------
		
		foreach c in `ctrs' {
			local c_tit = lower("`c'")
			qui count if !missing(t10_avg_idx) & country == "`c'" 
			if r(N) != 0 {
				
				//one graph by country 
				graph twoway ///
					(line t1_avg_idx year, lcolor($c_cap) lwidth(thick)) ///
					(line t10_avg_idx year, lcolor($c_kap) lwidth(thick)) ///
					(line m40_avg_idx year, lcolor($c_wag) lwidth(thick)) ///
					(line b50_avg_idx year, lcolor($c_ben) lwidth(thick)) ///
					(line totavg_idx year, lcolor(black) ///
					lwidth(thick) lpattern(dot))   /// 
					if country == "`c'", title("${lname_`c_tit'}") ///
					yline(1, lcolor(black) lpattern(dash)) ///
					ytitle(/*"Real Average Income, Index base 1"*/ "") xtitle("") ///
					ylabel(/*0.5(0.5)2.5*/, $ylab_opts format(%2.1f)) ///
					xlabel(${first_y}(5)2020, $xlab_opts) $graph_scheme /*legend(off)*/ 
				qui graph export ///
					"figures/`type'/ineqstats/countries/`c'_idx_groups_`unit'.pdf", replace

				*get one with a legend 
				if "`c'" == "BRA" {		
				graph twoway ///
					(line t1_avg_idx year, lcolor($c_cap) lwidth(thick)) ///
					(line t10_avg_idx year, lcolor($c_kap) lwidth(thick)) ///
					(line m40_avg_idx year, lcolor($c_wag) lwidth(thick)) ///
					(line b50_avg_idx year, lcolor($c_ben) lwidth(thick)) ///
					if country == "`c'", title("${lname_`c_tit'}") ///
					yline(1, lcolor(black) lpattern(dash)) ///
					ytitle(/*"Real Average Income, Index base 1"*/ "") xtitle("") ///
					ylabel(0.5(0.5)2.5, $ylab_opts format(%2.1f)) ///
					xlabel(${first_y}(5)2020, $xlab_opts) $graph_scheme  
				qui graph export ///
			"figures/`type'/ineqstats/countries/legend_idx_groups_`unit'.pdf", ///
					replace
			
				}	
			}		
		}
		
		//loop over variables 
		foreach var in $ineqvars ///
			`idxvars_`shortlab_type'_`unit'' ///
			`pppvars_`shortlab_type'_`unit'' {
			
			di as text "  -`var'"
			
			//transform to percentage / index
			if !strpos("`var'", "ppp_`last_y'") qui replace `var' = `var' * 100
			
			//loop over countries  
			qui levelsof country if !missing(`var'), local(ctries) clean 
			local iter = 1 
			if "`ctries'" != "" {
				foreach c in `ctries' {
				
					//details
					local c1 = strlower("`c'")
					local c2 "c_`c1'"
					
					//prepare lines for main data 
					*if !inlist("`c'", "ARG") { // ("`c'", "ARG")
					
						*if "`c'" == "ARG" local pat lpattern(dash)
						*if "`c'" == "CHL" local pat lpattern(solid)
						*if !inlist("`c'", "ARG", "CHL") local pat 
						
						local lines_`var'_`shortlab_type'_`unit'  ///
							`lines_`var'_`shortlab_type'_`unit' ' ///
							(line `var' year if country == "`c'", ///
							lcolor($`c2') lwidth(thick) `pat')
							
						//prepare lines for extrapolated data
						if ("`type'" == "bfm_norep_pre") { 
							local e_`var'_`shortlab_type'_`unit' `e_`var'_`shortlab_type'_`unit'' ///
								(scatter `var' year if country == "`c'" ///
								& extrap == 1, msymbol(O) mfcolor($`c2'*0.5) ///
								mcolor($`c2'))
								
						}
						else local e_`var'_`shortlab_type'_`unit' ""
						
						//prepare legend 
						if ("`var'" == "t10_sh") { 
							local labels_`var'_`shortlab_type'_`unit' ///
								`labels_`var'_`shortlab_type'_`unit'' ///
								label(`iter' "${lname_`c1'}")
							//count iterations 
							local iter = `iter' + 1 
						}
					*}
				}
					
				//Define title of y-axis 
				if "`lang'" == "eng" {
					if strpos("`var'", "ppp_`last_y'") local ytit "average inc., PPP `last_y' euros"
					if strpos("`var'", "idx") local ytit "real average inc. (index)"
					if strpos("`var'", "sh") local ytit "share"
					if strpos("`var'", "t1_") local ytit "Top 1% `ytit'"
					if strpos("`var'", "t10") local ytit "Top 10% `ytit'"
					if strpos("`var'", "m40") local ytit "Middle 40% `ytit'"
					if strpos("`var'", "b50") local ytit "Bottom 50% `ytit'"
					if "`var'" == "gini" local ytit "Gini coefficient"
				}
				if "`lang'" == "esp" {
					if strpos("`var'", "ppp_`last_y'") {
						local ytit "Ing. promedio, PPP `last_y' euros"
					} 
					if strpos("`var'", "idx")  {
						local ytit "Ing. real promedio (indice)"
					}
					if strpos("`var'", "sh") local ytit "Parte del"
					if strpos("`var'", "t1_") local ytit "`ytit' 1% más rico"
					if strpos("`var'", "t10") local ytit "`ytit' 10% más rico"
					if strpos("`var'", "m40") local ytit "`ytit' 40% del medio"
					if strpos("`var'", "b50") local ytit "`ytit' 50% más pobre"
					if "`var'" == "gini" local ytit "Coeficiente de Gini"
				}
				
				//Define max value y-axis
				local maxy ""
				*if strpos("`var'", "ppp_`last_y'") local maxy = 12000
				if strpos("`var'", "idx") local maxy = 250 
				if "`var'" == "t1_sh" local maxy = 35
				if "`var'" == "t10_sh" local maxy = 70
				if "`var'" == "m40_sh" local maxy = 50
				if "`var'" == "b50_sh" local maxy = 20
				if "`var'" == "gini" local maxy = 80
				if "`var'" == "gini" & "`type'" == "uprofits"  local maxy = 100
				if "`var'" == "t1_sh" & "`type'" == "uprofits" local maxy = 40 
				//if "`var'" == "b50_sh" & "`type'" == "uprofits" local maxy = 20
				
				//Define min value y-axis
				local miny ""
				*if strpos("`var'", "ppp_`last_y'") local miny = 0
				if strpos("`var'", "idx") local miny = 50 
				if inlist("`var'", "t10_sh") local miny = 30
				if inlist("`var'", "m40_sh") local miny = 20
				if inlist("`var'", "b50_sh") local miny = 0
				if "`var'" == "gini" local miny = 40
				if ("`var'" == "t1_sh") {
					qui replace t1_sh = . if t1_sh > 40 
					local miny = 5
				} 
				
				if "`var'" == "m40_sh" & "`type'" == "uprofits" local miny = 25 
				//if "`var'" == "t10_sh" & "`type'" == "uprofits" local miny = 35 
				if "`var'" == "t1_sh" & "`type'" == "uprofits"  local miny = 5 
				
				
				//Define mid values y-axis 
				local midy = 10
				if strpos("`var'", "idx") local midy = 50 
				if "`var'" == "b50_sh" local midy = 5
				*if strpos("`var'", "ppp_`last_y'") local midy = 2000
				
				//Estimate average 
				sort country year 
				
				//add average to graph if needed
				if "`graph_avg'" == "YES" & !strpos("`var'", "idx") {
					bysort country: ipolate `var' year, gen(ipo_`var') epolate
					bysort year: egen `var'_avg = mean(ipo_`var')
					sort country year 	
					local avg_line_`var'_`shortlab_type'_`unit' (line `var'_avg year ///
						if country == "CRI", lcolor(black) lwidth(thick) ///
						/*mcolor(black) mfcolor(black) msize(normal)*/) 			
				}
				
				//add line in 100 for indexes 
				local l100 ""
				if strpos("`var'", "idx") local l100 yline(100, lcolor(black) ///
					lpattern(dash))
				local ylabs `miny'(`midy')`maxy'	
				if strpos("`var'", "ppp_`last_y'") local ylabs ""
				
				//Graph and save without legend		
				graph twoway `lines_`var'_`shortlab_type'_`unit'' ///
					`e_`var'_`shortlab_type'_`unit'' ///
					`avg_line_`var'_`shortlab_type'_`unit'' ///
					if !missing(`var'), `l100' ytitle(/*"`ytit'"*/ "") xtitle("") ///
					ylabel(`ylabs', $ylab_opts format(%2.0f)) ///
					xlabel(${first_y}(5)2020, $xlab_opts) ///
					$graph_scheme legend(off)
				qui graph export ///
					"figures/`type'/ineqstats/variables/`var'_`unit'.pdf", replace 
				*qui graph export ///
				*	"figures/`type'/ineqstats/variables/`var'_`unit'.png", replace

			}
		}

		//Save one graph with legend (for TeX file)
		local var "t10_sh"
		if "`graph_avg'" == "YES" {
			local lines_`var'_`shortlab_type'_`unit' ///
				`lines_`var'_`shortlab_type'_`unit' ' ///
				`avg_line_`var'_`shortlab_type'_`unit''
			qui levelsof country 
			local nctries = r(r) + 1
			di as result "n: " `nctries'
			local labels_`var'_`shortlab_type'_`unit' ///
				`labels_`var'_`shortlab_type'_`unit'' ///
				label(`nctries' "Average")
		}

		
		graph twoway ///
			`lines_`var'_`shortlab_type'_`unit' ' if !missing(`var'), ///
			legend(`labels_`var'_`shortlab_type'_`unit'') $graph_scheme
		qui graph export ///
			"figures/`type'/ineqstats/variables/legend_ctries.pdf" ///
			, replace	
		*qui graph export ///
		*	"figures/`type'/ineqstats/variables/legend_ctries.png" ///
		*	, replace		
					

		// PART 2: COMPOSITION -------------------------------------------------
		//import summary 	
		qui import excel "${summary}ineqstats_`type'_`unit'.xlsx", ///
			sheet("Composition") firstrow clear	
			
		//merge with shares 
		qui merge 1:1 country year using `tf_`type'', nogen	keep(3)

		//Placebo 
		qui gen tot_sh = 1
		
		//replace 0 for DOM in 2015 for t1_cap by small number for stack graph
		*qui replace t1_cap = t1_cap + 0.0000001 ///
		*	if country == "DOM" & year == 2014

		//Loop over population groups 

		foreach group in $groups {	

			di as result "decomposing group `group' at $S_TIME..."

			preserve
			
				//loop over variables to graph 
				local iter = 1 
				ds `group'_*
				foreach v in `r(varlist)' {
					
					display as text "   preparing variable `v' at $S_TIME"
				
					//percentage 
					qui replace `v' = `v' * 100
					
					//generate stack variables 
					qui gen `v'_a = `group'_sh * `v'
					if "`v'" != "`group'_sh" {
						local st_`group'_`type'_`class'_`unit' ///
							"`v'_a `st_`group'_`type'_`class'_`unit''"
					}
					
					//chose color and legend-label
					foreach w in ///
						"wag" "kap" "cap" "ben" "mix" "mbe" "wmbe" ///
							"mir" "imp" "upr" "pen" "lef" "indg" "hea" ///
							"edu" "oex" {
						if strpos("`v'", "`w'") local v_col ${c_`w'}
						if strpos("`v'", "`w'") local v_lab ${labcom_`w'_`lang'}
					}
					
					//prepare legend
					local ll_`group'_`type'_`class'_`unit' ///
						`ll_`group'_`type'_`class'_`unit'' ///
						label(`iter' "`v_lab'")
					
					//lines to add to plot 
					if "`v'" != "`group'_sh" {
						local alines_`group'_`type'_`class'_`unit' ///
							`alines_`group'_`type'_`class'_`unit'' ///
							(area f_`v'_a year, color(`v_col') lwidth(none))
					}	
						
					//count iterations 
					local iter = `iter' + 1
					local iter_endloop = `iter'
					
				}
			
				//Stack variables  
				display as text "   Stacking areas at $S_TIME"
				qui cap drop f_*
				qui genstack `st_`group'_`type'_`class'_`unit'', gen(f_)
				
				*continue if genstack works only
				if _rc == 0 {
					//handle missing values 
					foreach var in `st_`group'_`type'_`class'_`unit'' {
						qui replace f_`var' = . if f_`var' == 0 
					}
					
					//Graph composition of each country by group
					qui levelsof country if !missing(gini), local(graph_ctries)
					foreach  c in `graph_ctries' { 
					
						display as text "      preparing data for `c' at $S_TIME"
					
						//Legend label for group in loop
						if "`lang'" == "eng" {
							if "`group'" == "b50" local labg "Bottom 50% Share"
							if "`group'" == "m40" local labg "Middle 40% Share"
							if "`group'" == "t10" local labg "Top 10% Share"
							if "`group'" == "t1" local labg "Top 1% Share"
							local ytit "Income share (%)"
						}
						if "`lang'" == "esp" {
							if "`group'" == "b50" local labg "50% más pobre"
							if "`group'" == "m40" local labg "40% del medio"
							if "`group'" == "t10" local labg "10% más rico"
							if "`group'" == "t1" local labg "1% más rico"
							local ytit "Parte del ingreso total (%)"
						}
						
						//add name to legend 
						local iter_`c' = `iter_endloop'
						local ll_`group'_`type'_`class'_`unit' ///
							`ll_`group'_`type'_`class'_`unit'' ///
							label(`iter_`c'' "`labg'")
						
						//count extrapolated values to tag
						local extrapolated ""
						local leg_extra ""
						if ("`type'" == "bfm_norep_pre"){
							qui count if extrap == 1 & country == "`c'"
						} 
						
						//add a line to graph if needed
						if (r(N) != 0 & "`type'" == "bfm_norep_pre") {
							local iter_`c' = `iter_endloop' + 1
							local extrapolated (scatter `group'_sh year ///
								if extrap == 1, msize(small) mcolor(black) ///
								mfcolor(black*0.3))
							local ll_`group'_`type'_`class'_`unit' ///
								`ll_`group'_`type'_`class'_`unit'' ///
								label(`iter_`c'' "Extrapolated value")
						}
						
						//max value
						if inlist("`group'", "b50", "t1")  local maxy = 30
						if "`group'" == "m40" local maxy = 60
						if "`group'" == "t10" local maxy = 70
						if "`group'" == "t10" & "`type'" == "uprofits" {
							local maxy = 100
						}
						if "`group'" == "t1" & "`type'" == "uprofits" {
							local maxy = 100
						} 
						if "`group'" == "tot" local maxy = 100
						
						//mid value
						if inlist("`group'", "b50", "t1") local midy = 5
						if "`group'" == "t1" & "`type'" == "uprofits" {
							local midy = 20
						} 
						if inlist("`group'", "m40", "t10", "tot") {
							local midy = 10
						} 
						
						//add a line for the group's share
						local graph_share (connected `group'_sh year, ///
							msize(small) ///
							color(black) mcolor(black) mfcolor(black)) 
						if ("`group'" == "tot") local graph_share ""
						if ("`group'" == "tot") local extrapolated ""
						
						//graph composition 
						if "`c'" == "DOM" {
							local firsty = 2012
							local midy2 = 2
							local addcond & year >= `firsty'
						} 
						else {
							local firsty = $first_y
							local midy2 = 5
							local addcond 
						}
						
						graph twoway `alines_`group'_`type'_`class'_`unit'' ///
							`graph_share' `extrapolated' ///
							 if country == "`c'" `addcond', xtitle("") ///
							ytitle(/*"`ytit'"*/ "") ///
							ylabel(0(`midy')`maxy', $ylab_opts) ///
							xlabel(`firsty'(`midy2')2020, $xlab_opts) ///
							legend(`ll_`group'_`type'_`class'_`unit'') ///
							$graph_scheme
						qui graph export ///
							"figures/`type'/ineqstats/composition/`group'_`c'_`unit'.pdf" ///
							, replace
						
					}
				}		
			restore
		}	
	}
}

