
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

//Paths to files 
global aux_part  ""preliminary"" 
qui do "code/Do-files/auxiliar/aux_general.do"

//define language 
local lang $lang

// 0. Get macro data  ----------------------------------------------------------

//General 
qui use country iso year TOT_B5g_wid TOT_B5n_wid TOT_K1_wid priceindex npop* ///
	using ${sna_wid_merged} if !missing(country, TOT_B5g_wid), clear 	
duplicates drop 	

//define current and real values 
qui rename TOT_B5g_wid TOT_B5g_wid_curr
qui rename TOT_B5n_wid TOT_B5n_wid_curr
qui gen TOT_B5g_wid_real = TOT_B5g_wid_curr / priceindex
qui gen TOT_B5n_wid_real = TOT_B5n_wid_curr / priceindex

//save 
tempfile tf_wid
qui save `tf_wid', replace 

*other inflation and xrates
qui import excel ${inflation_data}, firstrow sheet("inflation-xrates") clear
tempfile tf_infl_xrates
qui save `tf_infl_xrates', replace 

// 1. Merge all steps ----------------------------------------------------------
global units " ${unit_list} "
foreach unit in $units {
	//Merge steps
	local iter = 1 
	foreach type in $steps_06d {

		//Import file
		qui import excel "${summary}ineqstats_`type'_`unit'.xlsx", ///
			sheet("Summary") firstrow clear	
			
		*qui drop if year >= 2021	
			
		//Add suffix 
		qui ds country year, not 
		local vars "`r(varlist)'"
		foreach var in `vars' {
			qui rename `var' `var'_`type'
		}
		
		//merge 
		if (`iter' != 1) {
			qui mer 1:1 country year using `tf_aux_`class'_`unit'', nogen
		} 
		
		//Save for later
		tempfile tf_aux_`class'_`unit'
		qui save `tf_aux_`class'_`unit'', replace 
		local iter = 0 
	}

	//Get to percentages 
	qui ds country year average* adpop*, not 
	foreach var in `r(varlist)' {
		qui replace `var' = `var' * 100
	}
		
	//get inflation indexes and ppp xrates 	
	qui merge 1:1 country year using `tf_infl_xrates', keep(2 3) nogen 
	qui levelsof country, local(countries) 
	foreach c in `countries' {
		qui levelsof xppp_eur if country == "`c'" & year == 2019, ///
			local(xppp_2019_`c')
	}

	//merge wid data 
	qui merge 1:1 country year using `tf_wid', keep(3) nogen

	//convert old currencies 
	global aux_part  ""old_currencies"" 
	qui do "code/Do-files/auxiliar/aux_general.do"

	*collect info for special case (ECU)
	preserve 
		quietly import excel $wb_xrates, sheet("ECU") clear firstrow
		qui destring year, replace 
		quietly levelsof year, local(xr_yrs_ecu) clean 	
		foreach z in `xr_yrs_ecu' {
			quietly sum rate if year == `z' 
			local xr_ecu_`z' = r(mean)
		}
	restore 

	qui gen anninc_svypop = TOT_B5n_wid_real / adpop_natinc
	qui gen agninc_svypop = TOT_B5g_wid_real / adpop_natinc

	foreach s in $steps_06d {

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
			qui replace average_`s' = (average_`s') / `xr_ecu_`z'' ///
				if year == `z' & country == "ECU" & year < 2000
		}
			
		//get real 	
		qui replace average_`s' = (average_`s') / priceindex ///
			if !inlist(country, "ECU")
		
		//get ppp eur 
		if "`s'" == "natinc"{
			qui gen agninc_m = .
			qui gen anninc_m = .
		} 
		foreach c in `countries' {
			qui replace  average_`s' = (average_`s'/12) / `xppp_2019_`c'' ///
				if country == "`c'" 
			if "`s'" == "natinc" {
				qui replace agninc_m = (agninc_svypop/12) / `xppp_2019_`c'' ///
					if country == "`c'" 
				qui replace anninc_m = (anninc_svypop/12) / `xppp_2019_`c'' ///
					if country == "`c'" 	
			}		
		}
	}
	
	*save data for transparency index 
	local date = subinstr("$S_DATE", " ", "", .)
	foreach s in "raw" "bfm_norep_pre" "rescaled" {
		tempvar aux_`s'
		qui egen `aux_`s'' = rowtotal(*_`s')
		qui gen `s' = 1 if `aux_`s'' != 0 
	}
	qui replace bfm_norep_pre = . ///
		if !missing(extrap) & !missing(bfm_norep_pre)
	qui rename (raw bfm_norep_pre rescaled) (Survey AdminData NatAccounts)
	qui export excel country year Survey AdminData NatAccounts using ///
			"results/transparency_info_latam_`date'.xlsx", replace firstrow(variables)
			
			
	//2. Compare estimates ---------------------------------------------------------

	//Graph basics  
	global aux_part  ""graph_basics""
	qui do "code/Do-files/auxiliar/aux_general.do"

	//Bring country/years with extrapolated SNA scaling factors 
	merge 1:m country year using "results/extrap_sna.dta", nogen
	qui duplicates drop 

	//Exclude countries with inconsistencies
	qui do "code/Do-files/auxiliar/aux_exclude_ctries.do"
	qui drop if exclude == 1 
	drop exclude 

	//loop over variables and countries 
	foreach var in `vars' {
		foreach c in $all_countries {
		display as text "working with `var' for `c'"
		
			//max value
			if strpos("`var'", "b50") local maxy = 20 
			if strpos("`var'", "m40") local maxy = 60
			if strpos("`var'", "t1") local maxy = 40
			if strpos("`var'", "average") local maxy = 1600
			if strpos("`var'", "gini") | strpos("`var'", "t10")  local maxy = 80 
			
			//mid value
			 local midy = 10 
			 if strpos("`var'", "b50") local midy = 5 
			 if strpos("`var'", "average") local midy = 400 
			
			//min malue 
			local miny = 0
			if strpos("`var'", "gini") local miny = 40
			if strpos("`var'", "t1") | strpos("`var'", "average")  local miny = 0 
			if strpos("`var'", "m40") local miny = 10
			if strpos("`var'", "t10") local miny = 30 
			
			//Legend label for group in loop
			if "`lang'" == "eng" {
				if strpos("`var'", "b50") local labg "Bottom 50% Share"
				if strpos("`var'", "m40") local labg "Middle 40% Share"
				if strpos("`var'", "t1") local labg "Top 1% Share"
				if strpos("`var'", "t10") local labg "Top 10% Share"
				if strpos("`var'", "gini") local labg "Gini coefficient"
				if strpos("`var'", "average") local labg "Average income, EUR PPP 2019 /month"	
			}
			if "`lang'" == "esp" {
				if strpos("`var'", "b50") local labg "Parte del 50% más pobre"
				if strpos("`var'", "m40") local labg "Parte del 40% del medio"
				if strpos("`var'", "t1") local labg "Parte del 1% más rico"
				if strpos("`var'", "t10") local labg "Parte del 10% más rico"
				if strpos("`var'", "gini") local labg "Coeficiente de Gini"
				if strpos("`var'", "average") local labg "Ingreso promedio, EUR PPP 2019 /mes"	
			}
			
			//define main attributes of graph 
			local iter = 1 
			//loop over steps
			foreach s in $steps_06d {
				if ("``c'_dum'" != "graph") local `c'_dum "ignore"
				qui count if !missing(`var'_`s') & country == "`c'" 
				if r(N) != 0 {
					local `c'_dum "graph" 
					//details 
					local s1 = substr("`s'", 1, 3)
					local s2 "c_`s1'"
					//graph lines 
					local mlines_`var'_`c'_`unit' `mlines_`var'_`c'_`unit'' ///
						(connected `var'_`s' year /*if exclude==0*/, ///
						lcolor($`s2') msymbol(O) mfcolor($`s2') mcolor($`s2')) 	
					//legend lines 	
					local  mlegend_`var'_`c'_`unit' ///
						`mlegend_`var'_`c'_`unit'' label(`iter' "${lab_`s1'_`lang'}")
			
					//count iterations 
					local odr_`v'_`c'_`unit' `odr_`v'_`c'_`unit'' `iter' 
					local iter = `iter' + 1
				}
			}
			
			if "``c'_dum'" == "graph" {
				//compare with wid data 
				local tline ""
				if "`var'" == "average" local tline (line agninc_m year, lcolor(black))
				//Graph 
				graph twoway `mlines_`var'_`c'_`unit'' `tline' if country == "`c'" ///
					,ytitle("`labg'") xtitle("") ///
					ylabel(`miny'(`midy')`maxy', $ylab_opts) ///
					xlabel(${first_y}(5)2020, $xlab_opts) ///
					legend(/*`mlegend_`var'_`c'_`unit'' symxsize(4pt)*/ off) ///
					${graph_scheme} 	
				qui graph export ///
					"figures/compare_steps/pre_vs_pos/`c'_`var'_`unit'.pdf", replace
				
				//legend	
				if ("`c'" == "URY" & "`var'" == "gini") {
					//Graph 
					graph twoway `mlines_`var'_`c'_`unit'' ///
						if country == "`c'" ///
						,ytitle("`labg'") xtitle("") ///
						ylabel(`miny'(`midy')`maxy', $ylab_opts) ///
						xlabel(${first_y}(5)2020, $xlab_opts) ///
						${graph_scheme} ///
						legend(`mlegend_`var'_`c'_`unit'' ///
						order(`odr_`v'_`c'_`unit''))				
						qui graph export ///
						"figures/compare_steps/pre_vs_pos/legend.pdf", replace
				
				}	
			}
		}
	}
}

