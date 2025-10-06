/*=============================================================================*
Goal: Run bfm with extrapolated correction factors 
Authors: Mauricio De ROSA, Ignacio FLORES, Marc MORGAN
*=============================================================================*/

//General settings
clear all

//to replace or not to replace
if "${bfm_replace}" == "yes" local ext ""	  	
if "${bfm_replace}" == "no" local ext "_norep"

//preliminary
global aux_part  ""preliminary"" 
qui do "code/Stata/auxiliar/aux_general.do"
// -----------------------------------------------------------------------------


//Get list of countries and years to adjust
preserve 
	qui import excel "intermediary_data/weight_adjusters/index.xlsx", ///
		sheet("country_years_03d") firstrow clear		
	qui ds 
	qui split `r(varlist)', parse(_) gen(v)	
	qui levelsof v1, local(countries) clean
	global extrap_countries `countries'
	foreach c in $extrap_countries {
		di as result "`c'"
		levelsof v2 if v1 == "`c'", local(`c'_years) clean
		global `c'_extrap_years ``c'_years'
	}
restore 

//memorize merging point from closest year 
foreach c in $extrap_countries {
	foreach y in $`c'_extrap_years {
		qui import excel "output/bfm_summary/all_thetas.xlsx", ///
			sheet("panel") clear firstrow
		qui keep if country == "`c'"
		cap drop distance_y y
		qui gen y = `y'
		qui destring year, replace 
		qui gen distance_y = abs(y - year)
		qui sum distance_y  
		qui keep if distance_y == r(min)
		qui sum year 
		local imp = r(min)
		qui keep if year == `imp'
		qui sum mpoint 
		local mp_`c'_`y' = r(mean) / 100
		local mp_`c'_`y' : display %6.5f `mp_`c'_`y''
		if ("`c'" == "URY" & `y' <= 2008) local  mp_`c'_`y' = 0.999
		di as result "`c' `y': `imp' (`mp_`c'_`y'')"
	}
}

//Loop over countries 
foreach c in $extrap_countries {
	
	//Loop over years 
	foreach t in ${`c'_extrap_years} {
	
		//Define file paths
		local adjfile "intermediary_data/weight_adjusters/`c'`t'.xlsx"
		local svyfile "intermediary_data/microdata/raw/`c'/`c'_`t'_raw.dta"
		// 1. TREAT RAW SURVEYS ------------------------------------------------
	
		//locals
		local weight "_fep"	
		local id "id_hogar"
		local age "edad"
		local taxinc ""
		local y " ${y_postax_tot} "
		if ("`c'" == "BRA") local y " ${y_pretax_tot_bra} "
		if ("`c'" == "CRI") local y " ${y_pretax_tot} "
		if ("`c'" == "PER") local y " ${y_postax_tot_per} "
		if ("`c'" == "PER") local taxinc taxincome(${y_postax_tot_per}) 
		if ("`c'" == "MEX") {
			local taxinc taxincome(${y_postax_formal_wage})
			if `t' == 2000 local taxinc taxincome(${y_postax_wag})
		} 
		if ("`c'" == "ARG") {
			local y "ind_pos_wag"
			local weight "n_fep"
			local taxinc taxincome(${y_postax_private_wage})
		} 
		
		//Open raw data
		use `svyfile', clear
		
		//Check weights are constant within household
		qui count if ///
			`weight' == 0 | missing(`weight') | `weight' < 1
		display as result "`c' - `t': " _continue  
		display as result "`r(N)' obs " _continue  
		display as result "with null weight (dropped)"
		qui drop if `weight' == 0
		egen weight2 = mean(`weight'), by(`id')
		cap assert weight2 == `weight'
		
		//Inform change of weights in log
		if r(N) == 0 {
			tempvar w_change
			qui gen `w_change' = (`weight' - weight2) / `weight' * 100
			qui sum `w_change' 
			local min_ch = round(`r(min)', 1)
			local max_ch = round(`r(max)', 1)
			di as text "Weights not constant within household in " _continue
			di as text "raw survey for `c' - `t'"
			di as result "...the average weight of the household " _continue
			di as result "was attributed to its members: weights " _continue
			di as result "incresed/decreased from " _continue
			if (`min_ch' < -1 | `max_ch' > 1) di as error "`min_ch'% to `max_ch'% " _continue
			if (`min_ch' > -1 | `max_ch' < 1) di as result "`min_ch'% to `max_ch'% " _continue
			di as result "(rounded to the nearest integer)"
		}
		
		//Write harmonised weights 
		qui replace `weight' = weight2
		qui drop weight2
		qui drop if missing(`weight') | `weight' < 1
		
		//Save total population
		qui sum `weight' 
		local orig_weights = r(sum)
		
		//Get age groups and restrict population 
		xtile age_group = `age', nquantiles(10)
		
		// 2. COMBINE SURVEY AND TAX DATA --------------------------
	
		//details
		tempfile tf 
		qui save `tf'
			
		//path to files
		local pf "pos"
		if inlist("`c'", "BRA", "CRI") local pf "pre"
		local corrfile ///
			"intermediary_data/microdata/bfm`ext'_`pf'/`c'_`t'_bfm`ext'_`pf'.dta"
		local MPgraph "output/figures/MP/`c'_`t'_MP_extrap.pdf"
		local export_results "output/bfm_summary/bfm`ext'_`pf'/bfm_summary.xlsx"
		local mpfile "output/bfm_summary/bfm`ext'_`pf'/merging_points.xlsx"
		 
		//find program
		sysdir set PERSONAL "code/Stata/ado/bfm_stata_ado/."
		clear programs
		
		//get trust region
		qui import excel `adjfile', firstrow clear
		qui sum p 
		local min_p_`c'_`t' = r(min) - 0.05
		qui use `tf', clear
		
		//to replace or not to replace?
		if ("`ext'" == "_norep") local addoption "noreplace"
		
		local slope = -0.99
		local pen = 20
		if "`c'" == "ECU" local slope = -0.6
		if inlist("`c'", "MEX", "PER", "COL") local slope = -2
		if "`c'" == "MEX" local min_p_`c'_`t' = 0.85
		if "`c'" == "ARG" local slope = -0.5
		if inlist("`c'", "URY", "COL") {
			local min_p_`c'_`t' = .9
		} 
		
		//Apply BFM correction 
		cap bfmcorr_experiments using `adjfile', weight(`weight') ///
			income(`y') `taxinc' households(`id') taxu(i) ///
			trust(`min_p_`c'_`t'') mergingpoint(`mp_`c'_`t'') ///
			thetalimit(/*${t_limit}*/20) ///
			holdmargins(sex age_group) slope(`slope') pen(`pen') /// 
			sampletop(0.01) `addoption'	/*minbracket(1)*/
				
		//check if the correction was made to continue
		if _rc == 0 {
		
			//save merging points 
			preserve 	
				//get the data
				tempname theta adj_factors
				matrix define `theta' = e(theta)
				matrix define `adj_factors' = e(adj_factors)
				local mpoint = e(mergingpoint)
				
				//write it down
				clear
				mat list `theta'
				svmat `theta', names(v)
				qui rename (v1 v2 v3 v4 v5 v6) ///
					(p big_t small_t antitonic extrap thr)
				qui gen mpoint = `mpoint' in 1
				
				//export it
				qui export excel `mpfile', ///
					firstrow(variables) sheet("`c'`t'") ///
					sheetreplace 	
			restore 
			
			//Save microdata 
			save `corrfile', replace 
			
			//postestimation commands 
			postbfm biasplot
			graph export `MPgraph', replace 
			postbfm	summarize, export(`export_results') ///
				year(`t') replace		
			
			//save list of country-years
			local ctry_yr_list`ext' "`ctry_yr_list`ext'' `c'_`t'"
		}
		else {
			//log failed attempts 
			di as error "`c' `t' failed to be adjusted"
			
			//store list of failed cases 
			local fail_`ext' "`fail_`ext'' `c'_`t'"
		}
	}	
}

//record list of countries and years corrected 

//start from scratch 
clear
local obs_mp = wordcount("`ctry_yr_list`ext''") + wordcount("`fail_`ext''")
set obs `obs_mp'
qui gen country_year = ""
qui gen extrap = . 
qui gen fail = . 

//loop over values 
forvalues n = 1 / `obs_mp' {
	if `n' <= wordcount("`ctry_yr_list`ext''") {
		local wd_`n': word `n' of `ctry_yr_list`ext''
		qui replace extrap = 1 in `n'
		qui replace fail = 0 in `n'
	}
	else {
		local n2 = `n' - wordcount("`ctry_yr_list`ext''")
		local wd_`n': word `n2' of `fail_`ext''
		qui replace extrap = 0 in `n'
		qui replace fail = 1 in `n'
	}
	qui replace country_year = "`wd_`n''" in `n'	
}

//save results 
local mpfile "intermediary_data/weight_adjusters/index.xlsx"
qui export excel `mpfile', firstrow(variables) ///
	sheet("country_years_03e") sheetreplace keepcellfmt
	
di as result "successful cases: " _continue 
di as text "`ctry_yr_list`ext''" 
di as result "failed cases: " _continue 
di as text "`fail_`ext''" 	

