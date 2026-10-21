// Direct and runner execution share country selection and staged publication.
args dina_requested_countries
local dina_country_worker : environment DINA_COUNTRY_WORKER
if "`dina_country_worker'" != "03e-extrapolate-bfm-correction" {
    do "code/Stata/auxiliar/dina_country_task.do" "03e-extrapolate-bfm-correction" "`dina_requested_countries'"
    exit
}

/*=============================================================================*
Goal: Run bfm with extrapolated correction factors 
Authors: Mauricio De ROSA, Ignacio FLORES, Marc MORGAN
*=============================================================================*/

//General settings
clear all

//preliminary
global aux_part  ""preliminary"" 
qui do "code/Stata/auxiliar/aux_general.do"
local ext "_norep"
global extrap_countries ""
foreach c in $all_countries {
	global `c'_extrap_years ""
}
// -----------------------------------------------------------------------------


//Get list of countries and years to adjust
preserve 
	qui import excel "intermediary_data/weight_adjusters/index.xlsx", ///
		sheet("country_years_03d") firstrow clear		
	capture confirm string variable country_year
	if _rc quietly tostring country_year, replace
	qui drop if inlist(country_year, "", ".")
	qui assert regexm(country_year, "^[A-Z][A-Z][A-Z]_[0-9][0-9][0-9][0-9]$")
	qui gen str3 v1 = substr(country_year, 1, 3)
	qui gen str4 v2 = substr(country_year, 5, 4)
	local selected_countries : subinstr global dina_run_countries `"""' "", all
	qui keep if strpos(" `selected_countries' ", " " + v1 + " ")
	qui levelsof v1, local(countries) clean
	global extrap_countries `countries'
	foreach c in $extrap_countries {
		di as result "`c'"
		levelsof v2 if v1 == "`c'", local(`c'_years) clean
		global `c'_extrap_years ``c'_years'
	}
restore 

if trim("$extrap_countries") == "" {
	foreach c in $dina_run_countries {
		dina_coverage, country("`c'") step("extrapolated_bfm") status("not_applicable")
	}
	// Replace an older success list with an explicit empty current index.
	clear
	qui set obs 1
	qui gen str1 country_year = ""
	qui gen extrap = .
	qui gen fail = .
	qui export excel "${dina_output_root}/intermediary_data/weight_adjusters/index.xlsx", ///
		firstrow(variables) sheet("country_years_03e") sheetreplace keepcellfmt
	di as text "03e: no extrapolation cases in the current 03d index."
	exit 0
}

//memorize merging point from closest year 
foreach c in $extrap_countries {
	foreach y in $`c'_extrap_years {
		qui import excel "output/bfm_summary/all_thetas.xlsx", ///
			sheet("panel") clear firstrow
		qui keep if country == "`c'"
		if _N == 0 {
			di as error "03e: `c' has extrapolation cases but no validated observed merging points."
			exit 459
		}
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
		if missing(`mp_`c'_`y'') | `mp_`c'_`y'' <= 0 | `mp_`c'_`y'' >= 1 {
			di as error "03e: invalid observed merging point for `c' `y'."
			exit 459
		}
		local mp_`c'_`y' : display %6.5f `mp_`c'_`y''
		if ("`c'" == "URY" & `y' <= 2008) local  mp_`c'_`y' = 0.999
		di as result "`c' `y': `imp' (`mp_`c'_`y'')"
	}
}

//Loop over countries 
foreach c in $extrap_countries {
	
	//Loop over years 
	foreach t in ${`c'_extrap_years} {
	
		dina_coverage, country("`c'") year(`t') step("extrapolated_bfm") status("expected")
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
			"${dina_output_root}/intermediary_data/microdata/bfm`ext'_`pf'/`c'_`t'_bfm`ext'_`pf'.dta"
		local MPgraph "${dina_output_root}/output/figures/MP/`c'_`t'_MP_extrap.pdf"
		local export_results "${dina_output_root}/output/bfm_summary/bfm`ext'_`pf'/bfm_summary_`c'_extrap.xlsx"
		local mpfile "${dina_output_root}/output/bfm_summary/bfm`ext'_`pf'/merging_points.xlsx"
		 
		//find program
		sysdir set PERSONAL "code/Stata/ado/bfm_stata_ado/."
		clear programs
		
		//get trust region
		qui import excel `adjfile', firstrow clear
		// The accepted panel expresses percentiles as rounded decimals, while
		// Stata-exported adjusters can retain float storage (e.g. .997 becomes
		// .996999979). Use the same stored value for an equivalent grid point;
		// otherwise BFM would wrongly discard that bracket and move one higher.
		// This tolerance is below the finest 0.00001 percentile-grid spacing.
		local dina_mp_requested = `mp_`c'_`t''
		qui count if abs(p - `dina_mp_requested') <= 0.0000001
		if r(N) > 1 {
			di as error "03e: ambiguous merging-point grid for `c' `t'."
			exit 459
		}
		if r(N) == 1 {
			qui sum p if abs(p - `dina_mp_requested') <= 0.0000001
			local mp_`c'_`t' : display %21.17g r(mean)
		}
		qui sum p 
		local min_p_`c'_`t' = r(min) - 0.05
		qui use `tf', clear
		
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
		capture noisily bfmcorr_experiments using `adjfile', weight(`weight') ///
			income(`y') `taxinc' households(`id') taxu(i) ///
			trust(`min_p_`c'_`t'') mergingpoint(`mp_`c'_`t'') ///
			thetalimit(/*${t_limit}*/20) ///
			holdmargins(sex age_group) slope(`slope') pen(`pen') /// 
			sampletop(0.01) noreplace	/*minbracket(1)*/
				
		local dina_bfm_rc = _rc
		// Retain the error code and report every failed case explicitly.
		if `dina_bfm_rc' == 0 {
		
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
					firstrow(variables) sheet("`c'`t'_extrap") ///
					sheetreplace 	
			restore 
			
			//Save microdata 
			save `corrfile', replace 
			
			//postestimation commands 
			postbfm biasplot
			graph export `MPgraph', replace 
			postbfm	summarize, export(`export_results') ///
				year(`t') replace		
			
			dina_coverage, country("`c'") year(`t') step("extrapolated_bfm") status("success")
			//save list of country-years
			local ctry_yr_list`ext' "`ctry_yr_list`ext'' `c'_`t'"
		}
		else {
			dina_coverage, country("`c'") year(`t') step("extrapolated_bfm") status("failed")
			//log failed attempts 
			di as error "03e: `c' `t' failed to be adjusted (Stata r(`dina_bfm_rc'))."
			
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
local mpfile "${dina_output_root}/intermediary_data/weight_adjusters/index.xlsx"
qui export excel `mpfile', firstrow(variables) ///
	sheet("country_years_03e") sheetreplace keepcellfmt
	
di as result "successful cases: " _continue 
di as text "`ctry_yr_list`ext''" 
di as result "failed cases: " _continue 
di as text "`fail_`ext''" 	

// Persist the complete success/failure index before stopping a failed run.
if wordcount("`fail_`ext''") > 0 {
	di as error "03e is incomplete; failed cases: `fail_`ext''"
	di as error "See the errors above and country_years_03e in the weight-adjuster index."
	exit 459
}
