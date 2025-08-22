/*=============================================================================*
Goal: correct CEPAL surveys with bfmcorr command (Blanchet, Flores, Morgan, 2019)
and save theta coefficients for analysis and further extrapolations
Authors: Mauricio De ROSA, Ignacio FLORES, Marc MORGAN
Date: Jan/2020
*=============================================================================*/

//general settings
clear all

//preliminary
global aux_part  ""preliminary"" 
do "code/Stata/auxiliar/aux_general.do"

//define macros 
if "${bfm_replace}" == "yes" {
	global types " "rep" "
} 	  	
if "${bfm_replace}" == "no" {
	global types " "norep" "
} 	  

// -----------------------------------------------------------------------------

//Loop over countries 
foreach c in  $countries_bfm_02a {
	
	//manage pre/post tax incomes 
	local pf "pos"
	if inlist("`c'", "BRA") local pf "pre" 
	
	//Loop over years 
	forvalues t = $first_y / $last_y {
	
		//define path to files 
		local taxfile "input_data/admin_data/`c'/gpinter_`c'_`t'.xlsx"
		local svyfile "intermediary_data/microdata/raw/`c'/`c'_`t'_raw.dta"
	
		// 1. CHECK WHAT DATA IS AVAILABLE AND REPORT --------------------------
		
		//Check if tax data exists
		cap confirm file `taxfile'
		if !_rc {
			
			//Check if survey data also exists 
			cap confirm file `svyfile'
			if !_rc {
			
				//report existence of files 
				di as result "`c' `t': Both survey and tax data " _continue
				di as result "exist, combining datasets (03a)... " _continue
				di as text "at $S_TIME"
				
				//Get trust region
				qui import excel "input_data/admin_data/directory.xlsx", ///
					sheet("trust") firstrow clear
				qui destring trust, replace	
				qui sum trust if country == "`c'" & year == `t'
				
				//Run code if trust exists 
				if ("`r(mean)'" != "") {
					local trust_`c'_`t' = `r(mean)'
				
					// 2. TREAT RAW SURVEYS ------------------------------------
				
					//locals-bfm-correction
					local raw_weight "_fep"	
					local id "id_hogar"
					local age "edad"
					local taxinc ""
					local y " ${y_`pf'tax_tot} "
					local yrtrust = `trust_`c'_`t''
					if ("`c'" == "BRA") local y " ${y_pretax_tot_bra} "
					if ("`c'" == "PER") local taxinc taxincome(${y_postax_tot_per}) 
					
					//Open raw data
					use `svyfile', clear

					//Ensure weights are constant within household
					qui count if ///
						`raw_weight' == 0 | missing(`raw_weight') | `raw_weight' < 1
					display as result "`c' `t': " _continue  
					display as result "`r(N)' obs " _continue  
					display as result "with null weight"
					qui drop if `raw_weight' == 0
					egen weight2 = mean(`raw_weight'), by(`id')
					cap assert weight2 == `raw_weight'
					
					//Inform change of weights in log
					if r(N) == 0 {
						tempvar w_change
						qui gen `w_change' = (`raw_weight' - weight2) / `raw_weight' * 100
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
					
					//Write definitive weights 
					qui replace `raw_weight' = weight2
					qui drop weight2
					qui drop if missing(`raw_weight') | `raw_weight' < 1
					
					//Save total population
					qui sum `raw_weight' 
					local orig_weights = r(sum)
					
					//Get age groups 
					xtile age_group = `age', nquantiles(10)
						
					// 3. COMBINE SURVEY AND TAX DATA --------------------------
					// loopy loop 
					
					tempfile tf 
					qui save `tf'
					
					foreach type in $types {
					
						// Details for file replace / no-replace distinction
						if ("`type'" == "rep") local command ""
						if ("`type'" == "rep") local ext ""
						if ("`type'" == "norep") local command "noreplace"
						if ("`type'" == "norep") local ext "_norep"
						
						//create main folders 
						local dirpath "intermediary_data/microdata/bfm`ext'_`pf'"
						mata: st_numscalar("exists", direxists(st_local("dirpath")))
						if (scalar(exists) == 0) {
							mkdir "`dirpath'"
							display "Created directory: `dirpath'"
						}
						
						//create main folders 
						local dirpath "output/figures/MP"
						mata: st_numscalar("exists", direxists(st_local("dirpath")))
						if (scalar(exists) == 0) {
							mkdir "`dirpath'"
							display "Created directory: `dirpath'"
						}
						
						//create main folders 
						local dirpath "output/bfm_summary"
						mata: st_numscalar("exists", direxists(st_local("dirpath")))
						if (scalar(exists) == 0) {
							mkdir "`dirpath'"
							display "Created directory: `dirpath'"
						}
						
						//create main folders 
						local dirpath "output/bfm_summary/bfm`ext'_`pf'"
						mata: st_numscalar("exists", direxists(st_local("dirpath")))
						if (scalar(exists) == 0) {
							mkdir "`dirpath'"
							display "Created directory: `dirpath'"
						}
							
						//path to files 
						local corrfile ///
							"intermediary_data/microdata/bfm`ext'_`pf'/`c'_`t'_bfm`ext'_`pf'.dta"
						local MPgraph "output/figures/MP/`c'_`t'_MP.pdf"
						local export_results "output/bfm_summary/bfm`ext'_`pf'/bfm_summary.xlsx"
						local mpfile "output/bfm_summary/bfm`ext'_`pf'/merging_points.xlsx"
											
						//find program
						qui sysdir set PERSONAL "code/Stata/ado/bfm_stata_ado/."
						clear programs
						qui use `tf', clear
						
						local slope = -0.99
						if inlist("`c'", "MEX") local slope = -2
						if "`c'" == "ARG" local slope = -0.5
						
						local thetalimit = ${t_limit}
		
						*exceptions 
						if !inlist("`c'", "") {
							
							//Apply BFM correction 
							/*quietly*/ bfmcorr_experiments using `taxfile', ///
								weight(`raw_weight') income(`y') `taxinc' ///
								households(`id') taxu(i) trust(`yrtrust') ///
								thetalimit(`thetalimit') ///
								holdmargins(sex age_group) ///
								slope(`slope') pen(20) sampletop(0.01) ///
								`command' /*minbracket(1)*/
				
							//save info on unobserved population 
							*local unobs_`c'_`t' = e(unobs_pop)
							local unobs_from_tax_`c'_`t' = e(unobs_from_tax)
							
							//save merging points 
							preserve 	
								//get the data
								tempname theta adj_factors
								qui matrix define `theta' = e(theta)
								qui matrix define `adj_factors' = e(adj_factors)
								local mpoint = e(mergingpoint)
								
								//write it down
								clear
								qui mat list `theta'
								qui svmat `theta', names(v)
								qui rename (v1 v2 v3 v4 v5 v6) ///
									(p big_t small_t antitonic extrap thr)
								qui gen mpoint = `mpoint' in 1
								
								//export it
								qui export excel `mpfile', ///
									firstrow(variables) sheet("`c'`t'") ///
									sheetreplace 	
							restore 
							
							//Save microdata 
							qui save `corrfile', replace 
							
							//postestimation commands 
							/*quietly*/ postbfm biasplot
							qui graph export `MPgraph', replace 	
							postbfm	summarize, export(`export_results') ///
								year(`t') replace		
							
							//save list of country-years
							local ctry_yr_list`ext'_`pf' ///
								"`ctry_yr_list`ext'_`pf'' `c'_`t'"
						}
					}
				
				}
				
				// 4. REPORT DATA-AVAILABILITY FOR NON TREATED YEARS -----------
				
				//Report if trust is missing
				else {
					di as error "Trust region is not defined for " _continue
					di as error "`c' - `t', skipping ... "
				}
			}		
			else {
				di as text "`c' `t': tax data exists but survey does not"
			}		
		}	
		else {	
			cap confirm file `svyfile'		
			if !_rc {
				di as text "`c' `t': Survey data exists, but tax data does not"
			}		
			else {
				di as text "`c' `t': Both survey and tax data" _continue
				di as text " do not exist for this year"
			}
		}
	}	
}

//record list of countries and years corrected 
foreach type in $types {
	
	foreach pf in "pre" "pos" {
		
		// Details for file replace / no-replace distinction
		if ("`type'" == "rep") local command ""
		if ("`type'" == "rep") local ext ""
		if ("`type'" == "norep") local command "noreplace"
		if ("`type'" == "norep") local ext "_norep"

		//start from scratch 
		clear
		local obs_mp = wordcount("`ctry_yr_list`ext'_`pf''")
		set obs `obs_mp'
		qui gen country_year = ""
		
		//loop over values 
		forvalues n = 1 / `obs_mp' {
			local wd_`n': word `n' of `ctry_yr_list`ext'_`pf''
			qui replace country_year = "`wd_`n''" in `n'
		}
		
		//save results 
		local mpfile "output/bfm_summary/bfm`ext'_`pf'/merging_points.xlsx"
		qui export excel `mpfile', firstrow(variables) ///
			sheet("country_years") sheetreplace keepcellfmt
				
		//report unobserved population 
		qui split country_year, parse(_) gen(v)	
		qui rename (v1 v2) (country year)
		qui gen unobs = . 
		qui drop country_year
		qui levelsof country, local(countries) clean
		foreach c in `countries' {
			qui levelsof year if country == "`c'", local(`c'_years) clean
			foreach y in ``c'_years' {
				qui replace unobs = `unobs_from_tax_`c'_`y'' ///
					if country == "`c'" & year == "`y'"
			}
		}
		
		//save results 
		local mpfile "output/bfm_summary/bfm`ext'_`pf'/merging_points.xlsx"
		qui export excel `mpfile', firstrow(variables) ///
			sheet("unobserved") sheetreplace keepcellfmt
			
	}
}
