/*=============================================================================*
Goal: correct CEPAL surveys with bfmcorr command for cases with tax and wage 
data and save theta coefficients for analysis and further extrapolations
Authors: Mauricio De ROSA, Ignacio FLORES, Marc MORGAN
Date: Feb/2020
Stage 1: wages
Stage 2: diverse
*=============================================================================*/

//general settings
clear all

//preliminary
global aux_part  ""preliminary"" 
quietly do "code/Stata/auxiliar/aux_general.do"

//define macros 
if "${bfm_replace}" == "yes" {
	global types " "rep" "
} 	  	
if "${bfm_replace}" == "no" {
	global types " "norep" "
} 	  	

// -----------------------------------------------------------------------------

//Loop over countries 
foreach c in $countries_2stage {
	
	local pf "pos"
	if inlist("`c'", "CRI") local pf "pre"
	
	//Loop over years 
	foreach t in ${years_`c'} {
			
		//Define file paths
		local taxfile "input_data/admin_data/`c'/diverse_`c'_`t'.xlsx"
		local svyfile "intermediary_data/microdata/raw/`c'/`c'_`t'_raw.dta"
		local wagefile "input_data/admin_data/`c'/wage_`c'_`t'.xlsx"
		
		// 1. CHECK WHAT DATA IS AVAILABLE AND REPORT --------------------------
		
		//Check if tax data exists
		cap confirm file `taxfile' 
		local fileornot  = _rc 
		if (`fileornot' != 0) {
			cap confirm file `wagefile'
			local fileornot = _rc
		}
		
		di as result "`c' `t' - fileornot: `fileornot'"
		
		if (`fileornot' == 0) {
			
			//Check if survey data also exists 
			cap confirm file `svyfile'
			if !_rc {
			
				di as result "`c' `t': Both survey and tax data " _continue
				di as result "exist, combining datasets (02b)... " _continue
				di as text "at $S_TIME"

				//Get trust region
				quietly import excel "input_data/admin_data/directory.xlsx", ///
					sheet("trust") firstrow clear
				quietly destring trust trust_wages, replace	
				quietly sum trust if country == "`c'" & year == `t'
				local trust_`c'_`t' = r(mean)
				
				//Run code if trust exists 
				quietly sum trust_wages if country == "`c'" & year == `t'
				if ("`r(mean)'" != "") {
					local trustw_`c'_`t' = `r(mean)'

				
					// 2. TREAT RAW SURVEYS ------------------------------------
				
					//locals
					local id "id_hogar"
					local age "edad"
					local yrtrust = `trust_`c'_`t''
					local yrtrustw = `trustw_`c'_`t''
					local w ${y_`pf'tax_formal_wage}
					local raw_weight "_fep"	
					if ("`c'" == "ARG") {
						local raw_weight "n_fep"
						local w ${y_`pf'tax_private_wage}
					}
					if ("`c'" == "SLV") local w ${y_`pf'tax_wag}
					if ("`c'" == "CRI") local w ${y_`pf'tax_formal_wage_cri}
						
					//Open raw data
					use `svyfile', clear

					//Define diverse (non-wage) income concepts 
					if ("`c'" == "SLV") {
						quietly gen y_div = 0
						quietly replace y_div = ${y_`pf'tax_tot} ///
							if (${y_`pf'tax_tot} > ind_`pf'_wag ) 
					}
					if ("`c'" == "MEX") {
						quietly gen y_div = 0
						quietly replace y_div = ${y_`pf'tax_tot}
					}
					if ("`c'" == "CRI") {
						quietly gen y_div = ind_`pf'_mix + ind_`pf'_cap 
					}
					local div "y_div"
					
					//Ensure weights are constant within household
					quietly count if ///
						`raw_weight' == 0 | missing(`raw_weight') ///
						| `raw_weight' < 1
					display as text "   -" _continue  
					display as text "`r(N)' obs " _continue  
					display as text "with null weight"
					quietly drop if `raw_weight' == 0
					egen weight2 = mean(`raw_weight'), by(`id')
					cap assert weight2 == `raw_weight'
					
					//Inform change of weights in log
					if r(N) == 0 {
						tempvar w_change
						quietly gen `w_change' = ///
							(`raw_weight' - weight2) / `raw_weight' * 100
						quietly sum `w_change' 
						local min_ch = round(`r(min)', 1)
						local max_ch = round(`r(max)', 1)
						di as text ///
							"Weights not constant within household in " ///
							_continue
						di as text "raw survey for `c' - `t'"
						di as result ///
							"...the average weight of the household " _continue
						di as result ///
							"was attributed to its members: weights " _continue
						di as result "incresed/decreased from " _continue
						if (`min_ch' < -1 | `max_ch' > 1) di as error "`min_ch'% to `max_ch'% " _continue
						if (`min_ch' > -1 | `max_ch' < 1) di as result "`min_ch'% to `max_ch'% " _continue
						di as result "(rounded to the nearest integer)"
					}
					
					//Write definitive weights 
					quietly replace `raw_weight' = weight2
					quietly drop weight2
					quietly drop if `raw_weight'==. | `raw_weight' < 1
					
					//Save total population
					quietly sum `raw_weight' 
					local orig_weights = r(sum)
					di as text "Population in raw survey: " ///
						round(`orig_weights' / 1000000, 0.1) " million"
					
					//Get age groups
					xtile age_group = `age', nquantiles(10)
									
					// 3. COMBINE SURVEY AND TAX DATA --------------------------
					// loopy loop 
					
					tempfile tf 
					quietly save `tf'
					
					foreach type in $types {
					
						// Details for file replace / no-replace distinction
						if ("`type'" == "rep") local command ""
						if ("`type'" == "rep") local ext ""
						if ("`type'" == "norep") local command "noreplace"
						if ("`type'" == "norep") local ext "_norep"
						
							
						//path to files 
						local corrfile ///
							"intermediary_data/microdata/bfm`ext'_`pf'/`c'_`t'_bfm`ext'_`pf'.dta"				
						local MPgraph_wages ///
							"output/figures/MP/`c'_`t'_MP.pdf"	
						local MPgraph "output/figures/MP/`c'_`t'_MP.pdf"					
						local export_results_wages ///
							"output/bfm_summary/bfm`ext'_`pf'/bfm_summary_wages_`c'.xlsx"
						local export_results "output/bfm_summary/bfm`ext'_`pf'/bfm_summary_`c'.xlsx"		
						local mpfile_wages ///
							"output/bfm_summary/bfm`ext'_`pf'/merging_points_wages.xlsx"
						local mpfile "output/bfm_summary/bfm`ext'_`pf'/merging_points.xlsx"
						
						//exceptions (only 1st stage)
						if inlist("`c'", "ARG", "MEX") {
							local export_results_wages "`export_results'"
							local mpfile_wages "`mpfile'"
							local MPgraph_wages "`MPgraph'"
						} 
											
						//find program
						sysdir set PERSONAL "code/Stata/ado/bfm_stata_ado/."
						clear programs
						quietly use `tf', clear					
												
						//3.1 Apply 1st-stage BFM correction (wages) -----------
						
						di as result "adjusting wage distribution..."
						`capornot' bfmcorr_experiments using `wagefile', ///
							weight(`raw_weight') income(ind_`pf'_wag) ///
							taxincome(`w') households(`id') taxu(i) ///
							trust(`yrtrustw') thetalimit(${t_limit}) ///
							holdmargins(sexo age_group) /*slope(-1)*/ ///
							pen(50) sampletop(0.01) `command' 	
								
						cap assert _N == 0
						if _rc!= 0 {
						
							//save merging points 
							preserve 	
								//get the data
								tempname theta_w adj_factors_w
								matrix define `theta_w' = e(theta)
								matrix define `adj_factors_w' = e(adj_factors)
								local mpoint_w = e(mergingpoint)
								
								//write it down
								clear
								mat list `theta_w'
								svmat `theta_w', names(v)
								quietly rename (v1 v2 v3 v4 v5 v6) ///
									(p big_t small_t antitonic extrap thr)
								quietly gen mpoint = `mpoint_w' in 1
								
								//export it
								qui export excel `mpfile_wages', ///
									firstrow(variables) sheet("`c'`t'") ///
									sheetreplace 
								
							restore 
							
							//Save microdata 
							save `tf', replace 
							
							//save list of country-years
							local ctry_yr_list`ext' "`ctry_yr_list`ext'' `c'_`t'"
							
							//if done with 1st stage 
							if inlist("`c'", "ARG", "MEX") save `corrfile', replace
							
							//postestimation commands 
							postbfm biasplot
							graph export `MPgraph_wages', replace 
							postbfm	summarize, export(`export_results_wages') ///
								year(`t') replace		
						
							//3.2. 2nd-stage BFM correction (diverse inc.) ----
							
							if !inlist("`c'", "ARG", "MEX") {
								quietly use `tf', clear
							
								// change new variable names
								foreach _var in _weight _hid _pid _factor _expand _expanded_weight {
									cap rename `_var' w`_var'
								}
								
								local cor_weight "w_weight"
								gen w_correction = . 
								tostring _correction, gen(_correction1)
								replace w_correction = 1 if _correction1 == "1"
								replace w_correction = 2 if _correction1 == "2"
								label define w_correction 1 "reweighted" 2 "replaced"
								label values w_correction w_correction
								label var w_correction "Correction type (wage)"
								label var w_weight "corrected survey weight (wage)"
								label var w_factor "income adjustment factor (wage)"
								label var w_hid "household ID (wage)"
								label var w_pid "personal ID (wage)"
								drop _correction _correction1
								
								//Write definitive weights 
								quietly drop if `cor_weight' == 0
								egen weight2 = mean(`cor_weight'), by(`id')
								cap assert weight2 == `cor_weight'
								quietly replace `cor_weight' = weight2
								quietly drop weight2
								quietly drop if `cor_weight'==. | `cor_weight' < 1
												
								//dummy for wage distribution
								quietly gen wage = 0
								quietly replace wage = 1 if (`w' > 0)
								
								di as result "adjusting diverse distribution..."
								
								bfmcorr_experiments using `taxfile', ///
									weight(`cor_weight') income(`div') ///
									households(w_hid) taxu(i) ///
									trust(`yrtrust') ///
									holdmargins(sexo age_group wage) ///
									thetalimit(${t_limit}) /*slope(-1)*/ ///
									pen(50) sampletop(0.01) `command' 
								
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
									quietly rename (v1 v2 v3 v4 v5 v6) ///
										(p big_t small_t antitonic extrap thr)
									quietly gen mpoint = `mpoint' in 1
									
									//export it
									quietly export excel `mpfile', ///
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
								
							}
						}
						else {
							di as error "wage and diverse distributions" _continue
							di as error " were not adjusted"
							exit 10
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
					
	// Details for file replace / no-replace distinction
	if ("`type'" == "rep") local command ""
	if ("`type'" == "rep") local ext ""
	if ("`type'" == "norep") local command "noreplace"
	if ("`type'" == "norep") local ext "_norep"

	//start from scratch 
	clear
	local obs_mp = wordcount("`ctry_yr_list`ext''")
	set obs `obs_mp'
	quietly gen country_year = ""
	
	//loop over values 
	forvalues n = 1 / `obs_mp' {
		local wd_`n': word `n' of `ctry_yr_list`ext''
		quietly replace country_year = "`wd_`n''" in `n'
	}
	
	//save results 
	local mpfile "output/bfm_summary/bfm`ext'_`pf'/merging_points.xlsx"
	quietly export excel `mpfile', firstrow(variables) ///
		sheet("country_years_2stage") sheetreplace keepcellfmt	

}



