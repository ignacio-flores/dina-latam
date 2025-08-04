
////////////////////////////////////////////////////////////////////////////////
//
// 							Title: BFM - Extrapolations 
// 			 Authors: Mauricio DE ROSA, Ignacio FLORES, Marc MORGAN 
// 									Year: 2020
//
// 			Description:
// 			Apply bfm correction based on theta coefficients observed for 
//			other years
//
////////////////////////////////////////////////////////////////////////////////

//General settings -------------------------------------------------------------
clear all

//preliminary
global aux_part  ""preliminary"" 
qui do "code/Do-files/auxiliar/aux_general.do"

// 1. Compute pseudo-thetas ----------------------------------------------------

//temporary names 
tempfile tf 
tempvar w_change

//Get list of country-years corrected in 02a-b
global aux_part  ""tax_svy_overlap"" 
qui do "code/Do-files/auxiliar/aux_general.do"	

//Get trust regions
qui import excel "${taxpath}directory.xlsx", ///
	sheet("trust") firstrow clear
qui destring trust, replace	
local itov = 1 
foreach c in $overlap_countries {
	if `itov' == 1 di as result "Tax-Survey overlaping years"
	di as result "`c': " _continue
	di as text "${`c'_overlap_years}"
	local itov = 0 
	foreach y in ${`c'_overlap_years} {
		local auxi ""
		if inlist("`c'", "CRI", "MEX", "SLV") local auxi "_wages"
		qui sum trust`auxi' if country == "`c'" & year == `y'
		local trust_`c'_`y' = `r(mean)'
	}
}

//loop over overlaping countries and years 
local iter = 1 
foreach c in $overlap_countries {	

	local iter_n = 1
	foreach t in ${`c'_overlap_years} {
	
		//locals
		local raw_weight "_fep"	
		local cor_weight "_weight"
		local id "id_hogar"
		local age "edad"
		local y " ${y_postax_tot} "
		local yrtrust = `trust_`c'_`t''
		if ("`c'" == "BRA") local y " ${y_pretax_tot_bra} "
		if ("`c'" == "CRI") local y " ${y_pretax_tot} "
		if ("`c'" == "PER") local y " ${y_postax_tot_per} "
		if ("`c'" == "MEX") local y " ${y_postax_formal_wage} "
		if ("`c'" == "ARG") {
			local y " ${y_postax_private_wage} "
			local raw_weight "n_fep"
		} 
	
		//define paths
		local pa "Data/"
		local type "bfm_norep_pos"
		if inlist("`c'", "BRA", "CRI") local type "bfm_norep_pre"
		local corfile "`pa'CEPAL/surveys/`c'/`type'/`c'_`t'_`type'.dta"
		
		//set temporary variables 
		tempvar ftile freq F fy cumfy L

		//bring corrected survey
		use `corfile', clear
		cap drop __*

		//check consistency of raw / corrected weights
		foreach type2 in "raw" "cor" {
			qui sum ``type2'_weight', meanonly 
			local poptot_`type2' = r(sum)
		}
		assert round(`poptot_cor' / `poptot_raw' * 100) == 100
		
		//compute cumulative distribution 
		sort `y' _pid 
		quietly	gen `freq' = `raw_weight'/`poptot_raw'
		quietly	gen `F' = sum(`freq')	
			
		//classify obs in 127 g-percentiles
		cap drop ftile 
		qui egen ftile = cut(`F'), ///
			at(0(0.01)0.99 0.991(0.001)0.999 0.9991(0.0001)0.9999 ///
			0.99991(0.00001)0.99999 1)
		qui replace ftile = 0.99999 if missing(ftile)	
			
		//aggregate to fractile level	
		qui collapse (sum) `raw_weight' `cor_weight' ///
			[weight = `raw_weight'], by(ftile)	
		qui gen country = "`c'"
		qui gen year = `t'
		
		//define pseudo-theta 
		qui gen pseudo_theta = `raw_weight' / _weight
		qui label var pseudo_theta "ratio: raw over corrected weight" 
		
		//check all fractiles are present in pseudo thetas 	
		cap assert _N == 127
		if _rc != 0 {
			tempfile tf_`c'_`t'
			qui save `tf_`c'_`t'', replace 
			clear 
			qui set obs 127
			qui gen ftile = (_n - 1)/100 in 1/100
			qui replace ftile = (99 + (_n - 100)/10)/100 in 101/109
			qui replace ftile = (99.9 + (_n - 109)/100)/100 in 110/118
			qui replace ftile = (99.99 + (_n - 118)/1000)/100 in 119/127
			qui merge 1:1 ftile using `tf_`c'_`t'', nogen
		
			//interpolate using model  
			qui nl (pseudo_theta = {a = 1} * (1 - ftile)^-{b = 1})  
			local coeff_`c'_`theta' = _b[b:]
			local coeff_rd_`c'_`theta' = round(`coeff_`c'_`theta'', 0.01)
			local coeff_txt_`c'_`theta' "`coeff_rd_`c'_`theta'' (`theta')"
			qui predict predicted //if e(sample)
			
			*qui ipolate pseudo_theta ftile, gen(ipo) epolate
			qui replace pseudo_theta = predicted if ///
				missing(pseudo_theta)
			qui drop predicted
			qui replace country = country[1] if missing(country)
			qui replace year = year[1] if missing(year)
			assert _N == 127
		}
	
		//get information from actual bfms 
		preserve
			//save info on merging point 
			qui import excel "results/`type'/merging_points.xlsx" ///
				, sheet(`c'`t') firstrow clear 
			qui levelsof mpoint, local(`c'_`t'_mp)	
			//save info on `unobserved' population 
			qui sum p if antitonic <= 0.1, meanonly
			local `c'_`t'_un = r(min)
		restore 
		
		//cosmetics 
		qui gen mpoint = ``c'_`t'_mp'
		qui keep if ftile >= `yrtrust' - 0.05 
		
		//append data if necessary
		if `iter' == 0 qui append using `tf'
		
		//save
		qui save `tf', replace 	
		
		//count iterations 
		local iter = 0
		local iter_n = `iter_n' + 1
		
	}
}

//save data 
qui gen inter = 1 if missing(`raw_weight', `cor_weight')
qui drop `raw_weight' `cor_weight'
qui export excel "${pseudo_thetas}", firstrow(variables) ///
	sheet("panel_pthetas") sheetreplace 
	
// 2. Graph Pseudo-thetas-------------------------------------------
tempvar aux_1 

//prepare to graph	
bysort country ftile: egen median_pt = median(pseudo_theta)	
qui replace ftile = ftile * 100
qui replace mpoint = mpoint * 100
qui gen `aux_1' = 1 
sort country year ftile  
qui gen p = ftile / 100

//graph
cap drop pt_smooth
qui gen pt_smooth = .
foreach c in $overlap_countries {
	foreach t in ${`c'_overlap_years} {
	
		//deal with pre/post tax incomes 
		local type "bfm_norep_pos"
		if inlist("`c'", "BRA", "CRI") local type "bfm_norep_pre"
		
		//declare model 
		qui nl (pseudo_theta = {a = 1} * (1 - p)^-{b = 1}) ///
			if country == "`c'" & year == `t'
		
		//save predicted values 
		local coeff_`c'`t'_`theta' = _b[b:]
		local coeff_rd_`c'`t'_`theta' = round(`coeff_`c'_`theta'', 0.01)
		local coeff_txt_`c'`t'_`theta' "`coeff_rd_`c'`t'_`theta'' (`theta')"
		qui cap predict predict_t_`c'`t' if e(sample)
		cap replace pt_smooth = predict_t_`c'`t' ///
			if country == "`c'" & year == `t'

		//cosmetic details
		qui sum ftile if country == "`c'" & year == `t'
		local minp = r(min)
		if (`minp' < 95) local minp = 90
		if `minp' >= 95 local minp = 95
		qui sum year if country == "`c'", meanonly 
		local max_yr_`c' = r(max)
		local min_yr_`c' = r(min)
		
		//prepare plot lines (for median and all years)
		/*
		qui levelsof year if country == "`c'" 
		local n_years_`c' = r(r)
		forvalues i = 1/`n_years_`c'' {
			local plotline_`c' `plotline_`c'' plot`i'opts(lc(black*0.3))
		}
		local leg_aux = `n_years_`c'' + 1
		*/
		
		//cosmetics 
		cap drop ftile_*
		qui egen ftile_c = concat(ftile)
		qui encode ftile_c, gen(ftile_n)
		
		//call graph parameters 
		global aux_part  ""graph_basics"" 
		qui do "code/Do-files/auxiliar/aux_general.do"	
		
		//Graph by country/year 
		preserve
			cap drop if (country != "`c'" | !inlist(year, `t',  `max_yr_`c''))
			qui replace median_pt = . if year != `max_yr_`c''
			sort country year ftile
			xtline pseudo_theta if country == "`c'" & ftile > `minp',  ///
				i(year) t(ftile) overlay `plotline_`c'' ///
				addplot(scatter median_pt ftile ///
				if country == "`c'" & ftile >`minp' & year == `max_yr_`c'', ///
				mcolor(black) msize(vsmall) ///
				|| scatter predict_t_`c'`t' ftile, mcolor(cranberry) ///
				mfcolor(cranberry) msize(tiny)) ///
				yline(1, lcolor(red) lpattern(dot)) ///
				ytitle("Pseudo-theta coefficient") xtitle("Fractile") ///
				ylabel(0(0.5)3, format(%2.1f) $ylab_opts ) ///
				xlabel(/*`minp'(1)100*/, $xlab_opts ) ///
				legend(/*order(1 "All obs. (`min_yr_`c''-`max_yr_`c'')" ///
				`leg_aux' "Median")  ring(0) bplace(neast) symx(3pt) ///
				rows(2)*/ region(lcolor(bluishgray))) $graph_scheme 
			//save
			qui graph export "figures/`type'/pthetas/`c'`t'.pdf", replace  	
		restore	
	}
}
qui sort country ftile year 

//gather info on unobserved population 
//(from first big loop)
foreach c in $overlap_countries {	
	local iter = 1 
	foreach t in ${`c'_overlap_years} {
		if "``c'_`t'_un'" != "." {
			if `iter' == 1 {
				local `c'_unobs_list ``c'_`t'_un'
			} 
			else {
				local `c'_unobs_list ``c'_unobs_list' + ``c'_`t'_un'
			} 
			local `c'_new_overlaps ``c'_new_overlaps' `t'
			local iter = 0 
			di as result "`c' - `y' - `t' unobs: " 1 - ``c'_`t'_un' 
			if (1-``c'_`t'_un') * 100 >= 0.05 {
				local `c'_`t'_un = 0.9995
				di as result "changed to 0.05"
			} 
			
		}
	}
	local n_yrs_`c' = wordcount("``c'_new_overlaps'")
}	

//graph unobserved population 
local obs_counter = 0 
foreach c in $overlap_countries {
	foreach t in ``c'_new_overlaps' {
		local obs_counter = `obs_counter' + 1
	}
}
preserve 
	clear 
	qui set obs `obs_counter'
	qui gen country = ""
	qui gen year = .
	qui gen unobs = . 
	local obs_counter = 1
	foreach c in $overlap_countries {
		foreach t in ``c'_new_overlaps' { 
			qui replace country = "`c'" in `obs_counter'
			qui replace year = `t' in `obs_counter'
			qui replace unobs = (1 - ``c'_`t'_un') * 100 in `obs_counter'
			local obs_counter = `obs_counter' + 1
		}
	}
	*xtline unobs, i(country) t(year) overlay
restore 

//estimate average unobserved by country 
qui gen obs = . 
foreach c in $overlap_countries {
	if (`n_yrs_`c'' != 0) {
		local obs_`c' = (``c'_unobs_list') / `n_yrs_`c''
	} 
	else {
		local obs_`c' = 1
	}
	qui replace obs = `obs_`c'' if country == "`c'"
}
qui gen unobs = 1 - obs 


**define weight adjuster 
gen weight_adjuster = 1 / pt_smooth
gen weight_adjuster_median = 1 / median_pt

//save info on expanded mass
tempvar size added_pop_brckt 
qui bysort country: gen `size' = (ftile - ftile[_n-1]) 
qui replace `size' = 1 if missing(`size') 
qui gen `added_pop_brckt' = `size' * (weight_adjuster - 1)
qui bysort country: egen added_pop_tot = total(`added_pop_brckt')

//save dbase for later 
sort country year ftile 
order country year ftile 
qui drop predict_t_*
tempfile weight_adjusters
qui save `weight_adjusters'

// 3. Generate weight-adjusters (years w/ survey but w/o tax data) -------------

//Loop over countries and years 
foreach c in $overlap_countries {
	forvalues t = $first_y/ $last_y {		
				
		//Define file paths
		local taxfile "${taxpath}`c'/gpinter_`c'_`t'.xlsx"
		local wagfile "${taxpath}`c'/wage_`c'_`t'.xlsx"
		local svyfile "${svypath}`c'/raw/`c'_`t'_raw.dta"
		
		//check existence of register data
		local register_`c'_`t' "no"
		di as result "register: `register_`c'_`t''"
		foreach prefix in "tax" "wag" {
			cap confirm file ``prefix'file'
			di as result "``prefix'file'"
			
			if !_rc local register_`c'_`t' "yes"
		}
		di as result "register: `register_`c'_`t''"
		
		//force exceptional extrapolations
		if ("`c'" == "CRI" & inrange(`t', 2000, 2009)) {
			local register_`c'_`t' "no"
		} 
		if ("`c'" == "CRI" & `t' == 2017) {
			local register_`c'_`t' "no"
		} 
		if ("`c'" == "CHL" & inrange(`t', 2000, 2004)) {
			local register_`c'_`t' "no"
		}	
		if inlist("`c'", "BRA", "COL") & `t' == 2002 {
			local register_`c'_`t' "no"
		} 

		//report existence of files in log 
		if ("`register_`c'_`t''" == "yes") {
		
			cap confirm file `svyfile'
			if !_rc {
				di as text "`c' `t': Both survey and tax data " _continue
				di as text "exist"
			}		
			else {
				di as text "`c' `t': tax data exists but survey does not"
			}		
		}	
				
		if ("`register_`c'_`t''" == "no") {	
					
			//Confirm this is what we want 
			cap confirm file `svyfile'		
			if !_rc {
				di as result "`c' `t': Survey exists, but tax data doesn't'"
				local `c'_yrs_to_adjust "``c'_yrs_to_adjust' `t'"
				
				//define macros
				local weight "_fep"	
				local age "edad"
				local y " ${y_postax_tot} "
				if ("`c'" == "BRA") local y " ${y_pretax_tot_bra} "
				if ("`c'" == "CRI") local y " ${y_pretax_tot} "
				if ("`c'" == "PER") local y " ${y_postax_tot_per} "
				if ("`c'" == "MEX") {
					local y " ${y_postax_formal_wage} "
					if `t' == 2000 local y " ${y_postax_wag} " 
				} 
				if ("`c'" == "ARG") {
					local y " ${y_postax_private_wage} "
					local weight "n_fep"
				} 
				tempvar freq F 
								
				//identify closest year from weight adjusters
				qui use `weight_adjusters', clear 
				qui keep if country == "`c'"
				cap drop distance_y y
				qui gen y = `t'
				qui gen distance_y = abs(y - year)
				qui sum distance_y  
				qui keep if distance_y == r(min)
				qui sum year 
				local imp = r(min)
				qui keep if year == `imp'
				if inlist("`c'", "ARG", "MEX", "CRI") {
					di as result "`c' `t': imputed with median"
				}
				else {
					di as result "`c' `t': imputed with `imp'"
				}
				
				tempfile wadj_`c'`t'
				qui save `wadj_`c'`t''
				
				//open original survey
				qui use `svyfile', clear 
				sort `y' //_pid
				
				//save total population
				qui sum `weight' 
				local orig_weights = r(sum)

				//save average inc in memory
				qui sum `y' [w=`weight']
				local inc_avg = r(mean)	
				di as result "`c' `y' `inc_avg'"
				qui sum	`weight', meanonly
				local poptot = r(sum)
				
				//skip if variable is empty 
				if `inc_avg' != . {
					
					//define cumulative distribution 
					quietly	gen `freq' = `weight' / `poptot'
					quietly	gen `F' = sum(`freq')
						
					// Classify obs in 127 g-percentiles 
					qui egen ftile = cut(`F'), at( ///
						0(0.01)0.99 ///
						0.991(0.001)0.999 ///
						0.9991(0.0001)0.9999 ///
						0.99991(0.00001)0.99999 1)
					qui replace ftile = 0.99999 if missing(ftile)	
					qui replace ftile = ftile * 100 
					
					//merge with weight adjusters
					qui gen country = "`c'"
					qui merge m:1 country ftile using ///
						`wadj_`c'`t'', keep(3) nogen
					
					//check consistency
					qui sum ftile, meanonly
					local minp = r(min) 
					assert _N != 99 - `minp' + 27
							
					//find new weight 
					if inlist("`c'", "ARG", "MEX", "CRI") {
						qui gen new_weight = ///
							`raw_weight' * weight_adjuster_median * obs
					} 
					else {
						qui gen new_weight = ///
							`raw_weight' * weight_adjuster * obs
					}
					
					qui gen new_freq = new_weight / `poptot'		
					
					//add an observation with missing pop 
					local newbigN = `=_N+1'
					qui set obs `newbigN'	
					//get maximum income in survey
					qui sum `y'
					local max_inc_`t'_`c' = r(max)
					//weight
					qui replace new_weight = ///
						(1 - `obs_`c'') * `poptot' in `newbigN'
						
					//freq
					qui replace new_freq = new_weight / `poptot' in `newbigN'
					
					//income (only for sorting purpose)	
					qui replace `y' = `max_inc_`t'_`c'' * 1.5 in `newbigN'
					
					//define new cumulative distribution
					gsort -`y'
					gen new_F = sum(new_freq) 
					qui replace new_F = 1 - new_F
					sort `y'
					
					//get rid of income for last obs
					qui replace `y' = . if `y' == `max_inc_`t'_`c'' * 1.5
					
					//Re-classify obs in 127 g-percentiles (new weights)
					qui egen new_ftile = cut(new_F), at( ///
						0(0.01)0.99 ///
						0.991(0.001)0.999 ///
						0.9991(0.0001)0.9999 ///
						0.99991(0.00001)0.99999 1)	
					*qui replace new_ftile = 0.99999 if missing(new_ftile)	
					
					//collapse to fractiles 
					qui collapse (min) thr = `y' ///
						(mean) bracketavg = `y' ///
						[w = new_weight], by(new_ftile)
					qui rename new_ftile p 
					
					//fill last brackets with values above maximum income 
					qui replace thr = `max_inc_`t'_`c'' * 1.5 in `=_N'
					qui replace bracketavg = `max_inc_`t'_`c'' * 2 in `=_N'
					
					//ensure thresholds are always increasing...
					local iter = 0
					qui count if thr[_n] >= thr[_n + 1] 
					while (r(N) > 0) {
						tempvar bracket newbracket queue weight nweight
						qui generate `queue' = sum(thr[_n] >= thr[_n + 1])
						qui generate `bracket' = _n
						//We group the bracket with the one just above
						qui gen `newbracket' = `bracket'[_n + 1 ] ///
							if (thr[_n] >= thr[_n + 1])
						qui replace `bracket' = `newbracket' ///
							if (`queue' == 1) & (thr[_n] >= thr[_n + 1])
						//weight brackets before collapsing
						qui gen `weight' = p[_n + 1] - p
						qui replace `weight' = 1 - p if missing(`weight')
						qui gen `nweight' = `poptot' * `weight'
						//collapse
						qui collapse  (min) p thr (mean) bracketavg ///
							[w=`nweight'], by(`bracket')
						qui count if (thr[_n] >= thr[_n + 1])
						local iter = `iter' + 1 
						if `iter' == 30 {
							di as error ///
								"'while' iterated 30 times w/o reaching goal"
							exit 1
						}
					}
					
					//check what intervals are missing
					tempfile tf2_`c'_`t'
					qui save `tf2_`c'_`t'', replace 
					qui sum p, meanonly 
					local minp_`c'_`t' = r(min)
					clear 
					qui set obs 127
					qui gen p = (_n - 1)/100 in 1/100
					qui replace p = (99    + (_n - 100)/10)/100 in 101/109
					qui replace p = (99.9  + (_n - 109)/100)/100 in 110/118
					qui replace p = (99.99 + (_n - 118)/1000)/100 in 119/127
					qui merge 1:1 p using `tf2_`c'_`t'', nogen
					qui drop if p < `minp_`c'_`t''
							
					//interpolate missing intervals (+1)
					foreach var in "thr" "bracketavg" {
						tempvar aux_`var' ipo_`var'
						recast double `var'
						qui ipolate `var' p, gen(`ipo_`var'')
						qui replace `var' = `ipo_`var'' if missing(`var')
						qui gen aux_`var' = ///
							.1 * `max_inc_`t'_`c'' if missing(`var')
						qui replace aux_`var' = sum(aux_`var')
						qui sum `var', meanonly
						qui replace `var' = r(max) if missing(`var')
						qui replace `var' = `var' + aux_`var' ///
							if !missing(aux_`var')
					}
					
					//cosmetics 
					cap drop __*
					//export
					qui export excel "${w_adj}`c'`t'.xlsx", ///
						firstrow(variables) sheet("brackets", replace)  	
					
					//save list of country-years
					local ctry_yr_list "`ctry_yr_list' `c'_`t'"				
				}
				else if `inc_avg' == . {
					di as error "income variable is empty for `c' `y'"
					exit 2
				}
			}
				
			//report existence of files 
			else {
				di as text "`c' `t': Both survey and tax data" _continue
				di as text " do not exist for this year"
			}
		}
	}
	
	//save list of adjusted years by country 
	if ("``c'_yrs_to_adjust'" != "") di as result "`c': ``c'_yrs_to_adjust'"
}

//list all coutry-years to adjust 
clear
local obs_mp = wordcount("`ctry_yr_list'")
qui set obs `obs_mp'
qui gen country_year = ""
forvalues n = 1 / `obs_mp' {
	local wd_`n': word `n' of `ctry_yr_list`ext''
	qui replace country_year = "`wd_`n''" in `n'
}	

//export index 
qui export excel "${w_adj}index.xlsx", ///
	firstrow(variables) sheet("country_years_03d") sheetreplace 
	


