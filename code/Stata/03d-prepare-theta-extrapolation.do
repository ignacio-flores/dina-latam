// Direct and runner execution share country selection and staged publication.
args dina_requested_countries
local dina_country_worker : environment DINA_COUNTRY_WORKER
if "`dina_country_worker'" != "03d-prepare-theta-extrapolation" {
    do "code/Stata/auxiliar/dina_country_task.do" "03d-prepare-theta-extrapolation" "`dina_requested_countries'"
    exit
}


////////////////////////////////////////////////////////////////////////////////
//Apply bfm correction based on theta coefficients observed for other years
////////////////////////////////////////////////////////////////////////////////

//General settings -------------------------------------------------------------
clear all

//preliminary
global aux_part  ""preliminary"" 
qui do "code/Stata/auxiliar/aux_general.do"

// 1. Compute pseudo-thetas ----------------------------------------------------

//temporary names 
tempfile tf 
tempvar w_change
local review_root : environment DINA_REVIEW_ROOT
local theta_panel "output/bfm_summary/all_thetas.xlsx"
if "`review_root'" != "" {
    local theta_panel "`review_root'/`theta_panel'"
}

// Use the accepted observed panel, including approved historical cases that
// the current BFM index sheets omit. Donor exclusions do not redefine which
// years are observed when selecting extrapolation targets.
local configured_countries : subinstr global dina_run_countries `"""' "" , all
global overlap_countries ""
foreach c in $all_countries {
	global `c'_overlap_years ""
	local observed_`c' ""
}
preserve
	qui import excel "`theta_panel'", sheet("panel") firstrow clear
	capture confirm numeric variable year
	if _rc quietly destring year, replace
	qui assert !missing(country) & !missing(year)
	qui keep if strpos(" `configured_countries' ", " " + country + " ")
	qui keep if inrange(year, $first_y, $last_y)
	// The accepted panel and stable summaries must describe the same observed
	// merging points. Permit only storage precision below the grid spacing.
	foreach c in $all_countries {
		qui levelsof year if country == "`c'", local(panel_years) clean
		foreach y in `panel_years' {
			qui summarize mpoint if country == "`c'" & year == `y', meanonly
			local panel_mp_`c'_`y' = r(mean) / 100
			assert !missing(r(mean)) & r(min) == r(max)
		}
	}
	qui keep country year
	qui duplicates drop
	foreach c in $all_countries {
		qui levelsof year if country == "`c'", local(observed_`c') clean
	}
	// Retain the established donor exclusions.
	qui drop if country == "CHL" & inlist(year, 2000, 2003)
	qui drop if country == "ECU" & year == 2011
	qui levelsof country, local(observed_countries) clean
	global overlap_countries "`observed_countries'"
	foreach c in $overlap_countries {
		qui levelsof year if country == "`c'", local(donor_years) clean
		global `c'_overlap_years "`donor_years'"
	}
restore

if trim("$overlap_countries") == "" {
	di as error "03d: the accepted panel has no eligible observed donors in the configured countries and years."
	exit 459
}

//Get trust regions
qui import delimited using "${admin_trust_regions}", clear varnames(1) asdouble
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

if "${bfm_replace}" == "yes" {
	local ext ""
}
if "${bfm_replace}" == "no" {
	local ext "_norep"
}

// Load immutable observed summaries captured by 03a/03b. Reading published
// corrected microdata here would feed a preceding 03e result back into itself.
local iter = 1
foreach c in $overlap_countries {
    foreach t in ${`c'_overlap_years} {
        di as result "preparing theta extrapolation for `c'-`t' from observed donor summary:"
        dina_coverage, country("`c'") year(`t') step("pseudo_theta") status("expected")
        local donorfile "output/bfm_summary/observed_donors/`c'`t'.dta"
        if "`review_root'" != "" {
            local donorfile "`review_root'/`donorfile'"
        }
        capture confirm file "`donorfile'"
        if _rc {
            di as error "03d: missing stable observed donor for `c' `t': `donorfile'"
            di as error "Reconstruct its original observed correction in 03a/03b and capture the donor summary; existing extrapolated microdata cannot substitute."
            exit 601
        }
        quietly use "`donorfile'", clear
        assert _N == 127
        isid country year ftile
        assert country == "`c'" & year == `t'
        assert recipe_version == 1 & source_method == "observed_bfm_noreplace"
        assert inlist(capture_stage, "03a", "03b")
        assert !missing(ftile, pseudo_theta, mpoint) & pseudo_theta > 0
        assert mpoint > 0 & mpoint < 1
        assert mpoint == mpoint[1] & unobs_cutoff == unobs_cutoff[1]
        local expected_mp = `panel_mp_`c'_`t''
        if missing(`expected_mp') | abs(mpoint[1] - `expected_mp') > 0.0000001 {
            di as error "03d: observed donor `c' `t' disagrees with the accepted panel merging point. Rebuild/review the matching observed products together."
            exit 459
        }
        local `c'_`t'_mp = mpoint[1]
        local `c'_`t'_un = unobs_cutoff[1]
        local yrtrust = `trust_`c'_`t''
        assert !missing(`yrtrust')
        quietly keep if ftile >= `yrtrust' - 0.05
        quietly keep ftile country year pseudo_theta mpoint inter
        if `iter' == 0 quietly append using `tf'
        quietly save `tf', replace
        local iter = 0
    }
}

quietly export excel "${dina_output_root}/output/bfm_summary/pseudo_thetas.xlsx", ///
    firstrow(variables) sheet("panel_pthetas") sheetreplace

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
		local type "bfm`ext'_pos"
		if inlist("`c'", "BRA", "CRI") local type "bfm`ext'_pre"
		
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
		qui do "code/Stata/auxiliar/aux_general.do"	
		
		//create main folders 
		local dirpath "${dina_output_root}/output/figures/pthetas"
		mata: st_numscalar("exists", direxists(st_local("dirpath")))
		if (scalar(exists) == 0) {
			mkdir "`dirpath'"
			display "Created directory: `dirpath'"
		}
		
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
			qui graph export "${dina_output_root}/output/figures/pthetas/`c'`t'.pdf", replace
			dina_coverage, country("`c'") year(`t') step("pseudo_theta") status("success")
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

// Explicit scientific choices are independent of Admin-file existence.
// These rows supplement ordinary unobserved targets; they do not change donors.
preserve
    quietly import delimited "config/theta_extrapolation_policy.csv", clear varnames(1)
    isid country year
    assert regexm(country, "^[A-Z][A-Z][A-Z]$") & year == floor(year)
    assert method == "extrapolated"
    foreach c in $all_countries {
        quietly levelsof year if country == "`c'", local(forced_`c') clean
    }
restore

// 3. Generate weight-adjusters (years w/ survey but w/o tax data) -------------

//Loop over countries and years 
foreach c in $overlap_countries {
	forvalues t = $first_y/ $last_y {		
				
		//Define file paths	
		local taxfile "input_data/admin_data/`c'/gpinter_`c'_`t'.xlsx"
		local svyfile "intermediary_data/microdata/raw/`c'/`c'_`t'_raw.dta"
		local wagefile "input_data/admin_data/`c'/wage_`c'_`t'.xlsx"
		
		// Extrapolate survey years without an accepted observed BFM case.
		// Preserve the existing explicit exceptions even when a case is observed.
		local target_year "`t'"
		local observed_years "`observed_`c''"
		local is_observed : list target_year in observed_years
		local register_`c'_`t' "no"
		if `is_observed' local register_`c'_`t' "yes"
		local forced_years "`forced_`c''"
		local force_extrap : list target_year in forced_years
		if `force_extrap' local register_`c'_`t' "no"
		if ("`c'" == "CRI" & inrange(`t', 2000, 2009)) local register_`c'_`t' "no"
		if ("`c'" == "CRI" & `t' == 2017) local register_`c'_`t' "no"
		if ("`c'" == "CHL" & inrange(`t', 2000, 2004)) local register_`c'_`t' "no"
		if inlist("`c'", "BRA", "COL") & `t' == 2002 local register_`c'_`t' "no"

		//report existence of files in log 
		if ("`register_`c'_`t''" == "yes") {
		
			cap confirm file `svyfile'
			if !_rc {
				di as text "`c' `t': Survey and observed BFM data " _continue
				di as text "exist"
			}		
			else {
				di as text "`c' `t': observed BFM case exists but survey does not"
			}		
		}	
				
		if ("`register_`c'_`t''" == "no") {	
					
			//Confirm this is what we want 
			cap confirm file `svyfile'		
			if !_rc {
				dina_coverage, country("`c'") year(`t') step("weight_adjuster") status("expected")
				di as result "`c' `t': Survey exists and requires theta extrapolation"
				local `c'_yrs_to_adjust "``c'_yrs_to_adjust' `t'"
				
				//define macros
				local weight "_fep"	
				if ("`c'" == "ARG") local weight "n_fep"
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
							`weight' * weight_adjuster_median * obs
					} 
					else {
						qui gen new_weight = ///
							`weight' * weight_adjuster * obs
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
					
					//create main folders 
					local dirpath "${dina_output_root}/intermediary_data/weight_adjusters"
					mata: st_numscalar("exists", direxists(st_local("dirpath")))
					if (scalar(exists) == 0) {
						mkdir "`dirpath'"
						display "Created directory: `dirpath'"
					}
					
					//export
					qui export excel "${dina_output_root}/intermediary_data/weight_adjusters/`c'`t'.xlsx", ///
						firstrow(variables) sheet("brackets", replace)  	
					
					dina_coverage, country("`c'") year(`t') step("weight_adjuster") status("success")
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
				di as text "`c' `t': Survey and observed BFM data" _continue
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
local index_rows = max(1, `obs_mp')
qui set obs `index_rows'
qui gen country_year = ""
if `obs_mp' > 0 {
	forvalues n = 1 / `obs_mp' {
		local wd_`n': word `n' of `ctry_yr_list'
		qui replace country_year = "`wd_`n''" in `n'
	}
}	

//export index 
qui export excel "${dina_output_root}/intermediary_data/weight_adjusters/index.xlsx", ///
	firstrow(variables) sheet("country_years_03d") sheetreplace 
	
