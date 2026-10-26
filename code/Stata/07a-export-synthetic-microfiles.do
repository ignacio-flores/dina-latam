* DINA country-run entry; the worker writes only to its temporary output folder.
args dina_requested_countries
local dina_worker : environment DINA_COUNTRY_WORKER
if "`dina_worker'" != "07a-export-synthetic-microfiles" {
    do "code/Stata/auxiliar/dina_country_task.do" "07a-export-synthetic-microfiles" "`dina_requested_countries'"
    exit
}
local dina_output_env : environment DINA_OUTPUT_ROOT
local dina_countries_env : environment DINA_RUN_COUNTRIES
if `"`dina_output_env'"' == "" | trim("`dina_countries_env'") == "" {
    di as error "Missing country-run output folder or selected countries."
    exit 198
}
global dina_output_root `"`dina_output_env'"'
global dina_run_countries "`dina_countries_env'"

////////////////////////////////////////////////////////////////////////////////
//
// 					Title: EXPORT SYNTHETIC MICRO-FILES 
// 			 Authors: Mauricio DE ROSA, Ignacio FLORES, Marc MORGAN 
// 									Year: 2020
//
////////////////////////////////////////////////////////////////////////////////

clear all

//0. General settings ----------------------------------------------------------

//get list of paths 
global aux_part " "preliminary" " 
qui do "code/Stata/auxiliar/aux_general.do"
local lang $lang

tempname dina_coverage dina_empty
file open `dina_coverage' using "${dina_output_root}/_coverage.csv", write text replace
file write `dina_coverage' "country,year,step,unit,status,reason" _n
file open `dina_empty' using "${dina_output_root}/_empty_workbooks.csv", write text replace
file write `dina_empty' "path,sheet" _n

*get list of countries and years 
global steps " ${all_steps} "
global units " ${all_units} "
local dina_steps_env : environment DINA_RUN_STEPS
local dina_units_env : environment DINA_RUN_UNITS
if trim("`dina_steps_env'") != "" global steps "`dina_steps_env'"
if trim("`dina_units_env'") != "" global units "`dina_units_env'"
local dina_selected_steps : copy global steps
local dina_efftax : list posof "natinc" in dina_selected_steps

* Explicit replacement scope also records selected partitions with no eligible rows.
tempname dina_income_scope
file open `dina_income_scope' using "${dina_output_root}/_income_scope.csv", write text replace
file write `dina_income_scope' "country,step,unit" _n
foreach c in $dina_run_countries {
    foreach s in $steps {
        local shortstep = substr("`s'", 1, 3)
        foreach u in $units {
            file write `dina_income_scope' "`c',`shortstep',`u'" _n
        }
    }
}
file close `dina_income_scope'

*get date 
local date "$S_DATE"
local date = subinstr("`date'", " ", "", .)

*make room 
if `dina_efftax' {
foreach u in $units {
	global efftax_`u' ${dina_output_root}/output/efftax/efftax_`u'_`date'.xlsx
	qui cap erase "${efftax_`u'}"
}
}


//use WID deflator and PPP rates fetched by `dina sources include wid`
quietly use "input_data/wid/prices_deflator_ppp_eur.dta", clear
qui sum year if defl_xxxx == 1 	
local xppp_yr = r(mean)
qui label var defl_xxxx "GDP deflator year `xppp_yr'"
quietly drop if year < 2000 
qui order country countrycode year defl_xxxx xppp_eur	
tempfile td_ixd 
qui save `td_ixd'

//prepare file for long format 
tempfile tf_lformat_detail tf_lformat_grouped tf_lformat_efftax 
local iter_lformat_detail = 1 
local iter_lformat_grouped = 1 

//create folders if necessary 
local dirpath "${dina_output_root}/output/synthetic_microfiles"
mata: st_numscalar("exists", direxists(st_local("dirpath")))
if (scalar(exists) == 0) {
	mkdir "`dirpath'"
	display "Created directory: `dirpath'"
}

*loop over units 
foreach unit in $units {
	di as result "unit: `unit'"
	
	*loop over steps 
	foreach step in $steps {	
		local s1 = substr("`step'", 1, 3)
		di as text "step: `s1'"
		
		*erase microfiles if they already exist 
		global mfile_`s1' ${dina_output_root}/output/synthetic_microfiles/smicrofile_`unit'_`s1'_`date'.xlsx
		qui cap erase "${mfile_`s1'}"
		
		local ctries ""
		local have_summary = 0
		tempfile summary_cases
		local iter_1st_`sheet' = 1
		
		*loop over results-sheets 
		foreach sheet in "Summary" "Composition" {
			
			*define temporary file 
			tempfile tf_`s1'_`sheet'_`unit'
			di as text " -sheet: `sheet'"
			local iter_1st_`sheet' = 1
				
			*import sheet 
			qui import excel ///
				"output/ineqstats/ineqstats_`step'_`unit'.xlsx", ///
				sheet("`sheet'") firstrow clear
				
			*save step and unit 
			qui gen step = "`s1'" 
			qui gen unit = "`unit'"
			qui order step unit country year
			qui keep if strpos(" $dina_run_countries ", " " + country + " ")
				
			*Exclude some cases and missings
			qui do "code/Stata/auxiliar/aux_exclude_ctries.do"	
			qui replace exclude = 1 if country == "DOM" & year < 2012
			***********ACTIVATE SUPERLIGHT VERSION FOR DEBUGGING
			*qui replace exclude = 1 if !inlist(country, "CHL", "BRA")
			*qui replace exclude = 1 if year < 2018
			***********ACTIVATE SUPERLIGHT VERSION FOR DEBUGGING
			qui drop if exclude == 1
			qui drop exclude 
			if _N == 0 {
                local empty_sheet "Summary-estimates"
                if "`sheet'" == "Composition" local empty_sheet "Summary-composition"
                file write `dina_empty' "output/synthetic_microfiles/smicrofile_`unit'_`s1'_`date'.xlsx,`empty_sheet'" _n
                if "`s1'" == "nat" file write `dina_empty' "output/efftax/efftax_`unit'_`date'.xlsx,`empty_sheet'" _n
                continue
            }
			if "`sheet'" == "Composition" & !`have_summary' {
                file write `dina_empty' "output/synthetic_microfiles/smicrofile_`unit'_`s1'_`date'.xlsx,Summary-composition" _n
                if "`s1'" == "nat" file write `dina_empty' "output/efftax/efftax_`unit'_`date'.xlsx,Summary-composition" _n
                continue
            }
			
			*Deal with estimate summary   
			if "`sheet'" == "Summary" {
				*list countries and corresponding years 
				qui drop if missing(average)
                if _N == 0 {
                    file write `dina_empty' "output/synthetic_microfiles/smicrofile_`unit'_`s1'_`date'.xlsx,Summary-estimates" _n
                    if "`s1'" == "nat" file write `dina_empty' "output/efftax/efftax_`unit'_`date'.xlsx,Summary-estimates" _n
                    continue
                }
				preserve
					qui keep country year
					qui duplicates drop
					qui save `summary_cases', replace
				restore
				local have_summary = 1
				qui levelsof country, clean local(ctries) 
				foreach c in `ctries' {
					qui levelsof year if country == "`c'", ///
						clean local(`c'_years)
				}
				
				*append and save
				local namesheet "Summary-estimates"
				
				*append and save in wide format 
				preserve 
					cap confirm file `tf_`s1'_`sheet'_`unit'' 
					if _rc == 0 qui append using `tf_`s1'_`sheet'_`unit''
					qui save `tf_`s1'_`sheet'_`unit'', replace 
				restore 
				
				*transform to long format  
				qui rename (gini average adpop) ///
					(tot_gini tot_average tot_adpop)
				qui reshape long tot_ b50_ m40_ t10_ t1_, ///
					i(country year step unit) j(variable) string 			
				qui rename *_ value*
				qui reshape long value, ///
					i(country year step unit variable) j(group) string
				qui drop if missing(value)
				
				*save in long format 
				qui order step unit country year group variable 
				qui sort step unit country year group variable 
				
				*save long version 
				qui replace variable = "inc_sh" if variable == "sh"
				cap confirm file `tf_lformat_grouped'
				if _rc == 0 /*& `iter_1st_`sheet'' == 1*/ {
					qui append using `tf_lformat_grouped'
				}
				qui save `tf_lformat_grouped', replace
			}
			
			*Deal with composition
			if "`sheet'" == "Composition" {
				qui merge m:1 country year using `summary_cases', keep(3) nogen
				local namesheet "Summary-composition"
				*check sum is 100%
				foreach x in tot t1 t10 m40 b50 {
					qui egen check_`x' = rowtotal(`x'_*)
					qui replace check_`x' = round(check_`x' * 10^2) 
					*assert check_`x' == 10^2 | check_`x' == 0
					qui drop check_`x'
				}
				
				qui gen keepit = 0
				foreach c in $countries_tax {
					qui replace keepit = 1 if country == "`c'"
				}
				
				qui keep if keepit == 1 
				qui drop keepit 
				
				*keep only years with data 
				qui egen auxi1 = rowtotal(tot_*)
				qui drop if auxi1 == 0 
				qui drop auxi1
                if _N == 0 {
                    file write `dina_empty' "output/synthetic_microfiles/smicrofile_`unit'_`s1'_`date'.xlsx,Summary-composition" _n
                    if "`s1'" == "nat" file write `dina_empty' "output/efftax/efftax_`unit'_`date'.xlsx,Summary-composition" _n
                    continue
                }
				
				*rename composition variables
				cap rename (*_sh_`unit'_*) (*_*)
				cap rename (*_sca) (*)
				cap rename (*_pre*) (**)
				
				*save wide version
				preserve 
					cap confirm file `tf_`s1'_`sheet'_`unit'' 
					if _rc == 0 append using `tf_`s1'_`sheet'_`unit''
					qui save `tf_`s1'_`sheet'_`unit'', replace 
				restore 
				
				*reshape 
				qui reshape long tot_ b50_ m40_ t10_ t1_, ///
					i(country year step unit) j(variable) string
				qui rename (*_) (value*)	
				qui reshape long value, ///
					i(country year step unit variable) j(group) string						
				*save long version 
				qui replace variable = variable + "_sh"
				foreach bit in `unit' tax pod pon {
					qui replace variable = ///
						subinstr(variable, "`bit'_", "", .)
				} 
				qui append using `tf_lformat_grouped'
				qui save `tf_lformat_grouped', replace
			}
			
			*end first iteration 
			local iter_1st_`sheet' = `iter_1st_`sheet'' + 1 	
			
			//reopen unified sheet in synthetic micro files and efftax
  			qui use `tf_`s1'_`sheet'_`unit'', clear 
			//reorganise composition 
			if "`sheet'" == "Composition" { 
				qui order country year tot_* b50_* m40_* t10_* t1_*
			}
			*save summary of estimates 
			qui export excel "${mfile_`s1'}", ///
				sheet("`namesheet'", replace) firstrow(var) keepcellfmt	
			*save a summary for efftax too 
			if "`s1'" == "nat" {
				qui export excel "${efftax_`unit'}", ///
					sheet("`namesheet'", replace) firstrow(var) keepcellfmt	
			}
		}
		
        if !`have_summary' {
            foreach c in $dina_run_countries {
                file write `dina_coverage' "`c',0,`s1',`unit',excluded,No eligible Summary cases" _n
            }
            continue
        }

		//save cpi and xrates 
		//save for input in other dofiles 
		preserve 
			qui use `td_ixd', clear 
			qui export excel "${mfile_`s1'}", firstrow(variables) ///
				sheet("inflation-xrates") sheetreplace keepcellfmt  
		restore	
		
		//save sheets 	
		di as text " -saving individual country sheets..."
			
		//loop over countries 
		foreach c in `ctries' {
			foreach t in ``c'_years' {
                file write `dina_coverage' "`c',`t',`s1',`unit',expected,Summary detail" _n
				qui cap import excel ///
					"output/ineqstats/ineqstats_`step'_`unit'.xlsx", ///
					sheet("`c'`t'") firstrow clear
                local dina_detail_rc = _rc
                if `dina_detail_rc' {
                    di as error "Summary lists `c'/`t' but its `step'/`unit' detail sheet is unreadable."
                    exit `dina_detail_rc'
                }
                isid p
                assert _N == 127
				if `dina_detail_rc' == 0 {
	
					qui rename (s) (bckt_sh)
					cap drop topavg_* topsh_* topshbckt_*
					foreach v in country year gini average {
						qui replace `v' = ///
							`v'[1] if missing(`v')
					}
					*round p 
					qui replace p = round(p*10^5)
					qui recast int p
					qui replace p = p/10^5
					*format variables 
					qui ds country year average p, not 
					foreach v in `r(Varlist)' {
						format %3.2f `v'
					}
					*save step and unit 
					qui gen step = "`s1'" 
					qui gen unit = "`unit'"
					qui order step unit country year 
					
					foreach bit in pre `unit' pod pon sca tax pre {
						cap rename `bit'_* *
						cap rename *_`bit' *
						cap rename _`bit'_* *
					}
					
					qui export excel "${mfile_`s1'}", ///
						sheet("`c'`t'", replace) firstrow(var) keepcellfmt
						
					*save in long format too 
					if `iter_lformat_detail' == 0 {
						qui append using `tf_lformat_detail'
					} 
					local iter_lformat_detail = 0 
					qui save `tf_lformat_detail', replace
                    file write `dina_coverage' "`c',`t',`s1',`unit',success,127 detail rows exported" _n
				}		
			}
		}			
	}
}

capture confirm file `tf_lformat_detail'
if !_rc quietly use `tf_lformat_detail', clear
else {
	clear
	foreach v in country step unit {
		qui gen str3 `v' = ""
	}
	foreach v in year p thr avg bckt_sh b mbe wmbe hea edu oex {
		qui gen double `v' = .
	}
}
foreach v in thr avg bckt_sh b {
	qui replace `v' = 0 if `v' < 0
}

foreach v in mbe wmbe hea edu oex  {
	qui replace `v' = 0 if mbe < 0 | wmbe < 0 |  hea < 0 | edu < 0 | oex < 0 
}

qui export delimited "${dina_output_root}/output/synthetic_microfiles/_smicrofile_long_detailed.csv", replace

capture confirm file `tf_lformat_grouped'
if !_rc quietly use `tf_lformat_grouped', clear
else {
	clear
	foreach v in country step unit group variable {
		qui gen str12 `v' = ""
	}
	qui gen year = .
	qui gen value = .
}
drop if missing(value)
qui export delimited "${dina_output_root}/output/synthetic_microfiles/_smicrofile_long_grouped.csv", replace

*Effective tax rates: derive cases from each unit's actual input workbook.
*The detail export above must not leave behind a last-step country/year list.
* A disposable-only refresh preserves the existing pretax effective-tax export.
if `dina_efftax' {
clear
qui set obs 1
qui gen str3 country = ""
qui gen year = .
qui do "code/Stata/auxiliar/aux_exclude_ctries.do"
local excluded_cases : subinstr global excl_ccyy "," " " , all
local excluded_cases : subinstr local excluded_cases `"""' "" , all
local iter = 1
foreach unit in $units {
	di as text "saving: effective tax rates (`unit')..."
	local source "output/efftax/efftax_summary_`unit'.xlsx"
	capture confirm file "`source'"
	if _rc continue
	quietly import excel using "`source'", describe
	local sheets ""
	forvalues i = 1/`r(N_worksheet)' {
		local sheets "`sheets' `r(worksheet_`i')'"
	}
	foreach c in $dina_run_countries {
		if "`c'" == "DOM" continue
		forvalues t = $first_y/$last_y {
			local case "`c'`t'"
			local present : list case in sheets
			local excluded : list case in excluded_cases
			if !`present' | `excluded' continue
            file write `dina_coverage' "`c',`t',efftax,`unit',expected,Effective tax detail" _n
			quietly import excel using "`source'", firstrow sheet("`case'") clear
			isid ftile_sca
			assert _N == 109
			cap drop n
			qui rename ftile_sca p
			qui rename `unit'_* *
			qui replace p = round(p * 10^5) / 10^5
			qui export excel "${efftax_`unit'}", ///
				sheet("`case'", replace) firstrow(var) keepcellfmt
			qui gen country = "`c'"
			qui gen unit = "`unit'"
			qui gen year = `t'
			qui order unit country year p
			if `iter' != 1 qui append using `tf_lformat_efftax'
			qui save `tf_lformat_efftax', replace
			local iter = 0
            file write `dina_coverage' "`c',`t',efftax,`unit',success,109 tax rows exported" _n
		}
	}
}

if `iter' == 0 quietly use `tf_lformat_efftax', clear
else {
	clear
	qui gen str3 country = ""
	qui gen str3 unit = ""
	qui gen year = .
	qui gen double p = .
}
qui export delimited "${dina_output_root}/output/synthetic_microfiles/_smicrofile_long_efftax.csv", replace
}

file close `dina_coverage'
file close `dina_empty'
