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
qui do "code/Do-files/auxiliar/aux_general.do"
local lang $lang 

*get list of countries and years 
global steps " "raw" "bfm_norep_pre" "rescaled" "natinc" "pod" "pon" "   
global units " "act" "ind" "pch" "esn" "

*get date 
local date "$S_DATE"
local date = subinstr("`date'", " ", "", .)

*make room 
foreach u in $units {
	global efftax_`u' ${efftaxes}efftax_`u'_`date'.xlsx
	qui cap erase "${efftax_`u'}"
}

//save for input in other dofiles 
qui import excel ${inflation_data}, firstrow ///
	sheet("inflation-xrates") 
qui order country countrycode year defl_xxxx xppp_eur	
tempfile td_ixd 
qui save `td_ixd'	

//prepare file for long format 
tempfile tf_lformat_detail tf_lformat_grouped tf_lformat_efftax 
local iter_lformat_detail = 1 
local iter_lformat_grouped = 1 

*loop over units 
foreach unit in $units {
	di as result "unit: `unit'"
	
	*loop over steps 
	foreach step in $steps {	
		local s1 = substr("`step'", 1, 3)
		di as text "step: `s1'"
		
		*erase microfiles if they already exist 
		global mfile_`s1' ${microfiles}smicrofile_`unit'_`s1'_`date'.xlsx
		qui cap erase "${mfile_`s1'}"
		
		local iter_1st_`sheet' = 1
		
		*loop over results-sheets 
		foreach sheet in "Summary" "Composition" {
			
			*define temporary file 
			tempfile tf_`s1'_`sheet'_`unit'
			di as text " -sheet: `sheet'"
			local iter_1st_`sheet' = 1
				
			*import sheet 
			qui import excel ///
				"${summary}ineqstats_`step'_`unit'.xlsx", ///
				sheet("`sheet'") firstrow clear
				
			*save step and unit 
			qui gen step = "`s1'" 
			qui gen unit = "`unit'"
			qui order step unit country year 
				
			*Exclude some cases and missings
			qui do "code/Do-files/auxiliar/aux_exclude_ctries.do"	
			qui replace exclude = 1 if country == "DOM" & year < 2012
			***********ACTIVATE SUPERLIGHT VERSION FOR DEBUGGING
			*qui replace exclude = 1 if !inlist(country, "CHL", "BRA")
			*qui replace exclude = 1 if year < 2018
			***********ACTIVATE SUPERLIGHT VERSION FOR DEBUGGING
			qui drop if exclude == 1
			qui drop exclude 
			
			*Deal with estimate summary   
			if "`sheet'" == "Summary" {
				*list countries and corresponding years 
				qui drop if missing(average) 
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
				qui cap import excel ///
					"${summary}ineqstats_`step'_`unit'.xlsx", ///
					sheet("`c'`t'") firstrow clear
				if _rc == 0 {
	
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
						sheet("`c'`t'") firstrow(var) keepcellfmt 
						
					*save in long format too 
					if `iter_lformat_detail' == 0 {
						qui append using `tf_lformat_detail'
					} 
					local iter_lformat_detail = 0 
					qui save `tf_lformat_detail', replace 
				}		
			}
		}			
	}
}

qui use `tf_lformat_detail', clear 
foreach v in thr avg bckt_sh b {
	qui replace `v' = 0 if `v' < 0
}

foreach v in mbe wmbe hea edu oex  {
	qui replace `v' = 0 if mbe < 0 | wmbe < 0 |  hea < 0 | edu < 0 | oex < 0 
}

qui export delimited "${microfiles}smicrofile_long_detailed.csv", replace 

qui use `tf_lformat_grouped', clear 
drop if missing(value)
qui export delimited "${microfiles}smicrofile_long_grouped.csv", replace 

*Effective tax rates
local iter = 1 
foreach unit in $units {
	di as text "saving: effective tax rates (`unit')..."
	foreach c in `ctries' {
		foreach t in ``c'_years' {
			*import database 
			cap quietly import excel using ///
				"${summary}efftax_`unit'.xlsx", ///
				firstrow sheet("`c'`t'") clear 
			if _rc == 0 & !inlist("`c'`y'", "${excl_ccyy}") & ///
				!inlist("`c'", "DOM") {
				cap drop n 
				qui rename ftile_sca p 
				qui rename `unit'_* *
				qui replace p = round(p * 10^5)
				qui replace p = p / 10^5
				qui export excel "${efftax_`unit'}", ///
					sheet("`c'`t'", replace) firstrow(var) keepcellfmt
				*also save in long format
				qui gen country = "`c'"
				qui gen unit = "`unit'"
				qui gen year = `t'
				qui order unit country year p 
				if `iter' != 1 qui append using `tf_lformat_efftax' 
				qui save `tf_lformat_efftax', replace 
				local iter = 0 
			}	
		}
	}	
}
 
*save efftax in long format  
qui use `tf_lformat_efftax', clear 
qui export delimited "${microfiles}smicrofile_long_efftax.csv", replace 

 
