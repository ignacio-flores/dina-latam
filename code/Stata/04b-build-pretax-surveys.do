////////////////////////////////////////////////////////////////////////////////
// 	Calculates effective tax rates for country-years for which we have 
//		no data. After that, it computes pre-tax incomes for corrected surveys
////////////////////////////////////////////////////////////////////////////////

//make room
clear all

//preliminary settings 
global aux_part  ""preliminary"" 
quietly do "code/Stata/auxiliar/aux_general.do"

//define macros 
if "${bfm_replace}" == "yes" local ext ""
if "${bfm_replace}" == "no" local ext "_norep"

//define paths 
global codes		"code/Stata"
global data_svy 	"intermediary_data/microdata/"
global data_tax 	"input_data/admin_data"
*global pre_aux		"Data/Tax-data/auxiliar-pre-tax"

*-------------------------------------------------------------------------------
* I. We create effective tax rates as average of each country, 
* to fill the gap when it does not exist.
*-------------------------------------------------------------------------------

foreach c in $all_countries  	{  
	forvalues  year = $first_y / $last_y {
	
		local eff_tax_file ///
			"$data_tax/`c'/eff-tax-rate/`c'_effrates_`year'.dta" 
		local syneff_tax_file 	///
			"$data_tax/`c'/eff-tax-rate/`c'_effrates_syn.dta" 
		
		*Take the first eff_tax_rate file, and merge with all the following
		local year_p = `year' + 1
		qui cap confirm file `eff_tax_file' 
		if _rc==0 {
			di as result  "`eff_tax_file' found"
			qui cap use `eff_tax_file', clear
			
			qui cap gen eff_tax_rate_ipol_`year' = eff_tax_rate_ipol
			qui cap gen eff_ss_rate_ipol_`year'  = eff_ss_rate_ipol
			
			qui cap drop eff_tax_rate_ipol
			qui cap drop eff_ss_rate_ipol
			
			forvalues  x = `year_p' / $last_y {
				qui cap confirm file ///
					"$data_tax/`c'/eff-tax-rate/`c'_effrates_`x'.dta"
				if _rc==0 {
					qui cap merge 1:1 p_merge using ///
						"$data_tax/`c'/eff-tax-rate/`c'_effrates_`x'.dta" ///
						, gen(merge_`x')
					qui cap gen eff_tax_rate_ipol_`x' = eff_tax_rate_ipol
					qui cap gen eff_ss_rate_ipol_`x'  = eff_ss_rate_ipol
					qui cap drop eff_tax_rate_ipol
					qui cap drop eff_ss_rate_ipol

				}
			}
			continue, break 			
		} 
		else {
			*di as text "`eff_tax_file' not found"
		}		
	}
	
	//write them down
	qui cap egen eff_tax_rate_ipol_syn = rmean(eff_tax_rate_ipol*)
	qui cap egen eff_ss_rate_ipol_syn  = rmean(eff_ss_rate_ipol*)
	
	//keep selected variables 
	qui cap confirm variable eff_ss_rate_ipol_syn 
		if !_rc {
			qui keep eff_tax_rate_ipol_syn eff_ss_rate_ipol_syn p_merge
		}
		else {
			qui keep eff_tax_rate_ipol_syn p_merge
		}
	
	//save 
	*qui cap duplicates drop p_merge
	*assert !missing(eff_tax_rate_ipol_syn)
	qui save `syneff_tax_file', replace
}

*-------------------------------------------------------------------------------
* II. We merge corrected surveys and effective tax rates
*-------------------------------------------------------------------------------

foreach c in $all_countries   {  
	forvalues  year = $first_y / $last_y {
	
		//locate relevant files 
		foreach x in "pre" "pos" {
			local svy_`x' ///
				"${data_svy}bfm`ext'_`x'/`c'_`year'_bfm`ext'_`x'.dta"
		}
		local eff_tax_file "$data_tax/`c'/eff-tax-rate/`c'_effrates_`year'.dta" 
		local syneff_tax_file "$data_tax/`c'/eff-tax-rate/`c'_effrates_syn.dta" 
	
		* Brazil already has pre-tax survey, we compute post-tax incomes
		if ("`c'" == "BRA" | "`c'" == "CRI") {
			clear
			cap use `svy_pre', clear
	
			qui cap assert _N == 0
				if _rc != 0 {
						
					//create fractiles in survey to prepare merge
					global prefix "pre"
					global section ""fractiles""
					qui do "$codes/auxiliar/aux_pretax.do"
				
					cap confirm file `eff_tax_file'
					if _rc==0 {
						qui cap drop _merge
						qui merge m:1 p_merge using `eff_tax_file'

						di as result "`c' `year' merging corr. survey" _continue
						di as result " with effective tax rates"
					}
					else {
						di as text "`c' `year' eff. tax rates not" _continue 
						di as text " found (`eff_tax_file')" _continue 
						di as result "using average instead"
						qui cap drop _merge
						qui cap merge m:1 p_merge using `syneff_tax_file'
					}
	
				qui cap drop if _merge == 2
				
				//create fractiles in survey to prepare merge
				global section ""adjustment""
				qui do "$codes/auxiliar/aux_pretax.do"
					
					foreach var in 	///
						"wag" "fwag" "prfwag" "mix" "cap" "oth" ///
						"ben" "imp" "kap" "mir" "tot" "totnb" ///
						"totesn" "totesb" "oth" "pen" {
						
						cap drop pos_`var'_svy 
						cap drop ind_pos_`var'
						
						qui gen pos_`var'_svy = ///
							pre_`var'_svy * (1 - eff_tax_rate_ipol)	
						qui gen ind_pos_`var' = ///
							ind_pre_`var' * (1 - eff_tax_rate_ipol) 	
					}	
				qui save `svy_pos', replace
				qui save `svy_pre', replace

			}
		}
		
		* For the rest, we compute all pre-tax incomes 
		*(= proportion as total income for each source)
		else {		
			clear
			cap use `svy_pos', clear
			cap drop __*
			
			qui cap assert _N == 0
			if _rc != 0 {
				
				//create fractiles in survey to prepare merge
				global prefix "pos"
				global section ""fractiles""
				qui do "$codes/auxiliar/aux_pretax.do"
				
				cap confirm file `eff_tax_file'
				if _rc==0 {
					qui cap drop _merge
					qui merge m:1 p_merge using `eff_tax_file'
					di as result "`c' `year' merging corr. survey" _continue
					di as result " with effective tax rates"
				}
				else {
					di as text "`c' `year' eff. tax rates not found (`eff_tax_file')" _continue
					di as result " using country average instead"
					qui cap drop _merge
					qui  merge m:1 p_merge using `syneff_tax_file'

				}
				
				qui cap drop if _merge == 2
	
				//create fractiles in survey to prepare merge
				global section ""adjustment""
				qui do "$codes/auxiliar/aux_pretax.do"
				
				foreach var in 	"wag" "fwag" "prfwag" "mix" "cap" "oth" ///
					"ben" "imp" "kap" "mir" "tot" "totnb" "totesn" "totesb" ///
					"oth" "pen" {
					
					qui cap drop ind_pre_`var'_svy ind_pre_`var'
					qui cap gen ind_pre_`var'_svy = ///
						ind_pos_`var'_svy / (1 - eff_tax_rate_ipol)
					qui cap gen ind_pre_`var' = ///
						ind_pos_`var' / (1 - eff_tax_rate_ipol)	
				}	
				qui save `svy_pre', replace
				
			}
			else {
				di in red "`c' in `year' does not have corrected survey (`svy_pos')"
			}
		}		
	}
}

*-------------------------------------------------------------------------------
* III. Subtract social contributions in Brazil in pre-tax and post-tax
* (exceptional case)
*-------------------------------------------------------------------------------
local ext ""
forvalues  year = $first_y / $last_y {
	
	//locate relevant files 
	foreach x in "pre" "pos" {
		clear
		
		local BRA_svy_`x' ///
			"${data_svy}bfm`ext'_`x'/BRA_`year'_bfm`ext'_`x'.dta"
		di as result "working with `BRA_svy_`x'' ..." 
		cap use `BRA_svy_`x'', clear
		qui cap assert _N == 0
		
		if _rc != 0 {
			foreach var in "wag" "fwag" "tot" {			
				qui replace `x'_`var'_svy = `x'_`var'_svy ///
					- socsec_valid_contribs_svy
				qui replace ind_`x'_`var' = ind_`x'_`var' ///
					- socsec_valid_contribs	
				qui qui replace `x'_`var'_svy = 0 if `x'_`var'_svy < 0
				qui replace ind_`x'_`var' = 0 if ind_`x'_`var' < 0
			}
		qui save `BRA_svy_`x'', replace		
		di as result " done"
		} 
		else {
			di as error "not found"
		}
	}
}

