// Direct and runner execution share country selection and staged publication.
args dina_requested_countries
local dina_country_worker : environment DINA_COUNTRY_WORKER
if "`dina_country_worker'" != "04b-build-pretax-surveys" {
    do "code/Stata/auxiliar/dina_country_task.do" "04b-build-pretax-surveys" "`dina_requested_countries'"
    exit
}

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

foreach c in $dina_run_countries {
	dina_coverage, country("`c'") step("synthetic_rates") status("expected")
	clear
	local found 0
	if "`c'" == "ARG" {
		forvalues arg_year = 2002/2019 {
			if inrange(`arg_year', $first_y, $last_y) {
				local arg_rate_file "$data_tax/ARG/eff-tax-rate/ARG_effrates_`arg_year'.dta"
				capture confirm file `arg_rate_file'
				if _rc {
					di as error "ARG `arg_year' rate file is missing; run 04a5-generate-arg-rates."
					exit 601
				}
			}
		}
	}
	forvalues  year = $first_y / $last_y {
	
		local eff_tax_file ///
			"$data_tax/`c'/eff-tax-rate/`c'_effrates_`year'.dta" 
		local syneff_tax_file 	///
			"${dina_output_root}/$data_tax/`c'/eff-tax-rate/`c'_effrates_syn.dta"
		
		*Take the first eff_tax_rate file, and merge with all the following
		local year_p = `year' + 1
		qui cap confirm file `eff_tax_file'
		local rate_found = (_rc == 0)
		if `rate_found' == 1 {
			local found 1
			di as result  "`eff_tax_file' found"
			qui use `eff_tax_file', clear
			
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
	if `found' == 0 {
		di as error "`c': no yearly effective tax rates found; cannot build a synthetic rate."
		exit 601
	}

	//write them down
	qui egen eff_tax_rate_ipol_syn = rmean(eff_tax_rate_ipol*)
	qui cap egen eff_ss_rate_ipol_syn  = rmean(eff_ss_rate_ipol*)
	assert !missing(eff_tax_rate_ipol_syn)
	isid p_merge
	
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
	qui save `syneff_tax_file', replace
	dina_coverage, country("`c'") step("synthetic_rates") status("success")
}

*-------------------------------------------------------------------------------
* II. We merge corrected surveys and effective tax rates
*-------------------------------------------------------------------------------

foreach c in $dina_run_countries   {
	forvalues  year = $first_y / $last_y {
	
		//locate relevant files 
		foreach x in "pre" "pos" {
			local svy_`x' ///
				"${data_svy}bfm`ext'_`x'/`c'_`year'_bfm`ext'_`x'.dta"
		}
		local eff_tax_file "$data_tax/`c'/eff-tax-rate/`c'_effrates_`year'.dta" 
		local syneff_tax_file "${dina_output_root}/$data_tax/`c'/eff-tax-rate/`c'_effrates_syn.dta"
	
		* Brazil already has pre-tax survey, we compute post-tax incomes
		if ("`c'" == "BRA" | "`c'" == "CRI") {
			clear
			capture confirm file `svy_pre'
			if !_rc quietly use `svy_pre', clear
			// A published 04b file already has the accepted survey ranking.
			// Re-ranking its truncated sample would remove more top observations.
			local dina_reuse_fractiles 0
			capture confirm numeric variable p_merge fractiles eff_tax_rate_ipol
			if !_rc local dina_reuse_fractiles 1
			// Recompute derived rates when a previous 04b result is reused.
			// Keep the original pretax income fields as the calculation inputs.
			capture drop eff_tax_rate_ipol*
			capture drop eff_ss_rate_ipol*
	
			qui cap assert _N == 0
				if _rc != 0 {
				dina_coverage, country("`c'") year(`year') step("pretax_survey") status("expected")
						
					//create fractiles in survey to prepare merge
					global prefix "pre"
					global section ""fractiles""
					if `dina_reuse_fractiles' {
						capture assert !missing(p_merge, fractiles) & inrange(p_merge, 0, 9999) & p_merge == floor(p_merge) & inrange(fractiles, 0, 1)
						if _rc {
							di as error "`c' `year': existing 04b fractiles are invalid; regenerate the BFM input."
							exit 459
						}
					}
					else {
						qui do "$codes/auxiliar/aux_pretax.do"
					}
				
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
						qui merge m:1 p_merge using `syneff_tax_file'
						rename eff_tax_rate_ipol_syn eff_tax_rate_ipol
					}
	
				qui cap drop if _merge == 2
				
				//create fractiles in survey to prepare merge
				global section ""adjustment""
				qui do "$codes/auxiliar/aux_pretax.do"
				assert !missing(eff_tax_rate_ipol)
					
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
				qui save "${dina_output_root}/`svy_pos'", replace
				qui save "${dina_output_root}/`svy_pre'", replace
				dina_coverage, country("`c'") year(`year') step("pretax_survey") status("success")

			}
		}
		
		* For the rest, we compute all pre-tax incomes 
		*(= proportion as total income for each source)
		else {		
			clear
			capture confirm file `svy_pos'
			if !_rc quietly use `svy_pos', clear
			cap drop __*
			
			qui cap assert _N == 0
			if _rc != 0 {
				dina_coverage, country("`c'") year(`year') step("pretax_survey") status("expected")
				
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
					qui merge m:1 p_merge using `syneff_tax_file'
					rename eff_tax_rate_ipol_syn eff_tax_rate_ipol

				}
				
				qui cap drop if _merge == 2
	
				//create fractiles in survey to prepare merge
				global section ""adjustment""
				qui do "$codes/auxiliar/aux_pretax.do"
				assert !missing(eff_tax_rate_ipol)
				
				foreach var in 	"wag" "fwag" "prfwag" "mix" "cap" "oth" ///
					"ben" "imp" "kap" "mir" "tot" "totnb" "totesn" "totesb" ///
					"oth" "pen" {
					
					qui cap drop ind_pre_`var'_svy ind_pre_`var'
					qui cap gen ind_pre_`var'_svy = ///
						ind_pos_`var'_svy / (1 - eff_tax_rate_ipol)
					qui cap gen ind_pre_`var' = ///
						ind_pos_`var' / (1 - eff_tax_rate_ipol)	
				}	
				qui save "${dina_output_root}/`svy_pre'", replace
				dina_coverage, country("`c'") year(`year') step("pretax_survey") status("success")
				
			}
			else {
				di in red "`c' in `year' does not have corrected survey (`svy_pos')"
			}
		}		
	}
}
