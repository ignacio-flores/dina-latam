/*=============================================================================*
Goal: Make country specific adjustments to raw survey data
Author: De Rosa, Flores, Morgan
*=============================================================================*/

clear all

//preliminary 
global aux_part  ""preliminary"" 
qui do "code/Stata/auxiliar/aux_general.do"

//needed for Stata 15/Windows
cap set matsize 11000 

foreach c in "ARG" "BRA" "CRI" /*"DOM"*/ {
	
	//loop over countries 
	di as text "Adjusting survey data (01f) for " _continue 
	di as result "`c' " _continue 
	di as text "at $S_TIME ..."
	
	// 1. Reweight Argentinian Survey to represent the national population
	// bc it only represents urban areas 
	if ("`c'"=="ARG") {
		
		wid, areas(AR) ind(npopul) ages(999 /*992*/) pop(i) clear
		qui keep if year >= $first_y

		//Loop over years
		forvalues y = $first_y / $last_y {
			qui levelsof value if year == `y', clean local(totalpop`y')
		}
		
		forvalues year = $first_y / $last_y {
			clear 	
			qui cap use ///
				"intermediary_data/microdata/raw/ARG/ARG_`year'_raw.dta", clear	
			
			qui cap assert _N == 0
			if _rc != 0 {
			
				local raw_weight "_fep"	
				local id "id_hogar"
				
				//Ensure weights are constant within household
				qui drop if `raw_weight' == 0
				qui egen weight2 = mean(`raw_weight'), by(`id')
				qui cap assert weight2 == `raw_weight'
				
				//Write definitive weights 
				qui replace `raw_weight' = weight2
				qui drop weight2
				qui drop if missing(`raw_weight') | `raw_weight' < 1
				
				//sum weights 
				qui sum `raw_weight'
				local poptot = r(sum)
				di as result "ARG `year' adj factor: " ///
					100/((`poptot'/`totalpop`year'')*100)
			
				
				if ("`poptot'" != "`totalpop`year''") {
					qui gen r_fep = 100/((`poptot'/`totalpop`year'')*100)
					qui cap drop n_fep 
					qui gen n_fep = `raw_weight' * r_fep		
					qui drop r_fep
				} 
			}
			
			qui cap assert _N == 0
			
			if _rc != 0 & !inlist("`year'", "2015")  {
				qui save "intermediary_data/microdata/raw/ARG/ARG_`year'_raw.dta", replace	
			}
		}
	}

	// 2. Impute missing social contributions in Brazilian survey
	if ("`c'"=="BRA") {
		do "code/Stata/BRA/BRA_impute_socsec_contribs.do"
	}

	//3. Include gross wages in Costa Rica's survey
	// (only for years overlapping with the tax data)
	if ("`c'"=="CRI") {	
		forvalues year = 2010/2018 { 
			
			di as result "`year'"

			qui cap use "intermediary_data/microdata/raw/CRI/CRI_`year'_raw.dta", clear	
			
				qui merge 1:1 id_hogar id_pers using ///
					"input_data/surveys_CEPAL/CRI/BYN/CRI_`year'_byn.dta", nogen
				
				* Compute taxes paid on primary and secondary salaries
				foreach t in tax1 tax2 tax taxm ind_pre_wag_gross ///
					ind_pre_fwag_gross pre_wag_svy_gross ///
					pre_fwag_svy_gross {
					qui cap drop `t'
				}
				qui gen tax1 = 0
				qui gen tax2 = 0
				qui replace tax1 = spmb - spmn if spmb!=. | spmn!=.
				qui replace tax2 = ssmb - ssmn if ssmb!=. | ssmn!=.
				qui gen tax = tax1 + tax2
				qui gen taxm = tax/12
				
				* Add difference to net wages to make them gross
				qui gen ind_pre_wag_gross = ind_pre_wag + tax
				qui gen ind_pre_fwag_gross = ind_pre_fwag + tax
				qui gen pre_wag_svy_gross = pre_wag_svy + taxm
				qui gen pre_fwag_svy_gross = pre_fwag_svy + taxm
				
				qui save ///
					"intermediary_data/microdata/raw/CRI/CRI_`year'_raw.dta", replace	
		}
	}
}

// 4. Correct Dom. Rep income variables to exclude private transfers
/*
if ("`c'"=="DOM") {
	
	local pf "pos"
	local pre_lab "`pre_lab'"
	
	forvalues year = 2016 / 2020 { 
		
		di as result "`c'`year'"

		qui cap use "${svypath}`c'/raw/`c'_`year'_raw.dta", clear
		
		cap drop `pf'_othp_svy
		qui gen `pf'_othp_svy = yotrp_pe
		cap drop `pf'_bencor_svy 
		qui gen `pf'_bencor_svy = `pf'_pen_svy + `pf'_oth_svy ///
			- `pf'_othp_svy
		cap drop `pf'_totcor_svy 	
		qui gen `pf'_totcor_svy =  `pf'_wag_svy + `pf'_pen_svy + ///
			`pf'_mix_svy + `pf'_cap_svy + `pf'_oth_svy - `pf'_othp_svy
		
		// Label monthly variables
		local endlab "(survey) - `pre_lab'"
		qui la var `pf'_othp_svy ///
			"Private transfer income `endlab'"
		qui la var `pf'_bencor_svy ///
			"Total Social benefits (excl. private transfers) `endlab'"			
		qui la var `pf'_totcor_svy ///
			"Total income (excl. private trasnfers) `endlab'"	
		
		//Annualize income variables 
		foreach var in "othp" "bencor" "totcor" {
			cap drop ind_`pf'_`var' 
			qui gen ind_`pf'_`var' = `pf'_`var'_svy * 12
		}
		
		// Label annual variables
		local endlab "(survey) - annual `pre_lab'"
		qui la var ind_`pf'_othp "Private transfers `endlab2'"
		qui la var ind_`pf'_bencor ///
			"Total Social benefits (excl. private transfers) `endlab2'"
		qui la var ind_`pf'_totcor ///
			"Total income (excl. private transfers) `endlab2'"
		
		// Save dataset
		qui save "${svypath}`c'/raw/`c'_`year'_raw.dta", replace	
	}
}
*/
