/*=============================================================================*
Goal: Clean and prepare country surveys
The do file's goal is to create the main income variables that are going
to be used in the entire project (PART I-III). In PART IV, it creates different 
alternatives of proxies of capital ownership, in order to impute undistributed 
profits at a later stage.
*=============================================================================*/

//General-----------------------------------------------------------------------
 
clear all
global aux_part  ""preliminary"" 
qui do "code/Stata/auxiliar/aux_general.do"  

*which surveys do I clean?  
local countries "$really_all_countries"

// Create directory if it doesnt exist 
	local dirpath "intermediary_data/microdata/raw"
	mata: st_numscalar("exists", direxists(st_local("dirpath")))
	if (scalar(exists) == 0) {
		mkdir "`dirpath'"
		display "Created directory: `dirpath'"
	}
 
 
//loop over countries and years 
foreach c in `countries' {  
	
	//manage pre/post tax prefixes 
	local pf "pos"
	local pre_lab "`pre_lab'"
	
	//exceptions 
	if inlist("`c'", "BRA", "CRI"){
		local pf "pre"
		local pre_lab "pretax"
	} 
	
	forvalues  y = $first_y / $last_y {

		//open file 
		qui clear
		qui cap use "input_data/surveys_CEPAL/`c'/`c'_`y'N.dta", clear
		
		*Only run when data exists
		qui cap assert _N == 0
		if _rc != 0 {
		
			//report activity in log 
			di as text "01e - part 1: Cleaning survey in " _continue 
			di as result "`c' - `y' " _continue 
			di as text "at $S_TIME"

			*-------------------------------------------------------------------
			*PART 0: preliminary
			*-------------------------------------------------------------------

			//
			cap qui replace _fep = round(_fep) 
			* Calculating household size
			qui gen mem = 1
			qui egen hh_size = sum(mem), by(id_hogar)
			*tag adults / number of adults in the household
			qui gen  adults = 0
			qui replace adults = 1 if edad > 19
			qui egen adults_house = sum(adults), by(id_hogar) // nº of adults
			*Social benefits (other than pensions)
			*qui gen y_sbe	= yoemp4

			*-------------------------------------------------------------------
			*PART I: create income variables
			*-------------------------------------------------------------------
		
			*put 0 where null 
			mvencode sys_pe yoemp_pe yjub_pe gan_pe ycap_pe yotr_pe ///
				yaim_he yto_pe yto_he, mv(0) override 

			* Ia: income variables 
			qui gen `pf'_wag_svy = sys_pe + yoemp_pe 	
			qui gen `pf'_pen_svy = yjub_pe // + yoemp4
			qui gen `pf'_mix_svy = gan_pe
			qui gen `pf'_bus_svy  = gan_pe if categ5_p==1 
			qui replace `pf'_bus_svy = 0 if `pf'_bus_svy==.
			qui gen `pf'_cap_svy = ycap_pe
			qui gen `pf'_oth_svy = yotr_pe
			qui gen `pf'_ben_svy = `pf'_pen_svy + `pf'_oth_svy
			qui gen `pf'_imp_svy = yaim_he / adults_house
			qui replace `pf'_imp_svy = 0 if edad < 20
			qui gen `pf'_kap_svy = `pf'_cap_svy + `pf'_imp_svy
			qui gen `pf'_mir_svy = `pf'_mix_svy + `pf'_imp_svy
			qui gen `pf'_tot_svy =  `pf'_wag_svy + `pf'_pen_svy + ///
				`pf'_mix_svy + `pf'_cap_svy + `pf'_oth_svy 
			qui gen `pf'_totnb_svy 	= `pf'_tot_svy - `pf'_bus_svy
			
			if "`c'" == "BRA" {
				qui gen `pf'_otn_svy = yotn_pe
				qui replace `pf'_tot_svy = `pf'_tot_svy + `pf'_otn_svy
			}
			
			* Ib: formal wages
			
			//define cases without formality info 
			if 	("`c'" == "BRA" & `y' == 1990) ///
				| ("`c'" == "ARG" & inrange(`y', 2000, 2002)) ///
				| ("`c'" == "BOL" & inrange(`y', 2014, 2015)) ///
				| ("`c'" == "SLV" & inrange(`y', 2000, 2001)) ///
				| ("`c'" == "CRI" & inrange(`y', 2000, 2001)) ///
				| ("`c'" == "URY" & `y' == 2000) ///
				/*| ("`c'" == "MEX" & `y' == 2000)*/ {
				qui cap gen formal = 0
			}
			
			//define formality 
			else {
				qui cap gen formal = 0
				qui cap replace formal = 1 ///
					if (cotiza_ee==1 | afilia_ee==1) & `pf'_wag_svy > 0
			}
			
			//formal wages and formal private sector 
			qui gen `pf'_fwag_svy = 0
			qui gen `pf'_prfwag_svy = 0 
			qui gen `pf'_prwag_svy = 0 
			qui replace `pf'_fwag_svy = `pf'_wag_svy if formal == 1
			qui replace `pf'_prwag_svy = `pf'_wag_svy if sector_ee == 2
			qui replace `pf'_prfwag_svy = `pf'_fwag_svy if sector_ee==2
			
			*Ic: income labels
			local endlab "(survey) - `pre_lab'"
			qui la var `pf'_wag_svy "Wages"
			qui la var `pf'_fwag_svy "Formal wages `endlab'"
			qui la var `pf'_prwag_svy "Wages from private sector `endlab'"
			qui la var `pf'_prfwag_svy "Private sector formal wages `endlab'"
			qui la var `pf'_pen_svy "Pensions `endlab'"
			qui la var `pf'_mix_svy "Mixed income `endlab'"
			qui la var `pf'_cap_svy "Capital income `endlab'"
			qui la var `pf'_oth_svy "Other transfer incomes than pensions `endlab'"
			qui la var `pf'_ben_svy "Total Social benefits `endlab'"
			qui la var `pf'_imp_svy "Imputed rents `endlab'"
			qui la var `pf'_kap_svy "All capital & mixed income `endlab'"
			qui la var `pf'_mir_svy "Mixed income plus imputed rents `endlab'"
			qui la var `pf'_tot_svy "Total income `endlab'"
			qui la var `pf'_totnb_svy "Total non-business income `endlab'"
			
			*check for extreme values 
			if "`c'" == "ARG" local extreme = 500 
			if "`c'" != "ARG" local extreme = 1000
			qui sum `pf'_tot_svy [w=_fep], meanonly 
			local ai = r(mean) 
			qui gen excluder = `pf'_tot_svy / `ai'
			qui count if excluder >= `extreme'
			local ctr = r(N)
			if `ctr' > 0 {
				di as text "extreme values: " _continue
				di as result `ctr'
				list id* edad categ5_p excluder if excluder >= `extreme'
				qui drop if excluder >= `extreme'	
			}
			cap drop excluder 
			
		}
		
		// Create directory if it doesnt exist 
		local dirpath "intermediary_data/microdata/raw/`c'"
		mata: st_numscalar("exists", direxists(st_local("dirpath")))
		if (scalar(exists) == 0) {
			mkdir "`dirpath'"
			display "Created directory: `dirpath'"
		}
		
		// Save microdata 
		qui cap assert _N == 0
		if _rc != 0 {		
			qui save ///
				"intermediary_data/microdata/raw/`c'/`c'_`y'_raw.dta", replace
		}	
	}
}

*------------------------------------------------------------------------
*PART I.I Country adjustments
*------------------------------------------------------------------------
			
* Brazil
qui do "code/Stata/BRA/svy_adj_BRA.do"
			
*-----------------------------------------------------------------------
*PART II: Equal-split incomes and annualization
*-----------------------------------------------------------------------

foreach c in `countries' {  
	
	//manage pre/post tax prefixes 
	local pf "pos"
	local pre_lab "`pre_lab'"
	
	//exceptions 
	if inlist("`c'", "BRA", "CRI"){
		local pf "pre"
		local pre_lab "pretax"
	} 
	
	forvalues  y = $first_y / $last_y {

		//open file 
		qui clear
		qui cap use "intermediary_data/microdata/raw/`c'/`c'_`y'_raw.dta", clear
		
		*Only run when data exists
		qui cap assert _N == 0
		if _rc != 0 {
		
			//report activity in log 
			di as text "01e - part 2: Equal-splitting in " _continue 
			di as result "`c' - `y' " _continue 
			di as text "at $S_TIME"
			
			* make sure variables do not exist already 
			foreach v in "totesb" "totesn" {
				cap drop `pf'_`v'_svy
			}
			
			* Equal-split incomes
			// narrow = among married couples, 
			// broad = among all adult persons in household
			qui gen spouse = (paren_ee <= 2)
			cap drop married 
			qui egen married = sum(spouse) if paren_ee <= 2, by(id_hogar)
			qui egen double `pf'_totesn_svy = sum(`pf'_tot_svy/married) ///
				if married<=2, by(id_hogar)
			qui replace `pf'_totesn_svy = `pf'_tot_svy if married == .
			qui la var `pf'_totesn_svy ///
				"Total equal-split (narrow) income `endlab'"
			qui egen `pf'_totesb_svy = ///
				sum(`pf'_tot_svy / adults_house), by(id_hogar)
			qui la var `pf'_totesb_svy ///
				"Total equal-split (broad) income `endlab'"
			
			//Get annualization factors for Argentina
			preserve 
				qui import excel "input_data/prices_CEPAL/cpi_arg.xlsx", ///
					sheet("datos") firstrow clear	
				qui rename (Años__ESTANDAR Meses País__ESTANDAR) ///
					(year month country) 
				qui keep year month country value
				qui destring year, replace 
				qui gen m = 1 if month == "Enero"
				qui replace m = 2 if month == "Febrero"
				qui replace m = 3 if month == "Marzo"
				qui replace m = 4 if month == "Abril"
				qui replace m = 5 if month == "Mayo"
				qui replace m = 6 if month == "Junio"
				qui replace m = 7 if month == "Julio"
				qui replace m = 8 if month == "Agosto"
				qui replace m = 9 if month == "Septiembre"
				qui replace m = 10 if month == "Octubre"
				qui replace m = 11 if month == "Noviembre"
				qui replace m = 12 if month == "Diciembre"
				
				*compute monthly factor 
				qui gen f_m = value if m == 11 
				bysort year: ereplace f_m = mean(f_m)
				qui replace f_m = value / f_m 
				
				*compute annual factor 
				qui collapse (sum) factor = f_m, by(country year)
			
				*qui destring factor, replace	
				qui sum factor if country == "Argentina" & year == `y'
				local f_ARG_`y' = r(mean)	
			restore 
				
			//Annualize income variables 	
			foreach var in ///
				"wag" "fwag" "prwag" "prfwag" "pen" "mix" "cap" "oth" ///
				"ben" "imp" "kap" "mir" "tot" "totnb" "totesn" "totesb" {
				if ("`c'" == "ARG" & `y' >= 2000) {
					qui gen ind_`pf'_`var' = `pf'_`var'_svy * `f_ARG_`y''
				} 
				else {
					cap drop ind_`pf'_`var' 
					qui gen ind_`pf'_`var' = `pf'_`var'_svy * 12
				}
			}
			
			//label income variables 
			local endlab "(survey) - annual `pre_lab'"
			qui la var ind_`pf'_wag "Wages `endlab2'"
			qui la var ind_`pf'_fwag "Formal wages `endlab2'"
			qui la var ind_`pf'_prwag "Private sector wages `endlab2'"
			qui la var ind_`pf'_prfwag "Private sect. formal wages `endlab2'"
			qui la var ind_`pf'_pen "Pensions `endlab2'"
			qui la var ind_`pf'_mix "Mixed income `endlab2'"
			qui la var ind_`pf'_cap "Capital income `endlab2'"
			qui la var ind_`pf'_oth "Other transfers than pensions `endlab2'"
			qui la var ind_`pf'_ben "Total Social benefits `endlab2'"
			qui la var ind_`pf'_imp "Imputed rents `endlab2'"
			qui la var ind_`pf'_kap "Capital inc., w/ imputed rents `endlab2'"
			qui la var ind_`pf'_mir "Mixed income & imputed rents `endlab2'"
			qui la var ind_`pf'_tot "Total income `endlab2'"
			qui la var ind_`pf'_totnb "Total non-business income `endlab2'"
			qui la var ind_`pf'_totesn ///
				"Total equal-split (narrow) inc. `endlab2'"
			qui la var ind_`pf'_totesb ///
				"Total equal-split (broad) inc. `endlab2'"
			
			*Variables for comparison (CEPAL tot inc & WB/CEDLAS series)
			cap drop percap_inc_cep
			cap confirm variable yto_he
			if _rc == 0 qui gen percap_inc_cep = (yto_he * 12) / hh_size
			else {
				qui gen percap_inc_cep =  (yto_pe * 12) / hh_size  
				else di as text "   * yto_he not found " _continue
				di as text "using yto_pe instead"
			} 
			qui la var percap_inc_cep 	"Per capita household income `pre_lab'"
			
			cap drop ind_`pf'_tot_imp
			qui gen ind_`pf'_tot_imp = ind_`pf'_tot + ind_`pf'_imp
			qui la var ind_`pf'_tot_imp "Total income w/ imp- rents `endlab2'"
			
			*-------------------------------------------------------------------
			*PART III: Impute missing incomes 
			*-------------------------------------------------------------------
			
			*check if imputed rents is empty
			qui sum ind_`pf'_imp, meanonly 
			if "`macro_imp'" == "" {
				*define reference income
				local varli ""
				foreach var in "wag" "pen" "cap" "mix" {
					local varli "`varli' ind_`pf'_`var'"
				}
				tempvar aux1 
				qui egen `aux1' = rowtotal(`varli')
				*replace with 10% of it 
				qui replace ind_`pf'_imp = 0.1 * `aux1'
			}
			
			*-------------------------------------------------------------------
			*PART IV: Keep the necessary variables and save a data base:
			*-------------------------------------------------------------------

			*save important variables 
			if "`c'" == "BRA"  {
				qui keep condact3 sys_pe yjub_pe yoemp_pe *_svy ind_* ///
					_fep id_hogar id_pers paren_ee edad sexo adults_house ///
					hh_size percap_inc_cep li lp pobreza yto_pe ///
					formal sector_ee categ5_p married ///
					ramar_ee tamest_ee cotiza_ee //`pf'_sys_13th `pf'_hol_svy  afilia_ee
			}  
			
			else if "`c'" == "DOM" & inrange(`y',2016,2020) {				
				qui keep yotrp_pe *_svy ind_* _fep id_hogar id_pers ///
					paren_ee married edad sexo adults_house ///
					hh_size percap_inc_cep  li ///
					lp pobreza yto_pe formal sector_ee categ5_p ///
					ramar_ee tamest_ee
			} 
			
			else {				
				qui keep *_svy ind_* _fep id_hogar id_pers ///
					paren_ee married edad sexo adults_house ///
					hh_size percap_inc_cep  li ///
					lp pobreza yto_pe formal sector_ee categ5_p ///
					ramar_ee tamest_ee
			} 
		}
		
		qui cap assert _N == 0
		if _rc != 0 {
			qui save ///
				"intermediary_data/microdata/raw/`c'/`c'_`y'_raw.dta", replace
					 
		}	
	}
}

