////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
//
// 							Title: IMPUTE DISPOSABLE INCOME
// 			 Authors: Mauricio DE ROSA, Ignacio FLORES, Marc MORGAN 
// 									Year: 2022
//
////////////////////////////////////////////////////////////////////////////////

clear all

//0. General settings ----------------------------------------------------------
if "${bfm_replace}" == "yes" local ext ""
if "${bfm_replace}" == "no" local ext "_norep"

//get list of paths 
global aux_part " "preliminary" " 
qui do "code/Stata/auxiliar/aux_general.do"
global aux_part " "graph_basics" " 
qui do "code/Stata/auxiliar/aux_general.do"

local lang $lang 
local interpolation "spline" //for data from ceq
local ds "oecd_gni" //select database gni gdp 

//1. Fetch taxes and social assistance transfers from SNA/OECD/WID -------------

tempfile tf_ceq tf_sna_tax

*prepare extension of variables  
if "`interpolation'" == "spline" local i "si"
if "`interpolation'" == "linear" local i "li"

//`u'_pre_fac_sca (factor income distribution for production taxes)
//more detailed ceq data? 

//import ceq data 
qui use country ctry_yr year ftile ///
	sh_`i'_directtaxe sh_`i'_indirectta sh_`i'_vat sh_`i'_allcontrib ///
	sh_`i'_conditiona sh_`i'_directtran sh_`i'_disposable sh_`i'_indirectsu ///
	sh_`i'_education sh_`i'_health ///
	using  "input_data/CEQ/_clean/concentration/no_ssc.dta" 	
qui rename (sh_`i'_* ctry_yr) (sh_*_ceq ctry_yr_ceq) 
qui drop if inlist(ctry_yr, "COL_2010", "MEX_2010", "MEX_2012", "PER_2009")
qui drop year

//harmonise names (we should probably change indg name to avoid confusion)
qui rename (sh_indirectta* sh_allcontrib*) (sh_indg* sh_ssc*)
qui save `tf_ceq' 

//import SNA sheets
local taxlist "tot pit_tot pit_corp pit_hh ssc indg"
local taxlist2 "${oecd_taxes}"
foreach v in `taxlist' `taxlist2' {
	local taxlist_full `taxlist_full' tax_`v'_`ds' 
}

//bring tax data 
qui use country year `taxlist_full' uprofits_hh_ni gdp_wid TOT_B5g_wid ///
	using "intermediary_data/national_accounts/UNDATA-WID-OECD-Merged.dta", clear
//interpolate if necessary and save 
foreach tv in `taxlist_full' {
	qui replace `tv' = . if `tv' == 0 
}	
global imput_vars `taxlist_full' 
qui do "code/Stata/auxiliar/aux_fill_aver.do"
qui save `tf_sna_tax', replace

//import in-kind aggregate data from SNA-WID-OECD data
local spenvars /*oex_gni hea_gni edu_gni*/ oex_gdp_wid hea_gdp_wid edu_gdp_wid
qui use country year `spenvars' gdp_wid TOT_B5g_wid gdp_to_gni ///
	using "intermediary_data/national_accounts/UNDATA-WID-OECD-Merged.dta", clear	

*convert to % of ni 
foreach v in `spenvars' {	
	qui replace `v' = (`v' / 100) * gdp_to_gni
}	
qui rename *_gdp_wid *_gni

*interpolate/extrapolate if necessary
qui keep if year >= $first_y
qui sort country year 
//interpolate if necessary and save 
foreach sv in hea_gni edu_gni oex_gni {
	display as result "`tv'"
	qui replace `sv' = . if `sv' == 0 
}	
global imput_vars hea_gni edu_gni oex_gni 
qui do "code/Stata/auxiliar/aux_fill_aver.do"


*graph twoway (line hea_gni edu_gni oex_gni year) ///
*	(scatter hea_gni edu_gni oex_gni year if extrap_sca == 1), by(country)
qui drop extrap_*

*save for later 
tempfile tf_sna_spending
qui save `tf_sna_spending', replace

*interpolate missing fractiles 
tempfile tf109
// build 127 percentiles again from scratch
clear
quietly set obs 109
quietly gen ftile_sca = (_n - 1)/100 in 1/100
quietly replace ftile_sca ///
	= (99 + (_n - 100)/10)/100 in 101/109
*quietly replace ftile_sca ///
*	= (99.9 + (_n - 109)/100)/100 in 110/118
*quietly replace ftile_sca ///
*	= (99.99 + (_n - 118)/1000)/100 in 119/127
qui replace ftile_sca = round(ftile_sca * 10^5)
qui save `tf109', replace 

//2. Impute taxes and social assistance transfers to surveys -------------------

//2.0 Check existence of data and open

//loop over countries and years 
local a = 1 
global area " ${all_countries} "
foreach c in $area {	
	forvalues y = $first_y/$last_y { 

		clear

		if `a' == 1 {
			di as result "{hline 80}"
			display as result "imputing taxes and transfers ..."
			di as result "{hline 80}"
			local a = 0 
		}
		
		//exceptions
		if inlist("`c'`y'", "ARG2016", "CRI2002", "CRI2003", "CRI2005") { 
			continue
		}	
		
		// prepare effective rates of personal income tax
		tempfile effrates
		capture confirm file ///
			"input_data/admin_data/`c'/eff-tax-rate/`c'_effrates_`y'.dta"
		if _rc==0  {
			qui u "input_data/admin_data/`c'/eff-tax-rate/`c'_effrates_`y'.dta", clear
		}
		else {
			qui u "input_data/admin_data/`c'/eff-tax-rate/`c'_effrates_syn.dta", clear
			qui rename eff_tax_rate_ipol_syn eff_tax_rate_ipol
		}

		qui gen ftile_bfm = round(p_merge * 10) // * 10^5
		qui cap drop country
		qui gen country = "`c'"
		qui cap drop year
		qui gen year = `y' 
		qui save `effrates'
		
		//confirm corrected survey exists 
		local survey "intermediary_data/microdata/bfm`ext'_pre/`c'_`y'_bfm`ext'_pre.dta"
		capture confirm file "`survey'"
		if _rc==0 {
			
			*inform activity
			di as text "`c' `y': " 
			
			//open it
			qui use "`survey'", clear
				qui cap drop __*
			// 2.1 	Rank individuals by income

			*CEQ estimates rank individuals (not hhlds)
			qui cap drop inc
			qui egen inc = rowtotal(${ind_raw_norm})
			
			// Classify obs in g-percentiles
			qui replace _weight = round(_weight)
			tempvar freq F
			gsort -inc
			qui sum inc [fw=_weight]
			qui gen `freq'	= _weight / r(N)
			qui gen `F' = 1 - sum(`freq')
				
			// compute cumulative frequencies of original weights 	
			cap drop ftile
			sort `F'
			qui egen ftile = cut(`F'), ///
				at(0(0.01)0.99 0.991(0.001)0.999 ///
				0.9991(0.0001)0.9999 0.99991(0.00001)0.99999 1)	
			qui replace ftile = 0 if missing(ftile)	
			qui replace ftile = round(ftile * 10^5)
			
			//2.2 Impute taxes on production (and SSC) based on CEQ 

			// Merge with ceq incidence/concentration
			qui cap drop country
			qui gen country = "`c'"
			qui cap drop year
			qui gen year = `y'
			qui merge m:1 country ftile using `tf_ceq', nogen keep(2 3)
			qui keep if country == "`c'" & ftile != 100000
				
			// add up non-allocated shares 
			qui gsort ftile 
			qui count if missing(_weight) 
			while (r(N) > 0) {
				tempvar id newid queue 
				qui gen `queue' = sum(missing(_weight)) ///
					if missing(_weight)
				qui gen `id' = _n 
				qui gen `newid' = `id'[_n - 1] if missing(_weight)
				qui replace `id' = `newid' if missing(_weight)
				qui ds sh_* `id', not
				*merge empty shares with previous observation 
				qui collapse (sum) sh_* (firstnm) `r(varlist)', by(`id')
				qui count if missing(_weight)
			}
			
			//merge with totals from OECD/UN
			qui merge m:1 country year using `tf_sna_tax', nogen keep(3) 
			qui merge m:1 country year using `tf_sna_spending', nogen keep(3) 
			
			//count ppl in each fractile 
			cap drop ftile_N
			bysort ftile: egen ftile_N = total(_weight)
	
			*impute indirect taxes (and ssc)  
			foreach t in indg ssc gog goo {
				
				*make room for variables 
				qui cap drop tax_`t'_lcu
				qui cap drop sh_`t'_lcu
				foreach uni in ind esn esb pch {
					cap confirm variable `uni'_tax_`t', exact 
					if _rc == 0 cap drop `uni'_tax_`t'
				}
			
				*total taxes in lcu 
				qui cap drop tax_`t'_lcu 
				qui gen tax_`t'_lcu = tax_`t'_`ds' * TOT_B5g_wid
				qui la var tax_`t'_lcu ///
					"${labtax_`t'}, Total, in current LCU"	
				
				*use share of tax or proxy 
				local imputer sh_indg_ceq
				qui cap drop sh_`t'_lcu 
				qui gen sh_`t'_lcu = tax_`t'_lcu * `imputer'
		
 				*qui la var sh_`t'_lcu = "Fractile share of ${lab_`t'}"		
				cap confirm variable ind_tax_`t', exact 
					if _rc == 0 cap drop ind_tax_`t'
				qui gen ind_tax_`t' = sh_`t'_lcu / ftile_N
				qui la var ind_tax_`t' ///
					"Individual tax, ${labtax_`t'}"
				
				*compute indirect taxes by couple (narrow)
				cap confirm variable esn_tax_`t', exact 
					if _rc == 0 cap drop esn_tax_`t' 
				qui egen esn_tax_`t'= sum(ind_tax_`t' / married) ///
					if married <= 2, by(id_hogar)
				qui cap replace esn_tax_`t' = ///
					ind_tax_`t' if missing(married)
				qui cap la var esn_tax_`t' ///
					"Eq-split narrow tax ${labtax_`t'}"
				
				*compute indirect taxes by adults (broad)
				/*
				cap confirm variable esb_tax_`t', exact 
					if _rc == 0 cap drop esb_tax_`t' 
				qui egen esb_tax_`t' = ///
					sum(ind_tax_`t'/adults_house), by(id_hogar)
				qui la var esb_tax_`t' ///
					"Eq-split broad tax ${labtax_`t'}"
				*/	
				*compute indirect taxes by household 
				cap confirm variable pch_tax_`t', exact 
					if _rc == 0 pch_tax_`t' 
				qui egen pch_tax_`t' = ///
					sum(ind_tax_`t' / hh_size), by(id_hogar)
				qui la var pch_tax_`t' ///
					"Per capita hld. tax ${labtax_`t'}"	
			}

			//2.3 Impute taxes % to inc vars (corporate inc, payroll, 
			*immovable property and wealth) 
			foreach z in cit prl imo wea otp oth est {
				*define variable for imputation 
				if inlist("`z'", "cit", "wea", "otp") local v_svy "upr_2"
				if "`z'" == "prl" local v_svy "pre_wag"
				if "`z'" == "imo" local v_svy "pre_imp"
				if inlist("oth", "est") local v_svy "pre_tot"		
				*impute wealth tax only where it exists 
				if "`z'" == "wea" {
					qui sum tax_`z'_oecd_gni, meanonly 
					if r(mean) == 0 {
						local wea_`c'_`y' "no"
						continue 
					}
					else local wea_`c'_`y' "yes"
				}	
				*impute tax 
				qui cap drop tax_`z'_lcu
				qui gen tax_`z'_lcu = tax_`z'_oecd_gni * TOT_B5g_wid
				foreach uni in "$unit_list" {
					if "`uni'" != "act" {
						qui cap drop `uni'_sh_`v_svy'
						qui sum `uni'_`v_svy' [w=_weight], meanonly 	
						qui gen `uni'_sh_`v_svy' = `uni'_`v_svy' / r(sum)
						qui cap drop `uni'_tax_`z'
						qui gen `uni'_tax_`z' = tax_`z'_lcu * `uni'_sh_`v_svy'
					}
				}
			}	
			
			//2.4 Impute personal income tax based on admin data 
			
			//define income of reference 
			local fiscal_inc ${y_postax_tot}
			if ("`c'" == "BRA") local fiscal_inc ${y_pretax_tot_bra}
			if ("`c'" == "BRA") local fiscal_inc ${y_pretax_tot_bra} 
			if ("`c'" == "CRI") local fiscal_inc ${y_pretax_tot} 
			if ("`c'" == "MEX") local fiscal_inc ${y_postax_formal_wage}
			if ("`c'" == "PER") local fiscal_inc ${y_postax_tot_per}
				
			//total pre-tax national income
			*prepare list of exception countries 
			global ex_ct 
			local iter_exep = 1 
			foreach ct in ${exep_countries} {
				if `iter_exep' == 1 global ex_ct "`ct'"
				else global ex_ct "`ct'", "$ex_ct"
				local iter_exep = 0 
			}
			
			*create variable
			foreach uni in ind esn /*esb*/ pch {
				cap drop `uni'_pre_tot_sca
				if !inlist("`c'", "${ex_ct}"){ 
					egen `uni'_pre_tot_sca = rowtotal(${`uni'_nat_norm}) 
				} 
				if  inlist("`c'", "${ex_ct}"){
					egen `uni'_pre_tot_sca = rowtotal(${`uni'_nat_exep}) 
				} 
			}
			

			// Classify obs in g-percentiles
			foreach inc in "`fiscal_inc'" "ind_pre_tot_sca" {
				if "`inc'" == "`fiscal_inc'" 		local end "bfm" 
				if "`inc'" == "ind_pre_tot_sca" 	local end "sca" 
				tempvar F_`end'
				gsort -`inc'
				qui sum `inc' [fw = _weight]
				qui gen `F_`end'' = 1 - sum(`freq')
				sort `F_`end''
					
				// compute cumulative frequencies of original weights 	
				cap drop ftile_`end'
				qui egen ftile_`end' = cut(`F_`end''), ///
					at(0(0.05)0.99 0.991(0.003)0.999 1)	
				*gen F = `F_`end''	
				qui replace ftile_`end' = 0 if missing(ftile_`end')	
				qui replace ftile_`end' = round(ftile_`end' * 10^5)
			}
			
			// Merge with effective tax rates
			cap drop eff_tax_rate_ipol
			cap drop _merge 
			qui merge m:1 country year ftile_bfm using "`effrates'", ///
				keep(1 3) nogen
			qui sort `F_bfm'	
			qui replace eff_tax_rate_ipol = 0 if missing(eff_tax_rate_ipol)
			
			
			//make room for variables 
			foreach uni in ind esn esb pch {
				cap drop `uni'_tax_pit
			}
			
			//impute tax burden to individuals 
			qui gen ind_tax_pit = eff_tax_rate_ipol * `fiscal_inc' 
			qui la var ind_tax_pit "Per capita hld. tax ${labtax_pit}"	
			
			*compute pit taxes by couple (narrow)
			qui egen esn_tax_pit = sum(ind_tax_pit /married) ///
				if married <= 2, by(id_hogar)
			qui cap replace esn_tax_pit = ind_tax_pit if missing(married)
			qui cap la var esn_tax_`t' "Eq-split narrow tax ${labtax_pit}"
			
			*compute pit taxes by adults (broad)
			/*
			qui egen esb_tax_pit = sum(ind_tax_pit/adults_house), by(id_hogar)
			qui la var esb_tax_pit "Eq-split broad tax ${labtax_pit}"
			*/
			
			*compute pit indirect taxes by household 
			qui cap drop pch_tax_pit 
			qui egen pch_tax_pit = sum(ind_tax_pit / hh_size), by(id_hogar)
			qui la var pch_tax_pit "Per capita hld. tax ${labtax_pit}"	
			
			//Deduct all imputed taxes 
			foreach u in ind esn /*esb*/ pch {
				
				*make room 
				foreach iii in tax_tot_sca pod_tot_wmbe tax_not_sca ///
					pod_tot psp_tot psp_tot_wmbe etr_tot mbr_tot {
					cap drop `u'_`iii'
				}

				if "`wea_`c'_`y''" == "no" local wea ""
				if "`wea_`c'_`y''" == "yes" local wea `u'_tax_wea
				
				*define total taxes 
				qui egen `u'_tax_tot_sca = ///
					rowtotal(`u'_tax_goo `u'_tax_gog `u'_tax_cit ///
					`u'_tax_pit `u'_tax_prl `u'_tax_imo `wea' ///
					`u'_tax_est `u'_tax_otp `u'_tax_oth)
				qui egen `u'_tax_not_sca = ///
					rowtotal(`u'_tax_cit `u'_tax_pit `u'_tax_prl ///
					`u'_tax_imo `wea' `u'_tax_est `u'_tax_otp `u'_tax_oth)	
				//total post-tax disposable income (excl. monetary benefits)
				qui gen `u'_pod_tot_wmbe = `u'_pre_tot_sca - `u'_tax_tot_sca 
				qui lab var `u'_pod_tot_wmbe ///
					"Total income, posttax-disposable excl. monetary benefits"
				qui gen `u'_psp_tot_wmbe = `u'_pre_tot_sca - `u'_tax_not_sca 
				qui lab var `u'_psp_tot_wmbe ///
					"Total income, posttax-spendable excl. monetary benefits"	
				//monetary benefits 
				qui replace `u'_pre_mbe_sca = 0 if missing(`u'_pre_mbe_sca) 
				//create post-tax-disposable variable
				qui gen `u'_pod_tot = ///
					`u'_pre_tot_sca - `u'_tax_tot_sca + `u'_pre_mbe_sca
				qui lab var `u'_pod_tot "Total income, posttax-disposable"
				//create post-tax-spendable variable
				qui gen `u'_psp_tot = ///
					`u'_pre_tot_sca - `u'_tax_not_sca + `u'_pre_mbe_sca
				qui lab var `u'_psp_tot "Total income, posttax-spendable"
				//create total tax effective rates				
				qui gen `u'_etr_tot = `u'_tax_tot_sca / `u'_pre_tot_sca
				qui lab var `u'_etr_tot "Total imputed taxes effective rate"
				//create total monetary benefits effective rates
				qui gen `u'_mbr_tot = `u'_pre_mbe_sca / `u'_pre_tot_sca
				qui lab var `u'_mbr_tot "Total monetary benefits effective rate"
				
				//avoid negative values (added 18may2022)
				foreach vvv in `u'_pre_mbe_sca `u'_pod_tot_wmbe {
					qui replace `vvv' = 0 if `u'_pod_tot < 0
				}
				
			}
			//5F----------------------------------------------------------------
			
			*impute health and education transfers 
			foreach t in oex hea edu {
				qui cap drop exp_`t'_lcu
				qui gen exp_`t'_lcu = `t'_gni * TOT_B5g_wid
				qui la var exp_`t'_lcu "${lab_`t'}, Total, in current LCU"
				qui cap drop sh_`t'_lcu
			}
			
			qui gen sh_hea_lcu = exp_hea_lcu * sh_health_ceq
			qui gen sh_edu_lcu = exp_edu_lcu * sh_education_ceq
			foreach t in hea edu {
 				qui cap drop ind_exp_`t'
				qui gen ind_exp_`t' = sh_`t'_lcu / ftile_N
			}
			
			*impute other in-kind transfers
			*(proportionally to disposable income)
			qui cap drop sh_ind_pod_tot 
			qui sum ind_pod_tot [w=_weight], meanonly 	
			qui gen sh_ind_pod_tot = ind_pod_tot / r(sum)
			qui cap drop ind_exp_oex
			qui gen ind_exp_oex = exp_oex_lcu * sh_ind_pod_tot

			//aggregate all imputed spending
			qui cap drop ind_exp_tot 
			qui egen ind_exp_tot = ///
				rowtotal(ind_exp_hea ind_exp_edu ind_exp_oex)
			qui lab var ind_exp_tot "Total imputed in-kind transfers"
			
			//create post-tax-national income variable
			qui cap drop ind_pon_tot 
			qui gen ind_pon_tot = ind_pod_tot + ind_exp_tot
			qui lab var ind_pon_tot "Total income, posttax-national"

			//create total in-kind transfer effective incidence rates
			qui cap drop ind_exr_tot 
			qui gen ind_exr_tot = ind_exp_tot / ind_pod_tot	
			
			//create categories of in-kind transfers 
			foreach t in oex hea edu {
				qui cap drop ind_pon_`t'
				qui gen ind_pon_`t' = ind_exp_`t'
				qui la var ind_pon_`t' "${lab_`t'}, Total, in current LCU"
			}
			qui cap drop ind_exr_tot 
			qui gen ind_exr_tot = ind_exp_tot / ind_pod_tot
			qui lab var ind_exr_tot "Total in-kind transfers incidence rate"
			
			//generate expenditure variables for other units 
			foreach vz in exp_oex exp_hea exp_edu exp_tot exr_tot ///
				pon_tot pon_oex pon_hea pon_edu {
				*compute expenditures by couple (narrow)
				cap drop esn_`vz'
				qui egen esn_`vz' = sum(ind_`vz'/married) ///
					if married <= 2, by(id_hogar)
				qui replace esn_`vz' = ind_`vz' if missing(married)
				*compute expenditures by adults (broad)
				/*
				cap drop esb_`vz'
				qui egen esb_`vz' = sum(ind_`vz'/adults_house), by(id_hogar)
				*/
				*compute pit indirect taxes by household 
				cap drop pch_`vz' 
				qui egen pch_`vz' = sum(ind_`vz' / hh_size), by(id_hogar)
			}	
			
			// save database			
			qui save "`survey'", replace 

			*3. Draw effective rates 
			
			*loop over units 
			foreach u in "$unit_list"  {
				
				if "`u'" != "act" {
					
					preserve 
						if "`wea_`c'_`y''" == "no" local wea
						if "`wea_`c'_`y''" == "yes" local wea wea
						if inlist("`u'", "esn", "esb", "ind") {
							qui drop if edad < 20
						} 
						if inlist("`u'", "act") {
							qui drop if !inrange(edad, 20, 65)
						} 
						local iter = 1 
						foreach t in cit `wea' est otp imo pit prl ///
							/*indg*/ goo gog oth {
							qui cap drop `u'_etr_`t'
							qui gen `u'_etr_`t' = ///
								`u'_tax_`t' / `u'_pre_tot_sca
							local sckvs_`u'_`c'_`y' `u'_etr_`t' ///
								`sckvs_`u'_`c'_`y'' 
							local gphas_`u'_`c'_`y' `gphas_`u'_`c'_`y''  ///
								(area fp_`u'_etr_`t' /*ftile_sca*/ n, ///
								lwidth(none) color(${c_`t'})) 
							local  lgd_`u'_`c'_`y' `lgd_`u'_`c'_`y'' ///
								`iter' "${labtax_`t'}"
							local iter = `iter' + 1
						}
						local iter2 = `iter' + 1

						global xlab /*1"P0" 11"P10"*/ 21"P20" 31"P30" ///
							41"P40" 51"P50" 61"P60" 71"P70" 81"P80" ///
							91"P90" 100 "P99" 109 "P99.9" /*
							118 "P99.99" 127 "P99.999" */
							
						//rerank 
						local inc `u'_pre_tot_sca
						tempvar F freq 
						qui sum _weight, meanonly 
						local tp = r(sum)
						qui gen `freq' = _weight / `tp'
						cap drop ftile_sca
						qui gsort -`inc'
						qui sum `inc' [fw = _weight]
						qui gen `F' = 1 - sum(`freq')
						sort `F'
						qui gen F = `F' 
						// mira los dos parrafos adelante
						// compute cumulative frequencies of original weights 	
						cap drop ftile_sca
						qui egen ftile_sca = cut(`F'), ///
							at(0(0.05)0.99 0.991(0.003)0.999 1)	
						qui replace ftile_sca = 0 if missing(ftile_sca)	
						qui replace ftile_`end' = round(ftile_`end' * 10^5)
						
						*summarize distribution 
						qui collapse (mean) `u'_mbr_tot *etr*  *exp* ///
							`u'_pre_tot_sca [w=_weight], by(ftile_sca)
							
						*interpolate missing fractiles 
						qui merge 1:1 ftile_sca using `tf109'
						qui sort ftile_sca
						qui ds ftile_sca _merge, not 
						foreach v in `r(varlist)' {
							qui mipolate `v' ftile_sca, gen(i_`v')
							qui mipolate i_`v' ftile_sca, gen(e_`v') forward 
							qui drop i_`v' `v' 
							qui rename e_`v' `v'
						}
						qui replace ftile_sca = ftile_sca / 100000
						qui gen n = _n
						
						*export results 
						qui drop _merge 
						
						//create main folders 
						local dirpath "output/efftax"
						mata: st_numscalar("exists", direxists(st_local("dirpath")))
						if (scalar(exists) == 0) {
							mkdir "`dirpath'"
							display "Created directory: `dirpath'"
						}
						
						*print one sheet by unit 
						qui keep ftile_sca `u'_* n
						quietly export excel using ///
							"output/efftax/efftax_summary_`u'.xlsx", ///
							firstrow(variables) sheet("`c'`y'") ///
							sheetreplace keepcellfmt	
							
							
						//create main folders 
						local dirpath "output/figures/eff_tax_rates_sca"
						mata: st_numscalar("exists", direxists(st_local("dirpath")))
						if (scalar(exists) == 0) {
							mkdir "`dirpath'"
							display "Created directory: `dirpath'"
						}	
						
						*stack areas 
						qui ds
						foreach bv in `r(varlist)' {
							qui replace `bv' = 0 if missing(`bv')
						}
						cap drop fp_*
						qui genstack `sckvs_`u'_`c'_`y'', gen(fp_)
						cap graph twoway  `gphas_`u'_`c'_`y'' ///
							(line `u'_etr_tot /*ftile_sca*/ n, lcolor(black)) ///
							(line `u'_mbr_tot /*ftile_sca*/ n, lcolor(black) ///
							lpattern(dash)) if ftile_sca >= .2 /*& `u'_etr_tot < 1*/ , ///
							${graph_scheme} ylabel(0(0.1).4, ${ylab_opts_white}) ///
							ylab(0 "0" .1 "10%" .2 "20%" .3 "30%" .4 "40%") /// 
							xlabel(/*0.2(0.2)1*/ ${xlab}, ${xlab_opts_white}) ///
							ytit("Effective tax rate on pretax inc.") xtit("") ///
							legend(off)	
						cap qui graph export ///
								"output/figures/eff_tax_rates_sca/`u'_efftax_`c'_`y'.pdf", replace	
							
						if "`c'`y'" == "COL2002" {
							graph twoway  `gphas_`u'_`c'_`y'' ///
							(line `u'_etr_tot /*ftile_sca*/ n, lcolor(black)) ///
							(line `u'_mbr_tot /*ftile_sca*/ n, lcolor(black) ///
							lpattern(dash)) if ftile_sca >= .2 , ///
							${graph_scheme} ylabel(0(0.1).5, ${ylab_opts_white}) ///
							xlabel(/*0.2(0.2)1*/ ${xlab}, ${xlab_opts_white}) ///
							ytit("Effective tax rate on pretax inc.") xtit("") ///
							legend(order(`lgd_`u'_`c'_`y'' `iter' "Effective tax rate" ///
							`iter2' "Monetary benefits")) 
							qui graph export ///
								"output/figures/eff_tax_rates_sca/legend_efftax.pdf", replace	
								
						}
					restore
				}
			}		
		}
	}
}


