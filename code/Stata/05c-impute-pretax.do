////////////////////////////////////////////////////////////////////////////////
//
// 							Title: IMPUTE MISSING PRETAX INCOMES
//	Production/consumption taxes, government income, social insurance balance
// 			 Authors: Mauricio DE ROSA, Ignacio FLORES, Marc MORGAN 
// 									Year: 2021
//
////////////////////////////////////////////////////////////////////////////////

clear all

//0. General settings ----------------------------------------------------------

//get list of paths 
global aux_part " "preliminary" " 
qui do "code/Stata/auxiliar/aux_general.do"
local lang $lang 
local interpolation "spline" //for data from ceq
local ds "oecd_gni" //select database gni gdp 
*local unit $unit 

//define macros 
if "${bfm_replace}" == "yes" local ext ""
if "${bfm_replace}" == "no" local ext "_norep"

//1. Fetch incomes from SNA/OECD/WID dataset -----------------------------------

tempfile tf_sna_pretax
global varli country year bpi_corp_gg prop_inc_net_gg ///
	tax_indg_oecd_gni gdp_wid TOT_B5g_wid GG_D61_R

//bring tax data 
qui use $varli using ///
	"intermediary_data/national_accounts/UNDATA-WID-OECD-Merged.dta", clear	

//extrapolate a bit if necessary 
qui replace tax_indg_oecd_gni = . if tax_indg_oecd_gni == 0 
qui sort country year 
by country: mipolate tax_indg_oecd_gni year, forward gen(auxvar1)
qui replace tax_indg_oecd_gni = auxvar1 if missing(tax_indg_oecd_gni)
qui drop auxvar1 
qui save `tf_sna_pretax', replace

//2. Impute incomes to surveys -------------------------------------------------

//2.0 Check existence of data and open

//loop over countries and years 
local z = 1 
global area " ${all_countries} "
foreach c in $area {
	forvalues y = $first_y/$last_y {
	
		if `z' == 1 {
			di as result "{hline 80}"
			display as result "imputing taxes and transfers ..."
			di as result "{hline 80}"
			local z = 0 
		}
		
		local survey ///
			"intermediary_data/microdata/bfm`ext'_pre/`c'_`y'_bfm`ext'_pre.dta"
		
		//confirm corrected survey exists 
		capture confirm file "`survey'"
		if _rc==0 {
			
			//exceptions
			if inlist("`c'`y'", "CRI2002", "CRI2003", "CRI2005") {
				continue
			}
			
			*inform activity
			di as text "`c' `y': " _continue
				
			//open it
			qui use "`survey'", clear
				
			//2.1 Impute taxes on production and consumption (% to factor inc)
			cap drop country 
			cap drop year 
			cap drop __*
			qui gen country = "`c'"
			qui gen year = `y'
			
			//merge with totals from OECD/UN
			qui merge m:1 country year using `tf_sna_pretax', ///
				nogen keep(3)
			
			//prepare tax aggregate for imputation 
			cap drop tax_indg_lcu
			qui gen tax_indg_lcu = tax_indg_`ds' * TOT_B5g_wid
			qui la var tax_indg_lcu "${lab_`t'}, Total, in current LCU"
			
			//calculate share in factor income distribution
			foreach u in "$unit_list" {
				if "`u'" != "act" {
					cap drop `u'_sh_pre_fac_sca
					qui sum `u'_pre_fac_sca [w = _weight], meanonly 
					qui gen `u'_sh_pre_fac_sca = `u'_pre_fac_sca / r(sum) 
					*gen check = sum(sh_pre_fac_sca * _weight)
					
					*impute 
					cap drop `u'_tax_indg_pre
					qui gen `u'_tax_indg_pre = tax_indg_lcu * `u'_sh_pre_fac_sca
					qui la var `u'_tax_indg_pre ///
						"Taxes on products and cons., imputed % to factor income"
				}
			}
			
			/*
			// 2.3 Impute NPI and U.Profits of the Gov (%factor inc)
			qui gen ind_prop_gg = prop_inc_net_gg * sh_pre_fac_sca
			qui gen ind_corp_gg = bpi_corp_gg * sh_pre_fac_sca

			//2.4 Impute social insurance balance (% wages + pensions)
			//calculate social insurance balance in LCU
			qui gen balance_si = GG_D61_R - ind_pre_pen_sca
			
			// calculate share in wages + pensions distribution
			qui gen ind_pre_wpen_sca = rowtotal(ind_pre_wag_sca ind_pre_pen_sca)
			qui sum ind_pre_wpen_sca [w = _weight], meanonly 
			qui gen sh_pre_wpen_sca = ind_pre_wpen_sca / r(sum) 
			qui replace sh_pre_wpen_sca = ind_pre_wpen_sca * 100
			
			//impute the balance proportional to wages + pensions
			qui gen ind_balance_si = balance si * sh_pre_wpen_sca
			*/
			
			//cover gaps 
			global exception_ctries inlist("`c'", "BOL") //"ECU" "CHL"
			foreach u in "$unit_list" {
				if "`u'" != "act" {
					
					//construct total income 
					if ${exception_ctries} {
						local lv ${`u'_upr_exep} `u'_tax_indg_pre
					}
					else {
						local lv ${`u'_upr_norm} `u'_tax_indg_pre
					} 
					cap drop `u'_totaux
					qui egen `u'_totaux = rowtotal(`lv')
					
					//compare aggregates 
					qui sum `u'_totaux [w=_weight], meanonly 
					local `u'_macrogap = r(sum) / TOT_B5g_wid
					if "`u'" == "ind" {
						di as text ///
							" macro gap " ///
							round((1-``u'_macrogap')*100, 0.1) "% " 	
					}	
				}	
			}
			
			*if macro gap is negative scale down proportionally 
			if `ind_macrogap' > 1 {
				local invmg = 1/`ind_macrogap'
				foreach u in "$unit_list" {
					if "`u'" != "act" {
						cap drop `u'_pre_lef
						qui gen `u'_pre_lef = 0 
						//get list of variables by unit  
						if ${exception_ctries} {
							local lv ${`u'_upr_exep} `u'_tax_indg_pre
						}
						else {
							local lv ${`u'_upr_norm} `u'_tax_indg_pre
						}
						//scale them down proportionally 
						foreach vab in `lv' {
							qui replace `vab' = `vab' * `invmg'
							di as result "vab `vab'; inv: `invmg'"
						}
						cap drop `u'_totaux
						qui egen `u'_totaux = rowtotal(`lv')
					}
				}
				*exit 1
			}
			
			//if macro gap is positive, add 'leftover income'
			if `ind_macrogap' <= 1 {
				cap drop macrogap_lcu 
				qui gen macrogap_lcu = (1 - `ind_macrogap') * TOT_B5g_wid
				foreach u in "$unit_list" {
					if "`u'" != "act" {
						qui sum `u'_totaux [w=_weight], meanonly 
						qui gen `u'_shtotaux = `u'_totaux / r(sum)
						*qui gen `u'_check = sum(`u'_shtotaux)
						cap drop `u'_pre_lef
						qui gen `u'_pre_lef = `u'_shtotaux * macrogap_lcu 
					}
				}
			} 
			
			*check consistency 
			foreach u in "$unit_list" {
				if "`u'" != "act" {
					cap drop `u'_totaux2
					qui egen `u'_totaux2 = rowtotal(`u'_totaux `u'_pre_lef)
					qui sum `u'_totaux2 [w=_weight], meanonly 
					local checker = r(sum)
					qui sum TOT_B5g_wid, meanonly 
					local b5g = r(min)
					if "`u'" != "pch" {
						*di as result "`u'"
						*di as text ", check ratio (`u'): " ///
						*	round(`checker' / `b5g' * 100, 0.001) "%"
						if !inlist("`c'`y'", "CRI2017") & !inlist("`c'", "URY") ///
							& ("`u'" != "esb") {
							cap assert inrange(round(`checker' / `b5g' * 100), 99, 101)
							if _rc != 0 {
								di as error "checker out of bounds: " ///
									round(`checker' / `b5g' * 100)
							}
						}
						if inlist("`c'", "URY") | ("`u'" == "esb") {
							*assert inrange(round(`checker' / `b5g' * 100), 96, 103)
						}
					}
				}
			}	
			
			//save 
			qui drop $varli 
			foreach zzz in totaux totaux2 shtotaux {
				cap drop *_`zzz'
			}
			
			//save 
			qui save "`survey'", replace	
		}	
	}
}

