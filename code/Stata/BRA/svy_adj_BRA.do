/*=============================================================================*
Goal: Make corrections to Brazilian Survey
Author: Mauricio De Rosa, Ignacio Flores, Marc Morgan
Date: 	October/2019

The do file's goal is to make adjustments to the Brazilian surveys regarding the
classification of existing income variables and the imputation of missing income
variables.
*=============================================================================*/

global aux_part  ""preliminary"" 
quietly quietly do "code/Stata/auxiliar/aux_general.do"  
quietly do "code/Stata/BRA/aux_bra_admin_thresholds.do"
local prefix "pre"

forvalues y = $first_y / $last_y {

	clear 
	quietly cap use "intermediary_data/microdata/raw/BRA/BRA_`y'_raw.dta", clear
	
	*Only run when data exists
	qui cap assert _N == 0
	if _rc != 0 {
	
		*------------------------------------------------------------------------------*
		* Step 1: assigning current MW, UI, INSS and DIRPF thresholds					
		*------------------------------------------------------------------------------*
		
		bra_admin_thresholds, year(`y') require("minwage avg_ui maxlimit_inss exempt_dirpf")
		local minwage = r(minwage)
		local avgUI = r(avg_ui)
		local maxlimitINSS = r(maxlimit_inss)
		local exemptDIRPF = r(exempt_dirpf)
	
		*---------------------------------------------------------------------------
		*Step 2: Imputing missing income variables
		*---------------------------------------------------------------------------
		
		*----- Financial incomes -----*		
		
		qui la var `prefix'_otn_svy "Non-classifiable (survey)"
		qui cap drop `prefix'_fin_svy	
		
		qui gen `prefix'_fin_svy 	= 0										
		qui replace `prefix'_fin_svy = `prefix'_otn_svy if `prefix'_otn_svy >`minwage'
		qui la var `prefix'_fin_svy "Financial incomes (survey)"									
		qui replace `prefix'_cap_svy = `prefix'_cap_svy + `prefix'_fin_svy
		qui replace `prefix'_kap_svy = `prefix'_kap_svy + `prefix'_fin_svy
		qui replace `prefix'_otn_svy = `prefix'_otn_svy - `prefix'_fin_svy
		
		*----- Unemployment insurance -----*
		
		qui cap drop `prefix'_unemp_svy
		if `y' == 1990 qui gen double `prefix'_unemp_svy = 0
		if inrange(`y',1992,2015) {
			qui gen double `prefix'_unemp_svy = `avgUI'* `minwage' if condact3==2 & afilia_ee==1
			qui replace `prefix'_unemp_svy = 0 if `prefix'_unemp_svy==.
			qui la var `prefix'_unemp_svy "Unemployment insurance (imputed)"
		}
		
		*----- BPC/RMV -----*
		qui cap drop `prefix'_bpc_svy 
		if inrange(`y',1990,1995) {
			gen double `prefix'_bpc_svy = cond(inrange(`prefix'_otn_svy,.49*`minwage',.51*`minwage'), `prefix'_otn_svy, 0) if `prefix'_otn_svy~=.
		}
		if inrange(`y',1996,2015) { 
			gen double `prefix'_bpc_svy = cond(inrange(`prefix'_otn_svy,.99*`minwage',1.01*`minwage'), `prefix'_otn_svy, 0) if `prefix'_otn_svy~=.
			qui replace `prefix'_otn_svy = `prefix'_otn_svy - `prefix'_bpc_svy
			qui la var `prefix'_bpc_svy "BPC transfer (imputed)"
		}
	
		*---- Bolsa Familia -----*
		qui cap drop `prefix'_bf_svy
		if inrange(`y',1990,1999) qui gen `prefix'_bf_svy = 0
		if inrange(`y',2001,2015) { 
			gen kid0to6 = (inrange(edad,0,6))
			gen kid7to15 = (inrange(edad,7,15))
			gen teen16to17 = (inrange(edad,16,17))
			foreach var of varlist kid0to6-teen16to17 { 
				qui egen n`var' = sum(`var'), by(id_hogar)
				drop `var'
			}
			gen nkid0to15 = nkid0to6 + nkid7to15
			gen double `prefix'_bf_svy = 0
			local iff "if `prefix'_otn_svy~=."
			if `y'==2001 qui replace `prefix'_bf_svy = ///
				cond(inrange(`prefix'_otn_svy,15,min(nkid7to15*15,45)),`prefix'_otn_svy,0) `iff'
			if `y'==2002 qui replace `prefix'_bf_svy = ///
				cond(inrange(`prefix'_otn_svy,7,min(15+min(nkid0to6*15,45)+min(nkid7to15*15,45),105)),`prefix'_otn_svy,0) `iff'
			if `y'==2003 qui replace `prefix'_bf_svy = /// 
				cond(inrange(`prefix'_otn_svy,7,min(65+min(nkid0to6*15,45)+min(nkid7to15*15,45),155)),`prefix'_otn_svy,0) `iff'
			if `y'==2004 qui replace `prefix'_bf_svy = /// 
				cond(inrange(`prefix'_otn_svy,7,min(65+min(nkid7to15*15,45)+min(nkid0to15*15,45),155)),`prefix'_otn_svy,0) `iff'
			if `y'==2005 qui replace `prefix'_bf_svy = /// 
				cond(inrange(`prefix'_otn_svy,7,min(65+min(nkid7to15*15,45)+min(nkid0to15*15,45),155)),`prefix'_otn_svy,0) `iff'		
			if `y'==2006 qui replace `prefix'_bf_svy = /// 
				cond(inrange(`prefix'_otn_svy,7,min(65+min(nkid0to15*15,45),110)),`prefix'_otn_svy,0) `iff'					
			if `y'==2007 qui replace `prefix'_bf_svy = /// 
				cond(inrange(`prefix'_otn_svy,15,min(58+min(nkid0to15*18,54),115)), `prefix'_otn_svy, 0) `iff'						
			if `y'==2008 qui replace `prefix'_bf_svy = /// 
				cond(inrange(`prefix'_otn_svy,20,min(62+min(nkid0to15*20,60)+min(nteen16to17*30,60),185)),`prefix'_otn_svy,0) `iff'					
			if `y'==2009 qui replace `prefix'_bf_svy = /// 
				cond(inrange(`prefix'_otn_svy,20,min(68+min(nkid0to15*22,66)+min(nteen16to17*33,66),200)),`prefix'_otn_svy,0) `iff'	
			if `y'==2011 qui replace `prefix'_bf_svy = /// 
				cond(inrange(`prefix'_otn_svy,30,min(70+min(nkid0to15*32,160)+min(nteen16to17*38,76),310)), `prefix'_otn_svy, 0) `iff'						
			if `y'==2012 { 
				qui replace `prefix'_bf_svy = cond(inrange(`prefix'_otn_svy,30,min(70+min(nkid0to15*32,160)+min(nteen16to17*38,76),310)),`prefix'_otn_svy,0) `iff'
				qui replace `prefix'_bf_svy = `prefix'_otn_svy if nkid0to6>0 & inrange(`prefix'_otn_svy,102,hh_size*70) 
			}
			if `y'==2013 { 
				qui replace `prefix'_bf_svy = cond(inrange(`prefix'_otn_svy,30,min(70+min(nkid0to15*32,160)+min(nteen16to17*38,76),310)),`prefix'_otn_svy,0) `iff'
				qui replace `prefix'_bf_svy = `prefix'_otn_svy if inrange(`prefix'_otn_svy,70,hh_size*70) 
			}
			if `y'==2014 { 
				qui replace `prefix'_bf_svy = cond(inrange(`prefix'_otn_svy,35,min(77+min(nkid0to15*35,175)+min(nteen16to17*42,84),340)),`prefix'_otn_svy,0) `iff'
				qui replace `prefix'_bf_svy = `prefix'_otn_svy if inrange(`prefix'_otn_svy,77,hh_size*77) 
			}
			if `y'==2015 { 
				qui replace `prefix'_bf_svy = cond(inrange(`prefix'_otn_svy,35,min(77+min(nkid0to15*35,175)+min(nteen16to17*42,84),340)),`prefix'_otn_svy,0) `iff'
				qui replace `prefix'_bf_svy = `prefix'_otn_svy if inrange(`prefix'_otn_svy,77,hh_size*77) 
			}
			qui replace `prefix'_otn_svy = `prefix'_otn_svy - `prefix'_bf_svy
			drop nkid* nteen*
		
			qui la var `prefix'_bf_svy "Bolsa Familia transfer (imputed)"
		}

		
		*-----  Employer's incomes -----*
		
		* Scenario A: capital withdrawals are assumed to be the excess payments over the minimum wage.
		* Scenario B: capital withdrawals are assumed to be the excess payments over the exemption limit on DIRPF.
		* Scenario C: capital withdrawals are assumed to be the excess payments over the maximum limit on contributing salary for INSS. 
			foreach sc in A B C { 
				if "`sc'"=="A" local threshold = `minwage'
				if "`sc'"=="B" local threshold = `exemptDIRPF'
				if "`sc'"=="C" local threshold = `maxlimitINSS'
				qui cap drop ycapemployer1`sc'_svy
				gen double ycapemployer1`sc'_svy = cond(categ5_p==1, max(sys_pe - `threshold',0), 0)
				qui cap drop ylabemployer1`sc'_svy
				gen double ylabemployer1`sc'_svy = cond(categ5_p==1, sys_pe - ycapemployer1`sc'_svy, 0)
				qui cap drop ycapemployer`sc'_svy
				gen double ycapemployer`sc'_svy = ylabemployer1`sc'_svy 
				qui cap drop ylabemployer`sc'_svy
				gen double ylabemployer`sc'_svy = ylabemployer1`sc'_svy 
				qui cap drop yemployer`sc'_svy
				gen double yemployer`sc'_svy = ycapemployer`sc'_svy + ylabemployer`sc'_svy	
				drop ylabemployer1`sc'_svy ylabemployer1`sc'_svy 
			}
			qui la var ycapemployerA_svy "Employer capital income, assumed to be the excess payments over the minimum wage"
			qui la var ycapemployerB_svy "Employer capital income, assumed to be the excess payments over the exemption limit on income tax"
			qui la var ycapemployerC_svy "Employer capital income, assumed to be the excess payments over the maximum limit on contributing salary for social security"
		
		*----- Pension income -----*
		
		qui replace `prefix'_pen_svy = `prefix'_pen_svy //+ `prefix'_pen_13th
		
		*---- Labour, capital, Benefit incoime and total individual income -----*
		
		if inrange(`y',1990,2015) {
			qui replace `prefix'_ben_svy = `prefix'_pen_svy + `prefix'_unemp_svy + `prefix'_oth_svy + `prefix'_bf_svy + `prefix'_bpc_svy + `prefix'_otn_svy //+ `prefix'_abono_svy
		}
		else {
			qui replace `prefix'_ben_svy = `prefix'_pen_svy + `prefix'_oth_svy + `prefix'_otn_svy //+ `prefix'_abono_svy
		}
		
		qui cap drop `prefix'_assist_svy	
		if inrange(`y', 1990, 2015) { 
			qui egen `prefix'_assist_svy = rowtotal(`prefix'_bf_svy `prefix'_bpc_svy `prefix'_otn_svy)
		}
		else {
			qui egen `prefix'_assist_svy = rowtotal(`prefix'_oth_svy `prefix'_otn_svy)
		}

		qui la var `prefix'_assist_svy "Social assistance transfers (survey)" 
		cap confirm variable `prefix'_assist, exact 
		if _rc == 0 cap drop `prefix'_assist
		qui gen `prefix'_assist= `prefix'_assist_svy * 12
		qui la var `prefix'_assist "Social assistance transfers (survey) - annual"
		
		if inrange(`y',1990,2015) { 
			qui replace `prefix'_wag_svy = `prefix'_wag_svy + `prefix'_unemp_svy //+ `prefix'_hol_svy/12 + `prefix'_sys_13th/12
			
		}
		
		else {
			qui replace `prefix'_wag_svy = `prefix'_wag_svy //+ `prefix'_hol_svy/12 + `prefix'_sys_13th/12
		}
		qui replace `prefix'_tot_svy = `prefix'_wag_svy + `prefix'_pen_svy + `prefix'_mix_svy + `prefix'_cap_svy  //+ `prefix'_abono_svy // monetary income befores taxes and transfers
		

	}
	
	qui cap save "intermediary_data/microdata/raw/BRA/BRA_`y'_raw.dta", replace

}
	
	
