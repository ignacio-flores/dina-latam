/*=============================================================================*
Goal: Make corrections to Brazilian Survey
Author: Mauricio De Rosa, Ignacio Flores, Marc Morgan
Date: 	October/2019

The do file's goal is to make adjustments to the Brazilian surveys regarding the
classification of existing income variables and the imputation of missing income
variables.
*=============================================================================*/

global aux_part  ""preliminary"" 
quietly quietly do "code/Do-files/auxiliar/aux_general.do"  
local prefix "pre"

forvalues y = $first_y / $last_y {

	clear 
	quietly cap use "Data/CEPAL/surveys/BRA/raw/BRA_`y'_raw.dta", clear
	
	*Only run when data exists
	qui cap assert _N == 0
	if _rc != 0 {
	
		*------------------------------------------------------------------------------*
		* Step 1: assigning current MW, UI, INSS and DIRPF thresholds					
		*------------------------------------------------------------------------------*
		
		* For minwage check (valor Mensal): http://www.guiatrabalhista.com.br/guia/salario_minimo.htm
		local minwage1990 =	6056
		local minwage1992 =	522187
		local minwage1993 =	9606
		local minwage1995 =	100
		local minwage1996 =	112
		local minwage1997 =	120
		local minwage1998 =	130
		local minwage1999 =	136
		local minwage2001 = 180
		local minwage2002 = 200
		local minwage2003 = 240
		local minwage2004 = 260
		local minwage2005 = 300
		local minwage2006 = 350
		local minwage2007 = 380
		local minwage2008 = 415
		local minwage2009 = 465
		local minwage2010 = 510
		local minwage2011 = 545
		local minwage2012 = 622
		local minwage2013 = 678
		local minwage2014 = 724
		local minwage2015 = 788
		local minwage2016 = 880
		local minwage2017 = 937
		local minwage2018 = 954
		local minwage2019 = 998
		local minwage2020 = 1045
		local minwage2021 = 1100 
		local minwage2022 = 1212
		local minwage2023 = 1320 
		local minwage2024 = 1412
		
		*NOTE: avgUI does not need to be updated after 2015.
		local avgUI1990	= 1.75
		local avgUI1991	= 1.83
		local avgUI1992	= 1.69
		local avgUI1993	= 1.41
		local avgUI1994	= 1.55
		local avgUI1995	= 1.54
		local avgUI1996	= 1.56
		local avgUI1997	= 1.57
		local avgUI1998	= 1.56
		local avgUI1999	= 1.55
		local avgUI2000	= 1.51
		local avgUI2001	= 1.48
		local avgUI2002	= 1.42
		local avgUI2003	= 1.38
		local avgUI2004	= 1.39
		local avgUI2005	= 1.36
		local avgUI2006	= 1.31
		local avgUI2007	= 1.29
		local avgUI2008	= 1.28
		local avgUI2009	= 1.28
		local avgUI2010	= 1.26
		local avgUI2011	= 1.29
		local avgUI2012	= 1.28
		local avgUI2013	= 1.28
		local avgUI2014	= 1.30
		local avgUI2015	= 1.30
		local avgUI2016	= 1.29
		local avgUI2017	= 1.28
		local avgUI2018	= 1.28
		local avgUI2019	= 1.28
		local avgUI2020	= 1.28
		local avgUI2021	= 1.28
		local avgUI2022	= 1.28
		local avgUI2023	= 1.28
		local avgUI2024	= 1.28
		
		* For INSS top threshold check: https://www.gov.br/inss/pt-br/saiba-mais/seus-direitos-e-deveres/calculo-da-guia-da-previdencia-social-gps/tabela-de-contribuicao-mensal/tabela-de-contribuicao-historico
		* or https://www.gov.br/inss/pt-br/direitos-e-deveres/inscricao-e-contribuicao/tabela-de-contribuicao-mensal
		local maxlimitINSS1990	=	45288
		local maxlimitINSS1992	=	4780863
		local maxlimitINSS1993	=	86415
		local maxlimitINSS1995	=	833
		local maxlimitINSS1996	=	958
		local maxlimitINSS1997	=	1032
		local maxlimitINSS1998	=	1081
		local maxlimitINSS1999	=	1255
		local maxlimitINSS2001	=	1430
		local maxlimitINSS2002	=	1562
		local maxlimitINSS2003	=	1869
		local maxlimitINSS2004	=	2509
		local maxlimitINSS2005	=	2668
		local maxlimitINSS2006	=	2802
		local maxlimitINSS2007	=	2894
		local maxlimitINSS2008	=	3039
		local maxlimitINSS2009	=	3219
		local maxlimitINSS2011	=	3692
		local maxlimitINSS2012	=	3916
		local maxlimitINSS2013	=	4159
		local maxlimitINSS2014	=	4390
		local maxlimitINSS2015	=	4664
		local maxlimitINSS2016	=	5190
		local maxlimitINSS2017	=	5531
		local maxlimitINSS2018	=	5645
		local maxlimitINSS2019	=	5839
		local maxlimitINSS2020	=	6101
		local maxlimitINSS2021  =   6433
		local maxlimitINSS2022  =   7087
		local maxlimitINSS2023  =   7507
		local maxlimitINSS2024  =   7786
		
		* For DIRPF min threshold check: https://www.gov.br/receitafederal/pt-br/assuntos/orientacao-tributaria/tributos/irpf-imposto-de-renda-pessoa-fisica#calculo_mensal_IRPF
		local exemptDIRPF1990	=	27385
		local exemptDIRPF1992	=	3135620
		local exemptDIRPF1993	=	56480
		local exemptDIRPF1995	=	756
		local exemptDIRPF1996	=	900
		local exemptDIRPF1997	=	900
		local exemptDIRPF1998	=	900
		local exemptDIRPF1999	=	900
		local exemptDIRPF2000	=	900
		local exemptDIRPF2001	=	900
		local exemptDIRPF2002	=	1058
		local exemptDIRPF2003	=	1058
		local exemptDIRPF2004	=	1058
		local exemptDIRPF2005	=	1164
		local exemptDIRPF2006	=	1249
		local exemptDIRPF2007	=	1314
		local exemptDIRPF2008	=	1373
		local exemptDIRPF2009	=	1435
		local exemptDIRPF2011	=	1567
		local exemptDIRPF2012	=	1637
		local exemptDIRPF2013	=	1711
		local exemptDIRPF2014	=	1788
		local exemptDIRPF2015	=	1904
		local exemptDIRPF2016	=	1904
		local exemptDIRPF2017	=	1904
		local exemptDIRPF2018	=	1904
		local exemptDIRPF2019	=	1904
		local exemptDIRPF2020	=	1904
		local exemptDIRPF2021	=	1904
		local exemptDIRPF2022	=	1904
		local exemptDIRPF2023	=	2112
		local exemptDIRPF2024	=	2259
	
		local minwage = `minwage`y'' 
		local avgUI = `avgUI`y''
		local maxlimitINSS = `maxlimitINSS`y''
		local exemptDIRPF = `exemptDIRPF`y''
	
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

/*============THIS PART IS EXCLUDED DUE TO FORMALITY NOT IDENTIFIABLE AFTER 2015

		*----- Abono salarial  -----*
		
		qui cap drop `prefix'_abono_svy
		if `y' == 1990 qui gen double `prefix'_abono_svy = 0
		if inrange(`y',1992,2015) qui gen double `prefix'_abono_svy = cond(cotiza_ee==1 & sector_ee==2 & (sys_pe<=2*`minwage' | `prefix'_mix_svy<=2*`minwage') & categ5_p~=3, `minwage', 0)
		if inrange(`y',2016,$last_y) qui gen double `prefix'_abono_svy = cond(sector_ee==2 & (sys_pe<=2*`minwage' | `prefix'_mix_svy<=2*`minwage') & categ5_p~=3, `minwage', 0)
		
		qui la var `prefix'_abono_svy "Abono Salarial (imputed)"
		
		*----- Holiday bonus + 13th salary -----*

		*= For the sake of consistency, secondary jobs are excluded
		qui cap drop `prefix'_hol_svy 
		qui gen `prefix'_hol_svy = 0
		qui replace `prefix'_hol_svy = .33*sys_pe/12 if cotiza_ee==1 & (categ5_p==2 | categ5_p==3)
		//qui replace `prefix'_hol_svy = cond((cotiza_ee==1), .33*`prefix'_mix_sv`prefix'_pe/12, 0) if categ5_p==1 | categ5_p==4
		qui cap drop `prefix'_sys_13th
		qui gen `prefix'_sys_13th = 0
		qui replace `prefix'_sys_13th = sys_pe/12 if cotiza_ee==1 & (categ5_p==2 | categ5_p==3)
		//gen double `prefix'_mix_13th = cond((cotiza_ee==1), `prefix'_mix_pe/12, 0) if categ5_p==1 | categ5_p==4
		qui cap drop `prefix'_pen_13th 
		gen double `prefix'_pen_13th = `prefix'_pen_svy/12
		
		qui la var `prefix'_sys_13th "Thirteenth salary (imputed)"
		qui la var `prefix'_hol_svy "Holiday bonus (imputed)"

*/	
		
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
	
	qui cap save "Data/CEPAL/surveys/BRA/raw/BRA_`y'_raw.dta", replace

}
	
	
