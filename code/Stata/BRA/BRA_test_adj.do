/*=============================================================================*
Goal: Make corrections to Brazilian Survey
Author: Marc Morgan
Date: 	October/2019

The do file's goal is to make adjustments to the Brazilian surveys regarding the
classification of existing income variables and the imputation of missing income
variables.
*=============================================================================*/
set more off

local year "2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017"

		*------------------------------------------------------------------------------*
		* Step 0: assigning current MW, UI, INSS and DIRPF thresholds					
		*------------------------------------------------------------------------------*
			
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
		
		local minwage = `minwage`year'' 
		local avgUI = `avgUI`year''
		local maxlimitINSS = `maxlimitINSS`year''
		local exemptDIRPF = `exemptDIRPF`year''
		
		*---------------------------------------------------------------------------
		*Step 1: Moving financial incomes from non-classifiable to capital income
		*---------------------------------------------------------------------------
		
		if inrange(`year',2001,2015) {
			gen y_otn_svy = 0
			replace y_otn_svy 	= yotn_pe 								
			label var y_otn_svy "Non-classifiable (survey)"				
			gen y_fin_svy = 0										
			replace y_fin_svy = y_otn_svy if y_otn_svy >`minwage'	
			label var y_fin_svy "Financial incomes (survey)"									
			replace y_cap_svy = y_cap_svy + y_fin_svy
			replace y_kap_svy = y_kap_svy + y_fin_svy
			replace  y_otn_svy=y_otn_svy - y_fin_svy
		}
		//if inrange(`year',2016,2017) { // REVISAR!!!
		
		//}
		
		*---------------------------------------------------------------------------
		*Step 2: Imputing missing income variables
		*---------------------------------------------------------------------------
		
		*----- Unemployment insurance -----*
		
		if inrange(`year',2001,2015) {
			gen double y_unemp_svy = y_otn_svy
			replace y_unemp_svy = 0 if y_unemp_svy <=`minwage' | y_unemp_svy > `avgUI'*`minwage'
			replace y_otn_svy = y_otn_svy - y_unemp_svy
		}
		
		if inrange(`year',2016,2017) {
			gen double y_unemp_svy = y_oth_svy
			replace y_unemp_svy = 0 if y_unemp_svy <=`minwage' | y_unemp_svy > `avgUI'*`minwage'
			replace y_oth_svy = y_oth_svy - y_unemp_svy
		}
		
			
		*----- BPC/RMV -----*
		
		if inrange(`year',2001,2015) { 
			gen double y_bpc_svy = cond(inrange(y_otn_svy,.99*`minwage',1.01*`minwage'), y_otn_svy, 0) if y_otn_svy~=.
			replace y_otn_svy = y_otn_svy - y_bpc_svy
		}
		
		
		if inrange(`year',2016,2017) { 
			gen double y_bpc_svy = cond(inrange(y_oth_svy,.99*`minwage',1.01*`minwage'), y_oth_svy, 0) if y_oth_svy~=.
			replace y_oth_svy = y_oth_svy - y_bpc_svy
		}
		
			
		*---- Bolsa Familia -----*
		
		if inrange(`year',2001,2015) { 
			gen kid0to6 = (inrange(edad,0,6))
			gen kid7to15 = (inrange(edad,7,15))
			gen teen16to17 = (inrange(edad,16,17))
			foreach var of varlist kid0to6-teen16to17 { 
				egen n`var' = sum(`var'), by(id_hogar)
				drop `var'
			}
			gen nkid0to15 = nkid0to6 + nkid7to15
			gen double y_bf_svy = 0
			local iff "if y_otn_svy~=."
			if `year'==2001	replace y_bf_svy= cond(inrange(y_otn_svy,15,min(nkid7to15*15,45)),y_otn_svy,0) `iff'
			if `year'==2002	replace y_bf_svy= cond(inrange(y_otn_svy,7,min(15+min(nkid0to6*15,45)+min(nkid7to15*15,45),105)),y_otn_svy,0) `iff'
			if `year'==2003	replace y_bf_svy= cond(inrange(y_otn_svy,7,min(65+min(nkid0to6*15,45)+min(nkid7to15*15,45),155)),y_otn_svy,0) `iff'
			if `year'==2004 replace y_bf_svy= cond(inrange(y_otn_svy,7,min(65+min(nkid7to15*15,45)+min(nkid0to15*15,45),155)),y_otn_svy,0) `iff'
			if `year'==2005 replace y_bf_svy= cond(inrange(y_otn_svy,7,min(65+min(nkid7to15*15,45)+min(nkid0to15*15,45),155)),y_otn_svy,0) `iff'		
			if `year'==2006 replace y_bf_svy= cond(inrange(y_otn_svy,7,min(65+min(nkid0to15*15,45),110)),y_otn_svy,0) `iff'					
			if `year'==2007 replace y_bf_svy= cond(inrange(y_otn_svy,15,min(58+min(nkid0to15*18,54),115)), y_otn_svy, 0) `iff'						
			if `year'==2008 replace y_bf_svy= cond(inrange(y_otn_svy,20,min(62+min(nkid0to15*20,60)+min(nteen16to17*30,60),185)),y_otn_svy,0) `iff'					
			if `year'==2009 replace y_bf_svy= cond(inrange(y_otn_svy,20,min(68+min(nkid0to15*22,66)+min(nteen16to17*33,66),200)),y_otn_svy,0) `iff'	
			if `year'==2011 replace y_bf_svy= cond(inrange(y_otn_svy,30,min(70+min(nkid0to15*32,160)+min(nteen16to17*38,76),310)), y_otn_svy, 0) `iff'						
			if `year'==2012 { 
				replace y_bf_svy = cond(inrange(y_otn_svy,30,min(70+min(nkid0to15*32,160)+min(nteen16to17*38,76),310)),y_otn_svy,0) `iff'
				replace y_bf_svy = y_otn_svy if nkid0to6>0 & inrange(y_otn_svy,102,hh_size*70) 
			}
			if `year'==2013 { 
				replace y_bf_svy = cond(inrange(y_otn_svy,30,min(70+min(nkid0to15*32,160)+min(nteen16to17*38,76),310)),y_otn_svy,0) `iff'
				replace y_bf_svy = y_otn_svy if inrange(y_otn_svy,70,hh_size*70) 
			}
			if `year'==2014 { 
				replace y_bf_svy = cond(inrange(y_otn_svy,35,min(77+min(nkid0to15*35,175)+min(nteen16to17*42,84),340)),y_otn_svy,0) `iff'
				replace y_bf_svy = y_otn_svy if inrange(y_otn_svy,77,hh_size*77) 
			}
			if `year'==2015 { 
				replace y_bf_svy = cond(inrange(y_otn_svy,35,min(77+min(nkid0to15*35,175)+min(nteen16to17*42,84),340)),y_otn_svy,0) `iff'
				replace y_bf_svy = y_otn_svy if inrange(y_otn_svy,77,hh_size*77) 
			}
			replace y_otn_svy = y_otn_svy - y_bf_svy
			drop nkid* nteen*
		}
		
		if `year'==2016 {
			local iff "if y_oth_svy~=."
			gen double y_bf_svy = 0
			replace y_bf_svy = cond(inrange(y_oth_svy,38,min(82+min(nkid0to15*38,176)+min(nteen16to17*45,84),364)),y_oth_svy,0) `iff'
			replace y_bf_svy = y_oth_svy if inrange(y_oth_svy,82,hh_size*82)
			replace y_oth_svy = y_oth_svy - y_bf_svy
		}
		
		if `year'==2017 {
			local iff "if y_oth_svy~=."
			gen double y_bf_svy = 0
			replace y_bf_svy = cond(inrange(y_oth_svy,39,min(82+min(nkid0to15*39,170)+min(nteen16to17*46,85),372)),y_oth_svy,0) `iff'
			replace y_bf_svy = y_oth_svy if inrange(y_oth_svy,82,hh_size*82)
			replace y_oth_svy = y_oth_svy - y_bf_svy
		}
		
		
		*----- Abono salarial  -----*
		
		
		gen double y_abono_svy = cond((cotiza_ee==1) & sector_ee==2 & sys_pe<=2*`minwage' | y_mix_svy<=2*`minwage' & categ5_p~=3, `minwage', 0)
		
		*----- Holiday bonus + 13th salary -----*
		*= For the sake of consistency, secondary jobs are excluded
		
		gen y_hol_svy = cond((cotiza_ee==1), .33*sys_pe/12, 0) if categ5_p==2 | categ5_p==3
		//replace y_hol_svy = cond((cotiza_ee==1), .33*y_mix_svy_pe/12, 0) if categ5_p==1 | categ5_p==4
		gen double y_sys_13th = cond((cotiza_ee==1), sys_pe/12, 0) if categ5_p==2 | categ5_p==3
		//gen double y_mix_13th = cond((cotiza_ee==1), y_mix_pe/12, 0) if categ5_p==1 | categ5_p==4
		gen double y_pen_13th = y_pen_svy/12
		
		*----- Employee's labor income (includes both formal and informal; in-kind transfers not included) -----*
		
		gen double y_sys_svy = cond(categ5_p==2 & (cotiza_ee==1), sys_pe + y_hol_svy + y_sys_13th, 0)
		replace y_wag_svy = y_sys_svy + yoemp_pe 
		
		*-----  Employer's incomes -----*
		
		* Scenario A: capital withdrawals are assumed to be the excess payments over the minimum wage.
		* Scenario B: capital withdrawals are assumed to be the excess payments over the exemption limit on DIRPF.
		* Scenario C: capital withdrawals are assumed to be the excess payments over the maximum limit on contributing salary for INSS. 
			foreach sc in A B C { 
				if "`sc'"=="A" local threshold = `minwage'
				if "`sc'"=="B" local threshold = `exemptDIRPF'
				if "`sc'"=="C" local threshold = `maxlimitINSS'
				gen double ycapemployer1`sc'_svy = cond(categ5_p==1, max(sys_pe - `threshold',0), 0)
				gen double ylabemployer1`sc'_svy = cond(categ5_p==1, sys_pe - ycapemployer1`sc'_svy, 0)
				gen double ycapemployer`sc'_svy = ycapemployer1`sc'_svy
				gen double ylabemployer`sc'_svy = ylabemployer1`sc'_svy 
				gen double yemployer`sc'_svy = ycapemployer`sc'_svy + ylabemployer`sc'_svy			
			}
		
		*----- Pension income -----*
		
		replace y_pen_svy = y_pen_svy + y_pen_13th
		
		*---- Labour, capital, Benefit incoime and total individual income -----*
		
		foreach 1 in A B C { 
			gen double ylabour`1'_svy = y_wag_svy + y_pen_svy + y_oth_svy + y_otn_svy + y_unemp_svy + y_abono_svy + ylabemployer`1'_svy
			gen double ycapital`1'_svy = y_cap_svy + ycapemployer`1'_svy
		}
		
	
		replace y_ben_svy = y_pen_svy + y_unemp_svy + y_oth_svy
		
		gen y_assist_svy = y_bf_svy + y_bpc_svy + y_otn_svy
		gen y_assist_svy_y= y_assist_svy*12
		
		replace y_tot_svy = y_wag_svy + y_pen_svy + y_unemp_svy + y_abono_svy + y_oth_svy + y_otn_svy + y_cap_svy + y_mix_svy // monetary income befores taxes and transfers

	
	
