/*=============================================================================*
Goal: imputation of Social Security contributions to CEPAL's PNAD data 
Author:	Marc Morgan
Date: 	Oct/2019
*=============================================================================*/

forvalues year = $first_y / $last_y {

	clear 
	qui cap use "Data/CEPAL/surveys/BRA/raw/BRA_`year'_raw.dta", clear

	*Only run when data exists
	qui cap assert _N == 0
	if _rc != 0 {

//forvalues year = 2001/2017 {
//if !inlist(`year', 2010) {

	//use "Data/CEPAL/surveys/BRA/raw/BRA_`year'_raw.dta", clear
	
		*-------------------------------------------------------------------*
		* Current MW and INSS thresholds									*
		*-------------------------------------------------------------------*
		* For minwage check: http://www.guiatrabalhista.com.br/guia/salario_minimo.htm

		local	minwage1990	=	6056
		local	minwage1992	=	522187
		local	minwage1993	=	9606
		local	minwage1995	=	100
		local	minwage1996	=	112
		local	minwage1997	=	120
		local	minwage1998	=	130
		local	minwage1999	=	136
		local	minwage2001	=	180
		local	minwage2002	=	200
		local	minwage2003	=	240
		local	minwage2004	=	260
		local	minwage2005	=	300
		local	minwage2006	=	350
		local	minwage2007	=	380
		local	minwage2008	=	415
		local	minwage2009	=	465
		local	minwage2011	=	545
		local	minwage2012	=	622
		local	minwage2013	=	678
		local	minwage2014	=	724
		local	minwage2015	=	788
		local 	minwage2016 = 	880
		local 	minwage2017 = 	937
		local 	minwage2018 = 	954
		local 	minwage2019 = 	998
		local 	minwage2020 = 	1045
		local   minwage2021 = 	1100
		local   minwage2022 =   1212
		local 	minwage2023 = 	1320 
		local 	minwage2024 = 	1412
		
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
		
		local minwage = `minwage`year''
		local maxlimitINSS = `maxlimitINSS`year''
		//local alt_annualization = `alt_annualization`year''
		
		*-------------------------------------------------------------------*
		* Year by year adjustments											*
		*-------------------------------------------------------------------*	
		
		* Note: the imputations had to deal with two major obstacles -
		*	a) lack of pnad data: impossible to discriminate civil servants from formal private sector workers between 1978-1990
		*	b) lack of historical rates for military personnel and state/municipality civil servants
		* Thus, key simplifying assumptions were used: 
		*	a) all imputations for private sector workers assumed they were subject to the standard rates
		*	b) all workers were assumed to be subject to the same rates as private sector workers until 1990
		*	c) all public sector workers were assumed to be subject to the same rates as federal civil servants after 1992
		*	d) no special rates were applied to domestic employers
		*	e) all contributions tied to the minimum wage were assumed to be tied to the highest minimum wage in the country until 1984
		
		foreach z in "socsec_worker_contrib" "socsec_employer_contrib" ///
			"socsec_pensioner_contrib" "socsec_fgts_contrib" {
			cap drop `z'
			qui gen `z' = 0 			
		}
		
		qui cap drop n_mw
		qui cap drop n_teto
		qui cap drop rate_worker
		qui cap drop rate_emplyr
		qui gen n_mw = sys_pe/`minwage'
		qui gen n_teto = sys_pe / `maxlimitINSS'
		
		/*
		*= 1996 (L9032/1995)
		if inrange(`year',1996) { 
			forvalues i = 1/2 {	
				gen rate_worker`i' =  cond( n_teto`i' <= .3, .08, cond( n_teto`i' <= .5, .09, .10)) if emplstat`i'a==1
				replace rate_worker`i' = .105 * yjob`i' if emplstat`i'a==2
				replace rate_worker`i' = cond( n_teto`i' <= .3, .10, .20) if emplstat`i'a~=1 & emplstat`i'a~=2
				replace rate_worker`i' = rate_worker`i' * min(max(`minwage',yjob`i'),`maxlimitINSS') if emplstat`i'a~=2
				gen rate_emplyr`i' = cond( emplstat`i'a==1, .22 * max(`minwage',yjob`i'), 0) 	
			}
			replace socsec_worker_contrib = socsec_contrib_primary * rate_worker1 +	socsec_contrib_secondary * rate_worker2
			replace socsec_employer_contrib = socsec_contrib_primary * rate_emplyr1 + socsec_contrib_secondary * rate_emplyr2		
		}
		
		*= 1997-1998 (L9311/96, D2173/1997, L9630/1998)
		if inrange(`year',1997,1998) { 
			forvalues i = 1/2 { 
				gen rate_worker`i' = cond(n_teto`i'<=.3, .0782, cond(n_mw`i'<=3, .0882, cond(n_teto`i'<=.5, .09, .11))) if emplstat`i'a==1 // chg'ed rates
				replace rate_worker`i' = .11 * yjob`i' if emplstat`i'a==2
				replace rate_worker`i' = .20 if emplstat`i'a~=1 & emplstat`i'a~=2
				replace rate_worker`i' = rate_worker`i' * min(max(`minwage',yjob`i'),`maxlimitINSS') if emplstat`i'a~=2
				gen rate_emplyr`i' = cond( emplstat`i'a==1, .22 * max(`minwage',yjob`i'), 0) 	
			}
			replace socsec_worker_contrib = socsec_contrib_primary * rate_worker1 +	socsec_contrib_secondary * rate_worker2
			replace socsec_employer_contrib = socsec_contrib_primary * rate_emplyr1 + socsec_contrib_secondary * rate_emplyr2		
		}
		*/
		
		*= 1999-2007 (EC21/1999, P1987/2001, P288/2002, P727/2003, P479/2004, L10887/2004, P822/2005, P342/2006, P142/2007, D6042/2007)
		
		if inrange(`year',1999,2007) { 
			
			qui gen rate_worker = ///
				cond(n_teto<=.3, .0765, ///
				cond(n_mw<=3, .0865, ///
				cond(n_teto<=.5, .09, .11))) ///
				if (cotiza_ee==1) & sector_ee==2  // chg'ed rates
				
			qui replace rate_worker = .11 * sys_pe ///
				if (cotiza_ee==1) & sector_ee==1
				
			if `year'<=2006 {
				replace rate_worker = .20 if ///
					(cotiza_ee!=1 | cotiza_ee!=2) & ///
					(sector_ee!=1 | sector_ee!=2)
			} 
			if `year'==2007 {
				replace rate_worker = ///
					cond(n_mw <= 1, .11, .20) if ///
					(cotiza_ee!=1 | cotiza_ee!=2) & ///
					(sector_ee!=1 | sector_ee!=2)	// incl'ed PSPS
			} 
			qui replace rate_worker = ///
				rate_worker * min(max(`minwage',sys_pe),`maxlimitINSS') ///
				if (cotiza_ee==1) & sector_ee==1
			qui gen rate_emplyr= ///
				cond(cotiza_ee==1 & ///
				sector_ee==2, .22 * max(`minwage',sys_pe), 0) 	
			qui replace socsec_worker_contrib = cotiza_ee * rate_worker
			qui replace socsec_employer_contrib = cotiza_ee * rate_emplyr	
			if `year'>=2004 {
				qui replace socsec_pensioner_contrib ///
					= .11 * max(yjub_pe - `maxlimitINSS', 0)
			} 
		}
		
		*= 2008-2011 ( L10887/2004, PI77/2008)
		if inrange(`year',2008,2011) { 
			qui gen rate_worker = ///
				cond(n_teto <= .3, .08, cond(n_teto<= .5, .09, .11)) ///
				if (cotiza_ee==1) & sector_ee==2 // changed rates
			qui replace rate_worker = .11 * sys_pe ///
				if (cotiza_ee==1) & sector_ee==1
			replace rate_worker = cond( n_mw <= 1, .11, .20) if ///
				(cotiza_ee!=1 | cotiza_ee!=2) & (sector_ee!=1 | sector_ee!=2)
			qui replace rate_worker = ///
				rate_worker * min(max(`minwage',sys_pe),`maxlimitINSS') ///
				if (cotiza_ee==1) & sector_ee~=1
			qui gen rate_emplyr= ///
				cond(cotiza_ee==1 & sector_ee==1 & ///
				sector_ee==2, .22 * max(`minwage',sys_pe), 0) 	
			qui replace socsec_worker_contrib = cotiza_ee * rate_worker
			qui replace socsec_employer_contrib = cotiza_ee * rate_emplyr
			qui replace socsec_pensioner_contrib =  ///
				max( yjub_pe - `maxlimitINSS', 0) * .11
		}
		
		*= 2012-2017 (L12470/2011, PI MPS/MF 02/2012, PI15/2013, PI19/2014, PI13/2015, PI1/2016, PI8/2017)...extended to 2020
		if inrange(`year',2012,2020) {
			qui gen rate_worker = ///
				cond( n_teto <= .3, .08, cond( n_teto <= .5, .09, .11)) ///
				if (cotiza_ee==1) & sector_ee==2
			qui replace rate_worker = .11 * sys_pe if ///
				(cotiza_ee==1) & sector_ee==1
			replace rate_worker = cond( n_mw <= 1, .08, .20) ///
				if (cotiza_ee!=1 | cotiza_ee!=2) & ///
				(sector_ee!=1 | sector_ee!=2)	// incl'ed MEI
			qui replace rate_worker = ///
				rate_worker * min(max(`minwage',sys_pe),`maxlimitINSS') ///
				if (cotiza_ee==1) & sector_ee~=1
			qui gen rate_emplyr = ///
				cond(cotiza_ee==1 & sector_ee==1 & ///
				sector_ee==2, .22 * max(`minwage',sys_pe), 0) 	
			qui replace socsec_worker_contrib = cotiza_ee * rate_worker
			qui replace socsec_employer_contrib = cotiza_ee * rate_emplyr
			qui replace socsec_pensioner_contrib = ///
				max( yjub_pe - `maxlimitINSS', 0) * .11
			qui replace socsec_fgts_contrib = .08 * (sys_pe) if ///
				cotiza_ee==1 & sector_ee==2
		}
		
		*= Sum of contributions
		foreach z in socsec_tot_contribs_svy socsec_tot_contribs ///
			socsec_valid_contribs_svy socsec_valid_contribs {
			cap drop `z'
		}

		qui egen  socsec_tot_contribs_svy = rowtotal(socsec_*_contrib)
		qui label var socsec_tot_contribs_svy ///
			"Employer, employee & pensioner social contributions (imputed)"
		quietly gen socsec_tot_contribs = socsec_tot_contribs_svy*12
		qui label var socsec_tot_contribs ///
			"Employer, employee & pensioner social contributions (imputed) - annual" 
		qui egen  socsec_valid_contribs_svy = rowtotal(socsec_worker_contrib socsec_pensioner_contrib)
		qui label var socsec_valid_contribs_svy ///
			"Employee & pensioner social contributions (imputed)"
		quietly gen socsec_valid_contribs = socsec_valid_contribs_svy*12
		qui label var socsec_valid_contribs ///
			"Employee & pensioner social contributions (imputed) - annual"
		
		qui save "Data/CEPAL/surveys/BRA/raw/BRA_`year'_raw.dta", replace
		
		
		*-------------------------------------------------------------------*
		* Deduct from wage income and save												
		*-------------------------------------------------------------------*	
		
		qui cap drop rate_* 
		qui cap drop n_mw* 
		qui cap drop n_teto* 
		qui save "Data/CEPAL/surveys/BRA/raw/BRA_`year'_raw.dta", replace
		
	}
	
	
}


