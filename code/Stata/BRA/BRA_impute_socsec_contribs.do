/*=============================================================================*
Goal: imputation of Social Security contributions to CEPAL's PNAD data 
*=============================================================================*/

quietly do "code/Stata/BRA/aux_bra_admin_thresholds.do"

forvalues year = $first_y / $last_y {

	clear 
	qui cap use "intermediary_data/microdata/raw/BRA/BRA_`year'_raw.dta", clear

	*Only run when data exists
	qui cap assert _N == 0
	if _rc != 0 {

//forvalues year = 2001/2017 {
//if !inlist(`year', 2010) {

	//use "Data/CEPAL/surveys/BRA/raw/BRA_`year'_raw.dta", clear
	
		*-------------------------------------------------------------------*
		* Current MW and INSS thresholds									*
		*-------------------------------------------------------------------*
		bra_admin_thresholds, year(`year') require("minwage maxlimit_inss")
		local minwage = r(minwage)
		local maxlimitINSS = r(maxlimit_inss)
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
		
		qui save "intermediary_data/microdata/raw/BRA/BRA_`year'_raw.dta", replace	
		
		*-------------------------------------------------------------------*
		* Deduct from wage income and save												
		*-------------------------------------------------------------------*	
		
		qui cap drop rate_* 
		qui cap drop n_mw* 
		qui cap drop n_teto* 
		qui save "intermediary_data/microdata/raw/BRA/BRA_`year'_raw.dta", replace
		
	}
	else {
		di as error "intermediary_data/microdata/raw/BRA/BRA_`year'_raw.dta not found"
	}
	
}

