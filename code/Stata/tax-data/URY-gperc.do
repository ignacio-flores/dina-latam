clear

di as result "crunching Uruguayan adminisitrative microdata..."
global data "input_data/admin_data/URY"

/*
scalar pob_09=2348300 
scalar pob_10=2370788 
scalar pob_11=2390888 
scalar pob_12=2410258 
scalar pob_13=2430379 
scalar pob_14=2451739 
scalar pob_15=2474284 
scalar pob_16=2497361 

*/
scalar pob_09=3378083 
scalar pob_10=3396706 
scalar pob_11=3412636 
scalar pob_12=3426466 
scalar pob_13=3440157 
scalar pob_14=3453691 
scalar pob_15=3467054 
scalar pob_16=3480222					
*/
qui foreach year in "09" "10" "11" "12" "13" "14" "15" "16" { //  

	di as text "year `year'..." _continue
	use "$data/microdata/Mega20`year'_paracuadros_alt3", clear  

	*-------------------------------------------------------------------------------
	*PART I: MAIN VARIABLES
	*-------------------------------------------------------------------------------
	
	qui do "code/Stata/auxiliar/aux_URY_incomevar"
	
	
	*Add indivs so that database accounts for the entire population
	local new_pob 	=pob_`year' - _N
	local new = _N + `new_pob'
	set obs `new'

	*Missings recoded as "0"
	replace tot_inc=0 if tot_inc==.

	*Shares
	gen sh_cap=cap_inc/tot_inc
	gen sh_lab=lab_inc/tot_inc
	gen sh_pen=pen_inc/tot_inc
	gen sh_mix=mix_inc/tot_inc

	*Sex
	gen male=0
	replace male=1 if sex==1
	label var male "Male"

	*Age groups
	gen age_1=0
	replace age_1=1 if age<40 & age!=.
	gen age_2=0
	replace age_2=1 if age>39 & age<60  & age!=.
	gen age_3=0
	replace age_3=1 if age>59  & age!=.
	gen age_4=0
	replace age_4=1 if age==. // important because there are many missings in top incomes


	tempvar poptot agg_pop freq F  
	gsort -tot_inc
	gen `poptot' = pob_`year'
	gen `freq' 	= 1 / `poptot'
	gen `F' 		= 1-sum(`freq')
	
	// Classify obs in g-percentiles
	cap drop ftile
	gsort -`F'
	sort `F'
	egen ftile = cut(`F'), at(0(0.01)0.99 0.991(0.001)0.999 ///
		0.9991(0.0001)0.9999 0.99991(0.00001)0.99999 1)

	*-------------------------------------------------------------------------------
	*PART II: OUTPUT MATRIX
	*-------------------------------------------------------------------------------

	*Main output matrix
	mat out_mat_`year'	=J(127,17,.)
	mat tax_`year'		=J(127,2,.)

	cap drop aux
	gen aux = round(ftile*100000)
	gen p = 0 
	replace p = aux/100000

	cap drop countp
	gen countp = 0
	local x = 0
	forvalues i = 0(1000)99000 {
		local x = `x' + 1
		replace countp = `x' if aux == `i'
	}

	local x = 100
	forvalues i = 99100(100)99900 {
		local x = `x' + 1
		replace countp = `x' if aux == `i'
	}

	local x = 109
	forvalues i = 99910(10)99990 {
		local x = `x' + 1
		replace countp = `x' if aux == `i'
	}

	local x = 118
	forvalues i = 99991(1)100000 {
		local x = `x' + 1
		replace countp = `x' if aux == `i'
	}
	
	


	local x=0
	forvalues cent=1/127 {
		local x=`x'+1
		
		sum 	tot_inc, d
		local   pob_tot=r(N)
		local   average=r(mean)
		mat out_mat_`year'[`x',16]=`pob_tot'
		mat out_mat_`year'[`x',17]=`average'
		
		sum 	tot_inc if countp==`cent', d
		local 	aver=r(mean)
		local 	thres=r(min) + `x' // to meke sure they are ascending if equal ()
		local   pob=r(N)
		mat out_mat_`year'[`x',1]=`pob'
		mat out_mat_`year'[`x',2]=`thres'
		mat out_mat_`year'[`x',3]=`aver'

		sum 	tot_inc if countp >= `cent', d
		mat out_mat_`year'[`x',14]=r(mean)

		sum 	p if countp == `cent', d
		mat out_mat_`year'[`x',15]=r(max)

		sum 	male if countp==`cent', d
		local	aver_male=r(mean)
		local 	aver_female=1-r(mean)
		mat out_mat_`year'[`x',4]=`aver_male'
		mat out_mat_`year'[`x',5]=`aver_female'
		
		sum 	e_tax_rate if countp==`cent', d
		local	tax_rate=r(mean)
		mat tax_`year'[`x',1]=`tax_rate'
		sum 	e_ss_rate if countp==`cent', d
		local	ss_rate=r(mean)
		mat tax_`year'[`x',2]=`ss_rate'

		local z=5
		foreach group in "age_1" "age_2" "age_3" "age_4" {
			local z=`z'+1
			sum 	`group' if countp==`cent', d
			local	aver_`group'=r(mean)
			mat out_mat_`year'[`x',`z']=`aver_`group''
		}


		local z=9
		foreach group in "lab_inc" "mix_inc" "pen_inc" "cap_inc" {
			local z=`z'+1
			sum 	`group' if countp==`cent', d
			local	sum_`group'=r(sum)
			sum 	tot_inc if countp==`cent', d
			local	sum_tot_inc=r(sum)
			local 	aver_`group'=`sum_`group''/`sum_tot_inc'
			mat out_mat_`year'[`x',`z']=`aver_`group''
		}

	}


	*export the matrix--------------------------------------------------------------
	mat colnames out_mat_`year'=N thr bracketavg male female _40 _60 _ Miss_age lab_inc mix_inc pen_inc cap_inc topavg p totalpop average
	
	// Create directory if it doesnt exist 
	local dirpath "input_data/admin_data/URY/_clean"
	mata: st_numscalar("exists", direxists(st_local("dirpath")))
	if (scalar(exists) == 0) {
		mkdir "`dirpath'"
		display "Created directory: `dirpath'"
	}	

	putexcel set "input_data/admin_data/URY/gpinter_URY_20`year'.xlsx", modify
	putexcel A1=matrix(out_mat_`year'), colnames
	
	putexcel set "input_data/admin_data/URY/_clean/total-pre-URY.xlsx", modify sheet(20`year')
	putexcel A1=matrix(out_mat_`year'), colnames

	// save effective tax rates data
	
	qui collapse (mean)tot_inc 	(mean)e_tax_rate 	///
		(mean)e_ss_rate	(mean)ftile, by(countp)			
	*qui rename countp 		p_merge
	qui gen p_merge = ftile 
	qui rename e_tax_rate 	eff_tax_rate_ipol 
	qui rename e_ss_rate 	eff_ss_rate_ipol 
	qui replace eff_tax_rate_ipol = 0 if eff_tax_rate_ipol >= 1
	qui replace eff_ss_rate_ipol  = 0 if eff_ss_rate_ipol >= 1
	qui mvencode eff_ss_rate_ipol eff_tax_rate_ipol, mv(0) override
	assert !missing(eff_tax_rate_ipol,eff_ss_rate_ipol)

	qui replace p_merge = p_merge*10000
	qui duplicates drop p_merge, force
	qui drop if p_merge > 9999
	qui format p_merge %9.0g
	
	// Create directory if it doesnt exist 
	local dirpath "input_data/admin_data/URY/eff-tax-rate"
	mata: st_numscalar("exists", direxists(st_local("dirpath")))
	if (scalar(exists) == 0) {
		mkdir "`dirpath'"
		display "Created directory: `dirpath'"
	}	

	save "input_data/admin_data/URY/eff-tax-rate/URY_effrates_20`year'", replace
	di as result " done"
*exit 1
}

