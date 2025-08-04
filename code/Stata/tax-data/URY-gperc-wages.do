clear all
global data "Data/Tax-data/URY"

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
quietly foreach year in "09" "10" "11" "12" "13" "14" "15" "16" { //  

	use "$data/Mega20`year'_paracuadros_alt3", clear  

	*-------------------------------------------------------------------------------
	*PART I: MAIN VARIABLES
	*-------------------------------------------------------------------------------

	qui do "code/Do-files/auxiliar/aux_URY_incomevar"

	*Add indivs so that database accounts for the entire population
	local new_pob 	=pob_`year' - _N
	local new = _N + `new_pob'
	set obs `new'

	*Missings recoded as "0"
	replace lab_inc=0 if lab_inc==.


	tempvar poptot agg_pop freq F  
	gsort -lab_inc
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
		
		sum 	lab_inc, d
		local   pob_tot=r(N)
		local   average=r(mean)
		mat out_mat_`year'[`x',16]=`pob_tot'
		mat out_mat_`year'[`x',17]=`average'
		
		sum 	lab_inc if countp==`cent', d
		local 	aver=r(mean)
		local 	thres=r(min) + `x' // to meke sure they are ascending if equal ()
		local   pob=r(N)
		mat out_mat_`year'[`x',1]=`pob'
		mat out_mat_`year'[`x',2]=`thres'
		mat out_mat_`year'[`x',3]=`aver'

		sum 	lab_inc if countp >= `cent', d
		mat out_mat_`year'[`x',14]=r(mean)

		sum 	p if countp == `cent', d
		mat out_mat_`year'[`x',15]=r(max)

	}


	*export the matrix--------------------------------------------------------------
	mat colnames out_mat_`year'=N thr bracketavg male female _40 _60 _ Miss_age lab_inc mix_inc pen_inc cap_inc topavg p totalpop average


	putexcel set "$data/wage_URY_20`year'.xlsx", modify
	putexcel A1=matrix(out_mat_`year'), colnames

	putexcel set "$data/gpinter_input/wage-pre-URY.xlsx", modify sheet(20`year')
	putexcel A1=matrix(out_mat_`year'), colnames

	putexcel set "$data/gpinter_otuput/wage-pre-URY.xlsx", modify sheet(20`year')
	putexcel A1=matrix(out_mat_`year'), colnames	


}

