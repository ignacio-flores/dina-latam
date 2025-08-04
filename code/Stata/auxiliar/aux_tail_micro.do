		
tempvar 	weight poptot freq F ftile count aux countp
	cap drop income
	gen income = `income'
	tempfile	`country'_`income'_`year'	
			
	if data == "svy" {
		gen  `weight' = _fep
	}
	else {
		gen `weight' = 1 	
	}

	gsort -income
	gen `poptot' = `agg_pop'
	gen `freq' 	= `weight' / `poptot'
	gen `F' 		= 1-sum(`freq')
			
	// Classify obs in g-percentiles
	cap drop ftile
	gsort -`F'
	sort `F'
	egen ftile = cut(`F'), at(0.9(0.01)0.99 0.991(0.001)0.999 ///
		0.9991(0.0001)0.9999 0.99991(0.00001)0.99999 1)

	gen `aux' = round(ftile*100000)
	cap drop `countp'
	gen `countp' = 0
	local x = 0
	forvalues i = 0(1000)99000 {
		local x = `x' + 1
		replace `countp' = `x' if `aux' == `i'
	}

	local x = 100
	forvalues i = 99100(100)99900 {
		local x = `x' + 1
		replace `countp' = `x' if `aux' == `i'
	}

	local x = 109
	forvalues i = 99910(10)99990 {
		local x = `x' + 1
		replace `countp' = `x' if `aux' == `i'
	}

	local x = 118
	forvalues i = 99991(1)100000 {
		local x = `x' + 1
		replace `countp' = `x' if `aux' == `i'
	}
			
					
	// Interval thresholds and aggregate for later
	sum 	income	[fw = `weight']
	local 	mean_income		= r(mean)
	if "`country'" != "MEX" ///
		global  agg_inc_`country'_`year' ///
							= r(sum)

	*save info for later 
	*if "`country'" != "MEX" global agg_inc_`country'_`year' = `mean_income' * `agg_pop'
	drop if ftile==.
	sort ftile

	qui egen 	threshold_s	= min(income), by (ftile)
	qui gen 	topavg_s = 0
	forvalues cent=1/127 {
		qui sum 	income [fw = `weight'] if `cent' >= `countp'
		local 		tav = r(mean)
		qui replace	topavg_s = `tav'
	}

	qui gen 	s_function	= 1- `F'
	qui gen 	ln_s 		= ln(1/s_function)
	*qui sum 	income 	[fw = `weight']
	bysort 		ftile: gen `count' = _n 
	keep if 	`count' == 1
	qui gen		ln_inc_`year'	= ln(threshold_s / `mean_income')
	qui cap drop country
	qui gen 	country	= "`country'"
	qui gen 	ftile_r = ftile * 10000
	qui keep 	ln_inc_`year' ln_s country threshold_s topavg_s ftile ftile_r data
	keep if 	ln_inc_`year' > 0
	keep if 	ln_s  > 0
			
	qui save 	``country'_`income'_`year'' 


