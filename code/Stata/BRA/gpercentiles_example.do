/*=============================================================================*
Codigo para crear el formato 127 g-percentiles de microdatos impositivos                             
Del proyecto De Rosa, Flores, Morgan (2020)
Instructions: replace X with your own locations

Income concept: should be the same income definition as that one in the national 
household survey (after tax and worker contributions income if this is disposable
income and before tax and contributions if it is gross/market income)
*=============================================================================*/

clear
set more off
set trace off
capture cd "X" // define directory
/////////////////////////////////////////////////////////////////////////////

// Define important locals
local weight "X" // individual weight
local y "X" // total income variable

forvalues t= 2000/2017 { // define the range of years you have

	local taxfile "./X_`t'.dta" // define location of tax data (one file per year)

	use `taxfile', clear

	tempvar ftile freq F fy cumfy L d_eq bckt_size cum_weight wy

	// Total average
	quietly sum `y' [w=`weight']
	local inc_avg = r(mean)	
		
	// Estimate Gini and keep in memory
	quietly sum	`weight', meanonly
	local poptot = r(sum)
	sort `y'
		
	quietly	gen `freq' = `weight'/`poptot'
	quietly	gen `F' = sum(`freq')	
	quietly	gen `fy'= `freq'*`y'
	quietly	gen `cumfy' = sum(`fy')
	
	quietly sum `cumfy', meanonly
	local cumfy_max = r(max)
	quietly	gen `L'= `cumfy'/`cumfy_max'
	quietly gen `d_eq' = (`F' - `L')*`weight'/`poptot'
	quietly sum	`d_eq', meanonly
	local d_eq_tot = r(sum)
	local gini = `d_eq_tot'*2
		
	// Classify obs in 127 g-percentiles
	quietly egen `ftile' = cut(`F'), at(0(0.01)0.99 0.991(0.001)0.999 0.9991(0.0001)0.9999 0.99991(0.00001)0.99999 1)
				
	// Top average 
	gsort -`F'
	quietly gen `wy' = `y'*`weight'
	quietly gen topavg = sum(`wy')/sum(`weight')
	sort `F'
		
	// Interval thresholds
	quietly collapse (min) thr = `y' (mean) bckt_avg = `y' (min) topavg [w=`weight'], by (`ftile')
	sort `ftile'
	quietly gen ftile = `ftile'
		
	// Generate 127 percentiles from scratch
	tempfile collapsed_sum
	quietly save "`collapsed_sum'"
	clear
	quietly set obs 127
	quietly gen ftile = (_n - 1)/100 in 1/100
	quietly replace ftile = (99 + (_n - 100)/10)/100 in 101/109
	quietly replace ftile = (99.9 + (_n - 109)/100)/100 in 110/118
	quietly replace ftile = (99.99 + (_n - 118)/1000)/100 in 119/127
	quietly merge n:1 ftile using "`collapsed_sum'"
		
	// Interpolate missing info
	quietly ipolate bckt_avg ftile, gen(bckt_avg2)      
	quietly ipolate thr ftile, gen(thr2)
	quietly ipolate topavg ftile, gen(topavg2)
		
	// Fill last cases if blank
	sort ftile
	drop bckt_avg thr topavg
	quietly rename bckt_avg2 bckt_avg
	quietly rename thr2 thr
	quietly rename topavg2 topavg
	quietly sum bckt_avg, meanonly
	quietly replace bckt_avg = r(max) if missing(bckt_avg)
	quietly sum thr, meanonly
	quietly replace thr = r(max) if missing(thr) 
	quietly sum topavg, meanonly
	quietly replace topavg = r(max) if missing(topavg)		
		
	// Top shares  
	quietly replace ftile = round(ftile, 0.00001)
	quietly gen topshare = (topavg/`inc_avg')*(1 - ftile)  	
		
	// Total average  
	quietly gen average = .
	quietly replace average = `inc_avg' in 1		
		
	// Inverted beta coefficient
	quietly gen b = topavg/thr		
		
	// Fractile
	quietly rename ftile p
		
	// Year
	quietly gen year = `k' in 1
	
	// Write Gini
	quietly gen gini = `gini' in 1
		
	// Order and save	
	order year gini average p thr bckt_avg topavg topshare b
	keep year gini average p thr bckt_avg topavg topshare b	
	
	export excel using "./X_`t'.xlsx", firstrow(variables) replace // export to excel (separate workbooks per year)

}		
