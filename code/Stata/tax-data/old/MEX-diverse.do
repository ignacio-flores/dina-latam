/*=============================================================================*
Codigo para crear el formato 127 g-percentiles de microdatos impositivos                            
Caso: Mexico

Income concept: should be the same income definition as that one in the national 
household survey (after tax and worker contributions income if this is disposable
income and before tax and contributions if it is gross/market income)
*=============================================================================*/

clear
	global data "Data/Tax-data/MEX"
	global results "figures/eff_tax_rates"
	global codes "code/Do-files"

*capture cd "~/Dropbox/DINA-LatAm/Data/Tax-data/MEX/" // define directory
*capture cd "D:/Dropbox (Desigualdad)/LATAM-WIL/Data/Tax-data/MEX"


/////////////////////////////////////////////////////////////////////////////

forvalues t = 2009/2014 { // define the range of years you have

	if !inlist(`t',2009,2011,2013) {
		
		cap use "Data/CEPAL/surveys/MEX/raw/MEX_`t'_raw.dta", clear
			
			quietly sum _fep   
			local totalpop = r(sum)	
		
		local taxfile "$data/Database_taxfiles/Mexico`t'.dta" // define location of tax data (one file per year)

		use `taxfile', clear

		// Define income variable (post-tax pre-deduction income, except for deductions of expenses needed to incur income)
		// Post social contributions?
		/*
		gen net_inc = de_total_de_ing_acumulables - ((ar_ded_autorizadas + ot_ded_autorizadas)*0.5) - de_isr_causado
		*/
		qui include "$codes/auxiliar/auxiliar_MEX.do"
				
		local y "income_3"
		replace `y'=0 if `y'<=0

		rename 	de_isr_causado	tax
		gen eff_tax_rate = tax / `y'
		replace eff_tax_rate = 0 if eff_tax_rate == .
		replace eff_tax_rate = 0.3 if eff_tax_rate > 0.3

		tempvar weight ftile freq F fy cumfy L d_eq bckt_size cum_weight wy	
		// Estimate Gini and keep in memory
		gen `weight' = 1
		gen poptot = `totalpop'
		
		// Total average
		quietly sum `y'
		local inc_tot = r(sum)	
		local inc_avg = `inc_tot'/poptot
		gsort -`y'
		quietly	gen `freq' = `weight'/poptot
		quietly	gen `F' = 1- sum(`freq')
		sort `y'
		
		// Classify obs in g-percentiles
		quietly egen `ftile' = cut(`F'), at(0.98(0.01)0.99 0.991(0.001)0.999 0.9991(0.0001)0.9999 0.99991(0.00001)0.99999 1)

		// Top average 
		gsort -`F'
		quietly gen `wy' = `y'*`weight'
		quietly gen topavg = sum(`wy')/sum(`weight')
		sort `F'
			
		// Interval thresholds
		quietly collapse (mean) eff_tax_rate (min) poptot (min) thr = `y' (mean) bckt_avg = `y' (min) topavg [w=`weight'], by (`ftile')
		sort `ftile'
		quietly gen ftile = `ftile'
		
		// Generate 127 percentiles from scratch
		tempfile collapsed_sum
		quietly save "`collapsed_sum'"
		clear
		quietly set obs 29
		quietly gen ftile = (98 + (_n - 1))/100 in 1/2
		quietly replace ftile = (99 + (_n - 2)/10)/100 in 3/11
		quietly replace ftile = (99.9 + (_n - 11)/100)/100 in 12/20
		quietly replace ftile = (99.99 + (_n - 20)/1000)/100 in 21/29
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
		quietly gen year = `t' in 1
			
		// Order and save	
		rename bckt_avg bracketavg
		rename  poptot totalpop
		order year average p thr bracketavg topavg topshare b totalpop eff_tax_rate
		keep year average p thr bracketavg topavg topshare b totalpop eff_tax_rate
		
		export excel using "$data/gpinter_MEX_`t'.xlsx", firstrow(variables) keepcellfmt replace // export to excel (separate workbooks per year)

	}
		if !inlist(`t',2010,2012,2014) {
		
		
		//Import adult population data into tabulations
		cap use "Data/Population/PopulationLatAm.dta", clear

		mkmat year totalpop adultpop, matrix(_mat_sum)

		scalar totalpop2009=_mat_sum[795, 2]
		scalar totalpop2010=_mat_sum[796, 2]
		scalar totalpop2011=_mat_sum[797, 2]
		scalar totalpop2012=_mat_sum[798, 2]
		scalar totalpop2013=_mat_sum[799, 2]
		scalar totalpop2014=_mat_sum[800, 2]

		local taxfile "$data/Database_taxfiles/Mexico`t'.dta" // define location of tax data (one file per year)

		use `taxfile', clear
		// Define income variable (post-tax pre-deduction income, except for deductions of expenses needed to incur income)
		// Post social contributions?
		/*
		gen net_inc = de_total_de_ing_acumulables - ((ar_ded_autorizadas + ot_ded_autorizadas)*0.5) - de_isr_causado
		*/
		qui include "$codes/auxiliar/auxiliar_MEX.do"
				
		local y "income_3"
		replace `y'=0 if `y'<=0

		rename 	de_isr_causado	tax
			
		gen eff_tax_rate = tax / `y'
		replace eff_tax_rate = 0 if eff_tax_rate == .
		replace eff_tax_rate = 0.3 if eff_tax_rate > 0.3

		tempvar weight ftile freq F fy cumfy L d_eq bckt_size cum_weight wy	
		
		// Estimate Gini and keep in memory
		gen `weight' = 1
		gen poptot = totalpop`t'
		
		// Total average
		quietly sum `y'
		local inc_tot = r(sum)	
		local inc_avg = `inc_tot'/poptot
		gsort -`y'
		quietly	gen `freq' = `weight'/poptot
		quietly	gen `F' = 1- sum(`freq')
		sort `y'
		
		// Classify obs in g-percentiles
		quietly egen `ftile' = cut(`F'), at(0.98(0.01)0.99 0.991(0.001)0.999 0.9991(0.0001)0.9999 0.99991(0.00001)0.99999 1)

		// Top average 
		gsort -`F'
		quietly gen `wy' = `y'*`weight'
		quietly gen topavg = sum(`wy')/sum(`weight')
		sort `F'
			
		// Interval thresholds
		quietly collapse (mean) eff_tax_rate (min) poptot (min) thr = `y' (mean) bckt_avg = `y' (min) topavg [w=`weight'], by (`ftile')
		sort `ftile'
		quietly gen ftile = `ftile'
		
		// Generate 127 percentiles from scratch
		tempfile collapsed_sum
		quietly save "`collapsed_sum'"
		clear
		quietly set obs 29
		quietly gen ftile = (98+(_n - 1))/100 in 1/2
		quietly replace ftile = (99 + (_n - 2)/10)/100 in 3/11
		quietly replace ftile = (99.9 + (_n - 11)/100)/100 in 12/20
		quietly replace ftile = (99.99 + (_n - 20)/1000)/100 in 21/29
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
		quietly gen year = `t' in 1
			
		// Order and save	
		rename bckt_avg bracketavg
		rename  poptot totalpop
		order year average p thr bracketavg topavg topshare b totalpop eff_tax_rate
		keep year average p thr bracketavg topavg topshare b totalpop	eff_tax_rate 
		
		export excel using "$data/gpinter_MEX_`t'.xlsx", firstrow(variables) keepcellfmt replace // export to excel (separate workbooks per year)

	}
	
}	
	
