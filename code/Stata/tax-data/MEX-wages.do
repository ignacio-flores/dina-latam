/*=============================================================================*
Code to organise administrative salaried worker data in Mexico                             
Authors: De Rosa, Flores, Morgan
Data: Feb/2020

The database contains the universe of salaried workers, their gross income,
exempt income and taxable income from employment and the number of months they
worked. 
*=============================================================================*/
clear
/////////////////////////////////////////////////////////////////////////////

//Import adult population data into tabulations
qui use "input_data/population/PopulationLatAm.dta", clear
mkmat year totalpop adultpop, matrix(_mat_sum)

scalar totalpop2009=_mat_sum[795, 2]
scalar totalpop2010=_mat_sum[796, 2]
scalar totalpop2011=_mat_sum[797, 2]
scalar totalpop2012=_mat_sum[798, 2]
scalar totalpop2013=_mat_sum[799, 2]
scalar totalpop2014=_mat_sum[800, 2]


// define the range of years you have
forvalues t = 2009/2014 { 
		
		di as result "working with Mexican admin data..." _continue
		
	
		// define location of tax data (one file per year)
		local taxfile "input_data/admin_data/MEX/microdata/Database_wages_`t'.dta" 
		cap use `taxfile', clear
		if _rc != 0 {
			di as error " loading failed"
		}
		
		// Define income variable = total gross income for months worked
		rename ibt tot_gross_inc
		gen gross_inc_month = tot_gross_inc / mes
		local y "tot_gross_inc"
		replace `y'=0 if `y'<=0
		
		tempvar weight ftile freq F fy cumfy L d_eq bckt_size cum_weight wy	
		//get total population 
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
		quietly egen `ftile' = cut(`F'), ///
			at(0.82(0.01)0.99 0.991(0.001)0.999 0.9991(0.0001)0.9999 0.99991(0.00001)0.99999 1)

		// Top average 
		gsort -`F'
		quietly gen `wy' = `y'*`weight'
		quietly gen topavg = sum(`wy')/sum(`weight')
		sort `F'
			
		// Interval thresholds
		quietly collapse (min)poptot (min) thr = `y' (mean) bckt_avg = `y' (min) ///
			topavg [w=`weight'], by (`ftile')
		sort `ftile'
		quietly gen ftile = `ftile'
		
		// Generate 127 percentiles from scratch
		tempfile collapsed_sum
		quietly save "`collapsed_sum'"
		clear
		quietly set obs 45
		quietly gen ftile = (82 + (_n - 1))/100 in 1/18
		quietly replace ftile = (99 + (_n - 18)/10)/100 in 19/27
		quietly replace ftile = (99.9 + (_n - 27)/100)/100 in 28/36
		quietly replace ftile = (99.99 + (_n - 36)/1000)/100 in 37/45
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
		
		// Total wages
		quietly gen totinc = `inc_tot' in 1
			
		// Order and save	
		rename bckt_avg bracketavg
		rename  poptot totalpop
		order year totinc average p thr bracketavg topavg topshare b totalpop
		keep year totinc average p thr bracketavg topavg topshare b	totalpop
		
		// Create directory if it doesnt exist 
		local dirpath "input_data/admin_data/MEX/_clean"
		mata: st_numscalar("exists", direxists(st_local("dirpath")))
		if (scalar(exists) == 0) {
			mkdir "`dirpath'"
			display "Created directory: `dirpath'"
		}	
		
		// export to excel (separate workbooks per year)
		export excel using ///
			"input_data/admin_data/MEX/_clean/wage_MEX_`t'.xlsx", ///
			firstrow(variables) keepcellfmt replace 
			
		// export to excel (separate workbooks per year)
		export excel using ///
			"input_data/admin_data/MEX/wage_MEX_`t'.xlsx", ///
			firstrow(variables) keepcellfmt replace 	
			
		di as text " done"	

}
