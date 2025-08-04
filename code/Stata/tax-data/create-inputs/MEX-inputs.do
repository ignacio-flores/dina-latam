
	clear all
	*set tracedepth 1

	capture cd "D:/Desigualdad Dropbox/Mauricio De Rosa/LATAM-WIL"
	capture cd "C:/Users/Mauricio de Rosa/Dropbox (Desigualdad)/LATAM-WIL"
	capture cd "~/Dropbox (Personal)/DINA-LatAm/"
	capture cd "~/Dropbox/DINA-LatAm/"

	global data 	"Data/Tax-data/MEX"
	global results 	"figures/eff_tax_rates"
	global codes 	"code/Do-files"

	set matsize 1200


forvalues t = 2009/2014 { // 2009
	
	global year_aux = `t'
		
	use "Data/Population/PopulationLatAm.dta", clear
	mkmat year totalpop adultpop, matrix(_mat_sum)
	scalar totalpop2009=_mat_sum[795, 2]
	scalar totalpop2010=_mat_sum[796, 2]
	scalar totalpop2011=_mat_sum[797, 2]
	scalar totalpop2012=_mat_sum[798, 2]
	scalar totalpop2013=_mat_sum[799, 2]
	scalar totalpop2014=_mat_sum[800, 2]

	local taxfile "$data/Database_taxfiles/Mexico`t'.dta" // define location of tax data (one file per year)

	use `taxfile', clear
			
	qui do "$codes/auxiliar/auxiliar_MEX.do"
			
	qui keep 	de_total_de_ing_acumulables de_isr_causado	income*
		
	qui cap drop pre_tot_inc 
	qui gen pre_tot_inc = .
	qui replace 	pre_tot_inc = income_3  // "income" 
	qui rename 	de_isr_causado	tax
	qui cap drop pos_tot_inc
	qui gen pos_tot_inc = pre_tot_inc - tax
			
	foreach var in "pre" "pos" {					
	
	preserve
	tempvar poptot agg_pop freq F  
		gsort -`var'_tot_inc
		gen `poptot' = totalpop`t'
		gen `freq' 	= 1 / `poptot'
		gen `F' 	= 1-sum(`freq')
				
		// Classify obs in g-percentiles
		cap drop ftile
		gsort -`F'
		sort `F'
		egen ftile = cut(`F'), at(0(0.01)0.99 0.991(0.001)0.999 ///
			0.9991(0.0001)0.9999 0.99991(0.00001)0.99999 1)
						
			// Collapse by g-percentiles		
		qui gen present = !missing(`var'_tot_inc)
		qui sum `var'_tot_inc if present
		qui gen total_pob = r(N)
		qui gen average = r(mean)
		qui gen p 		= ftile
		qui gen p_merge	= p * 10000
		qui collapse (mean) average (min) thr=`var'_tot_inc			 	///
					(count) N=`var'_tot_inc (mean)total_pob 			///
					(mean) bracketavg = `var'_tot_inc (mean)p_merge		///
					if present, by(p) fast
					qui drop if missing(p)
							
		qui export 	excel using "$data/gpinter_input/total-`var'-MEX.xlsx" ///
				, sheet(`t', modify) first(var) 
		qui export 	excel using "$data/gpinter_output/total-`var'-MEX.xlsx" ///
				, sheet(`t', modify) first(var) 
	restore
	
	}
}
		
