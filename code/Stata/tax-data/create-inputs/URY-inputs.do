clear all
set more off
set trace off

capture cd "D:/Dropbox (Desigualdad)/LATAM-WIL"
capture cd "C:/Users/Mauricio de Rosa/Dropbox (Desigualdad)/LATAM-WIL"
capture cd "~/Dropbox (Personal)/DINA-LatAm/"
capture cd "~/Dropbox/DINA-LatAm/"


global data "Data/Tax-data/URY"


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
	local 	new_pob 	=pob_`year' - _N
	local 	new = _N + `new_pob'
	set 	obs `new'

	foreach var in "pre" "pos" {
		preserve
		tempvar poptot agg_pop freq F  
		gsort -`var'_tot_inc
		
		
		gen `poptot' = pob_`year'
		gen `freq' 	= 1 / `poptot'
		gen `F' 		= 1-sum(`freq')
		
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
					
		export 	excel using "$data/gpinter_input/total-`var'-URY.xlsx" ///
				, sheet(20`year', modify) first(var) 
		export 	excel using "$data/gpinter_output/total-`var'-URY.xlsx" ///
				, sheet(20`year', modify) first(var) 
		restore
	}
}

