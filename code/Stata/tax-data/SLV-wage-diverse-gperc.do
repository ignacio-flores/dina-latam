/*=============================================================================*
Goal: Import and prepare Salvadorian tax data for combination with Survey
Totales de ingreso en dólares estadounidenses
*=============================================================================*/

//General----------------------------------------------------------------------- 
clear 

forvalues year = 2000/2017 {
	if !inlist(`year', 2008,2011) { 

	foreach var in "asal" "div" {

		if "`var'" == "asal" local cellr "B10:G19"
		else if "`var'" == "div" local cellr "B10:L19"
		
		//import excel file
		qui import excel ///
			"input_data/admin_data/SLV/Tabulaciones_SLV.xls", /// 
			sheet(`year') cellrange(`cellr') clear
		
		
		if "`var'" == "asal" {
		//rename variables
			qui drop F
			quietly rename (B C D E G) ///
				(tramo rangos contrib tot_renta impuesto)

			local inc "wages"
			
		}
		
		else if "`var'" == "div" {
		//rename variables
			qui drop D E F G H K
			quietly rename (B C I J L) ///
				(tramo rangos contrib tot_renta impuesto)

			local inc "total"
			
		}
			
		//reclassify intervals
		quietly gen thr = .
		quietly replace thr = 0 if tramo==1
		quietly replace thr = 2514 if tramo==2
		quietly replace thr = 5000 if tramo==3
		quietly replace thr = 15000 if tramo==4
		quietly replace thr = 30000 if tramo==5
		quietly replace thr = 60000 if tramo==6
		quietly replace thr = 120000 if tramo==7
		quietly replace thr = 150000 if tramo==8
		quietly replace thr = 500000 if tramo==9
		quietly replace thr = 1000000 if tramo==10
		
		//drop observations with no wage information
		qui drop if tramo==10 & inlist(`year',2002,2003,2004,2009,2012,2013,2014) /// 
		& "`var'"=="asal"
		qui drop if tramo==9 & inlist(`year',2011) & "`var'"=="asal"
		qui replace thr = 500000 if tramo==10 & inlist(`year',2011) & "`var'"=="asal"
		
		/*
		//convert dollars to LCU in 2000 (from 2001 SLV adopts the dollar)
		//divide values by 1000 (punctuation error in tabulations)
		foreach v in thr tot_renta impuesto {
			replace `v' = `v'*8.755 if `year'==2000
		}
		*/
		qui drop tramo
		//gen variables of interest
		quietly gen totn_renta = tot_renta - impuesto
		quietly gen bracketavg = totn_renta/contrib
		
		qui gen year=`year' in 1
		qui egen totalnetinc=total(totn_renta)
		qui egen totalcontrib=total(contrib)
		qui gen average=(totalnetinc)/totalcontrib
		
		//keep variables of interest
		qui order year average totalnetinc contrib thr bracketavg 
		qui keep year average totalnetinc contrib thr bracketavg
		
		tempfile tab_`year'_`var'
		quietly save "`tab_`year'_`var''", replace
		cap use "intermediary_data/microdata/raw/SLV/SLV_`year'_raw.dta", clear
		
		cap assert _N == 0
		if _rc != 0 {
		
			quietly sum _fep   
			local totalpop = r(sum)
		
		}		
		qui use "`tab_`year'_`var''" , clear
		
		tempvar freq cumfreq 
		
		//Obtaining population totals, frequencies and cumulative frequencies
		quietly gen totalpop=`totalpop'
		gsort - bracketavg
		quietly gen `freq'=contrib/totalpop
		quietly	gen `cumfreq' = sum(`freq')
		
		//percentiles
		quietly gen p = 1 - `cumfreq'
		qui sort bracketavg
		qui sort p
		
		qui gen country="SLV" in 1
		qui replace average = totalnetinc / totalpop
		
		qui order year country totalpop average p thr bracketavg
		if ("`var'"=="asal") qui keep year country totalpop average p bracketavg
		else qui keep year country totalpop average p thr bracketavg
		
		// Create directory if it doesnt exist 
		local dirpath "input_data/admin_data/SLV/_clean"
		mata: st_numscalar("exists", direxists(st_local("dirpath")))
		if (scalar(exists) == 0) {
			mkdir "`dirpath'"
			display "Created directory: `dirpath'"
		}	
		
		cap export excel ///
		"input_data/admin_data/SLV/_clean/`inc'-SLV.xlsx", ///
			sheet("`year'", replace) firstrow(variables) keepcellfmt 
	}
	}
}
