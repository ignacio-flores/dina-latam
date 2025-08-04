/*=============================================================================*
Goal: Import and prepare Salvadorian tax data for combination with Survey
Authors: Mauricio De Rosa, Ignacio Flores, Marc Morgan
Date: 	Jan/2020

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
		import excel ///
			"Data/Tax-data/SLV/Tabulaciones_SLV.xls", /// 
			sheet(`year') cellrange(`cellr') clear
		
		
		if "`var'" == "asal" {
		//rename variables
			drop F
			quietly rename (B C D E G) ///
				(tramo rangos contrib tot_renta impuesto)

			local inc "wages"
			
		}
		
		else if "`var'" == "div" {
		//rename variables
			drop D E F G H K
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
		drop if tramo==10 & inlist(`year',2002,2003,2004,2009,2012,2013,2014) /// 
		& "`var'"=="asal"
		drop if tramo==9 & inlist(`year',2011) & "`var'"=="asal"
		qui replace thr = 500000 if tramo==10 & inlist(`year',2011) & "`var'"=="asal"
		
		/*
		//convert dollars to LCU in 2000 (from 2001 SLV adopts the dollar)
		//divide values by 1000 (punctuation error in tabulations)
		foreach v in thr tot_renta impuesto {
			replace `v' = `v'*8.755 if `year'==2000
		}
		*/
		drop tramo
		//gen variables of interest
		quietly gen totn_renta = tot_renta - impuesto
		quietly gen bracketavg = totn_renta/contrib
		
		gen year=`year' in 1
		egen totalnetinc=total(totn_renta)
		egen totalcontrib=total(contrib)
		gen average=(totalnetinc)/totalcontrib
		
		//keep variables of interest
		order year average totalnetinc contrib thr bracketavg 
		keep year average totalnetinc contrib thr bracketavg
		
		tempfile tab_`year'_`var'
		quietly save "`tab_`year'_`var''", replace
		
		cap use "Data/CEPAL/surveys/SLV/raw/SLV_`year'_raw.dta", clear
		
		cap assert _N == 0
		if _rc != 0 {
		
			quietly sum _fep   
			local totalpop = r(sum)
		
		}	
		
		use "`tab_`year'_`var''" , clear
		
		tempvar freq cumfreq 
		
		//Obtaining population totals, frequencies and cumulative frequencies
		quietly gen totalpop=`totalpop'
		gsort - bracketavg
		quietly gen `freq'=contrib/totalpop
		quietly	gen `cumfreq' = sum(`freq')
		
		//percentiles
		quietly gen p = 1 - `cumfreq'
		sort bracketavg
		sort p
		
		gen country="SLV" in 1
		replace average = totalnetinc / totalpop
		
		order year country totalpop average p thr bracketavg
		if ("`var'"=="asal") keep year country totalpop average p bracketavg
		else keep year country totalpop average p thr bracketavg
		
		cap export excel ///
		"Data/Tax-data/SLV/gpinter_input/`inc'-SLV.xlsx", ///
			sheet("`year'", replace) firstrow(variables) keepcellfmt 
	}
	}
}
