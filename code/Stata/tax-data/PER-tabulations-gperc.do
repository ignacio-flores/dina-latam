/*=============================================================================*
Goal: Import and prepare Peruvian tax data for combination with Survey
Authors: Mauricio De Rosa, Ignacio Flores, Marc Morgan
Date: 	Jan/2020

Totales de ingreso en soles
El ingreso total se compone en cinco categorias:
1a: rentas de arrendamiento de inmbuebles y predios
2a: rentas de capital (ganancias de capital, dividendos, intereses)
3a: rentas empresariales
4a: rentas de trabajo independiente 
5a: rentas de trabajo dependiente

NB: las rentas de 3a categoria no estan incluidas en las tabulaciones
*=============================================================================*/

//General----------------------------------------------------------------------- 
clear

forvalues year = 2016/2018 {
	
	//import excel file
	qui import excel ///
		"Data/Tax-data/PER/DeclaracionesPeru.xls", /// 
		sheet("PPNN_`year'") cellrange("C9:J25") clear
	
	//rename variables
	qui drop F
	qui rename (C D E G H I J) ///
		(thr hasta declar rent capital labour totalinc)
	qui drop hasta
		
	//gen variables of interest
	qui gen bracketavg = total/declar
	
	qui gen year=`year' in 1
	qui egen suminc=total(totalinc)
	qui egen totaldeclar=total(declar)
	qui gen average=(suminc)/totaldeclar
	
	foreach inc in "rent" "capital" "labour" {
		qui gen sh_`inc' =  `inc' / totalinc
	}
	
	//keep variables of interest
	qui order year average suminc declar thr ///
		bracketavg sh_rent sh_capital sh_labour
	qui keep year average suminc declar thr ///
		bracketavg sh_rent sh_capital sh_labour
	
	tempfile tab_`year'
	qui save `tab_`year'', replace
	
	cap use "Data/CEPAL/surveys/PER/raw/PER_`year'_raw.dta", clear
	
	cap assert _N == 0
	if _rc != 0 {
	
		qui sum _fep   
		local totalpop = r(sum)
	
	}	
	
	qui use "`tab_`year''" , clear
	
	tempvar freq cumfreq 
	
	//Obtaining population totals, frequencies and cumulative frequencies
	qui gen totalpop=`totalpop'
	qui gsort - bracketavg
	qui gen `freq'=declar/totalpop
	qui	gen `cumfreq' = sum(`freq')
	
	//percentiles
	qui gen p = 1 - `cumfreq'
	qui sort bracketavg
	qui sort p
	
	qui gen country="PER" in 1
	qui replace average = suminc / totalpop
	
	qui keep year country totalpop average p thr ///
		bracketavg sh_rent sh_capital sh_labour
	order year country totalpop average p thr bracketavg ///
		sh_rent sh_capital sh_labour
	
	cap export excel ///
	"Data/Tax-data/PER/gpinter_input/total-PER.xlsx", ///
		sheet("`year'", replace) firstrow(variables) keepcellfmt 
	
}
