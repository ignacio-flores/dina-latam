/*=============================================================================*
Goal: Import and clean Argentinian tax data for combination with Survey
Authors: Mauricio De Rosa, Ignacio Flores, Marc Morgan
Date: 	December/2019

Tablas: 2.2.2.1.1 = incomes | 2.2.2.1.2 = deductions | 2.2.2.1.11 = tax
Ingresos: 1cat = renta del suelo | 2cat = renta de capitales | 3cat = ingreso
mixto (beneficios de empresas + renta comercial) | 4cat = renta de trabajo

Totales de ingreso en miles de pesos corrientes
*=============================================================================*/

//General----------------------------------------------------------------------- 
clear all

forvalues year = 2001/2002 {

	foreach tab in "3.2.3.2.1" "3.2.3.2.2" {
	
		//define range
		if ("`tab'" == "3.2.3.2.1") & `year'==2001 local cellr "B17:L34" 
		else if ("`tab'" == "3.2.3.2.2") & `year'==2001 local cellr "B17:P34" 
		if ("`tab'" == "3.2.3.2.1") & `year'==2002 local cellr "B15:P21"
		else if ("`tab'" == "3.2.3.2.2") & `year'==2002 local cellr "B15:Q21"
		
		//import
		import excel ///
			"Data/Tax-data/ARG/EstadisticasTributarias`year'/AFIP/`tab'.xls", /// 
				cellrange(`cellr') clear
			
		if ("`tab'" == "3.2.3.2.1" & `year'==2001)  { 	
			//Change names	
			drop G-J
			quietly rename (B C D E F K L) ///
				(desde hasta declarantes totinc tot_1_4cat importe_part_emp_qb ///
				importeded_3cat)
			drop hasta
		}	
		else if ("`tab'" == "3.2.3.2.2" & `year'==2001) { 	
			//Change names	
			drop E-O
			quietly rename (B C D P) ///
				(desde hasta declarantes impuesto)
			drop hasta
		}		

		if ("`tab'" == "3.2.3.2.1" & `year'==2002) { 	
			//Change names	
			quietly rename (B C D E F G H I J K L M N O P) ///
				(desde hasta declarantes totinc tot_1_4cat ///
				casos_1cat importe_1cat casos_2cat importe_2cat ///
				casos_3cat importe_3cat casos_4cat importe_4cat ///
				casos_part_emp importe_part_emp)
			drop hasta
		}
		
		else if ("`tab'" == "3.2.3.2.2" & `year'==2002) { 	
			//Change names
			quietly rename (B C D E F G H I J K L M N O P Q) ///
			(desde hasta declarantes_ing declarantes_ded totded totded_1_4cat ///
			casosded_1cat importeded_1cat casosded_2cat importeded_2cat ///
			casosded_3cat importeded_3cat casosded_4cat importeded_4cat ///
			casos_part_emp_qb importe_part_emp_qb)		
			drop hasta
		}	

		quietly save ///
		"Data/Tax-data/ARG/EstadisticasTributarias`year'/`tab'.dta", ///
		replace 
	}
}

import excel ///
"Data/Tax-data/ARG/EstadisticasTributarias2002/AFIP/3.2.3.2.10.xls", /// 
cellrange(B16:O22) clear
	
//Change names	
drop E-N
quietly rename (B C D O) (desde hasta declarantes impuesto)
drop hasta

quietly save ///
"Data/Tax-data/ARG/EstadisticasTributarias2002/3.2.3.2.10.dta", replace 


local time "2003 2004 2005 2006 2007 2008 2015 2016 2017 2018 2019 2020"

foreach year in `time' {
	foreach tab in "2.2.2.1.1" "2.2.2.1.2" "2.2.2.1.11" {

		//define range
		if ("`tab'" == "2.2.2.1.1") & inrange(`year',2003,2006) {
			local cellr "B16:P33" 
		} 
		else if ("`tab'" == "2.2.2.1.2") & inrange(`year',2003,2006) {
			local cellr "B17:Q34"
		}  
		else if inrange(`year',2003,2006) local cellr "B17:H34"
		if ("`tab'" == "2.2.2.1.1") & inrange(`year',2007,2018) {
			local cellr "B14:P31"
		} 
		else if ("`tab'" == "2.2.2.1.2") & inrange(`year',2007,2018) {
			local cellr "B15:Q32"
		} 
		else if inrange(`year',2007,2019) {
			local cellr "B15:H32"
		} 
		if ("`tab'" == "2.2.2.1.1") & `year'== 2019 local cellr "B13:P30"
		else if ("`tab'" == "2.2.2.1.2") & `year'== 2019 local cellr "B14:Q31"
		if ("`tab'" == "2.2.2.1.1") & `year'== 2020 local cellr "B14:R43"
		else if ("`tab'" == "2.2.2.1.2") & `year'== 2020 local cellr "B15:S44"
		else if `year'== 2020 local cellr "B15:H44"
		
		//import
		import excel ///
			"Data/Tax-data/ARG/EstadisticasTributarias`year'/AFIP/`tab'.xls", /// 
				cellrange(`cellr') clear
			
		if ("`tab'" == "2.2.2.1.1") & inrange(`year',2003,2019) { 	
			//Change names	
			quietly rename (B C D E F G H I J K L M N O P) ///
				(desde hasta declarantes totinc tot_1_4cat ///
				casos_1cat importe_1cat casos_2cat importe_2cat ///
				casos_3cat importe_3cat casos_4cat importe_4cat ///
				casos_part_emp importe_part_emp)
			cap drop Q 
			drop hasta
		}
		else if ("`tab'" == "2.2.2.1.1") & `year'==2020 { 	
			//Change names	
			quietly rename (B C D E F G H I J K L M N O P Q R) ///
				(desde hasta declarantes totinc tot_1_4cat ///
				casos_1cat importe_1cat casos_2cat importe_2cat ///
				casos_3cat importe_3cat casos_4cat importe_4cat ///
				casos_div importe_div casos_part_emp importe_part_emp)
			cap drop Q 
			drop hasta
		}
		
		else if ("`tab'" == "2.2.2.1.2") & inrange(`year',2003,2019) { 	
			//Change names
			quietly rename (B C D E F G H I J K L M N O P Q) ///
			(desde hasta declarantes_ing declarantes_ded totded totded_1_4cat ///
			casosded_1cat importeded_1cat casosded_2cat importeded_2cat ///
			casosded_3cat importeded_3cat casosded_4cat importeded_4cat ///
			casos_part_emp_qb importe_part_emp_qb)		
			drop hasta
		}
	
		else if ("`tab'" == "2.2.2.1.2") & `year'==2020 { 	
			//Change names
			quietly rename (B C D E F G H I J K L M N O P Q R S) ///
			(desde hasta declarantes_ing declarantes_ded totded totded_1_4cat ///
			casosded_1cat importeded_1cat casosded_2cat importeded_2cat ///
			casosded_3cat importeded_3cat casosded_4cat importeded_4cat ///
			casos_div importe_div casos_part_emp_qb importe_part_emp_qb)		
			drop hasta
		}
		
		else {
			quietly rename (B C D E F G H) ///
				(desde hasta declarantes_tax totinc_tax casos_gan ///
				importe_gan impuesto)
			drop hasta
		}
		/*
		ds
		local vars "`r(varlist)'"
		foreach v in `vars' {
			quietly replace `v' = subinstr(`v', ".", "",.) 
			destring `v', replace
		}
		*/
		quietly save ///
			"Data/Tax-data/ARG/EstadisticasTributarias`year'/`tab'.dta", ///
			replace 
		
	}
		
}

local iter = 1 
local years "2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020"
local time "2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019"

foreach year in `years' {

	local t: word `iter' of `time'
//forvalues year = 2001/2018 {

	if `year'==2001 {
	use "Data/Tax-data/ARG/EstadisticasTributarias`year'/3.2.3.2.1.dta", ///
	clear
	
	// merge tabulations
	merge m:m desde using "Data/Tax-data/ARG/EstadisticasTributarias`year'/3.2.3.2.2.dta", nogenerate
	
	// change names and keep variables of interest
	rename (desde totinc) (thr total)
	gen year=`t' in 1
	gen total_net = total - importeded_3cat - importe_part_emp_qb - impuesto
	egen totalnetinc=total(total_net)
	replace totalnetinc=totalnetinc*1000
	egen totalpop=total(declarantes)
	gen average=(totalnetinc)/totalpop
	gen bracketavg = (total_net*1000)/declarantes
	
	order year average totalnetinc declarantes thr bracketavg 
	keep year average totalnetinc declarantes thr bracketavg
	
	}
	
	if `year'==2002 {
	use "Data/Tax-data/ARG/EstadisticasTributarias`year'/3.2.3.2.1.dta", ///
	clear
	
	// merge tabulations
	merge m:m desde using "Data/Tax-data/ARG/EstadisticasTributarias`year'/3.2.3.2.2.dta", nogenerate
	merge m:m desde using "Data/Tax-data/ARG/EstadisticasTributarias`year'/3.2.3.2.10.dta", nogenerate

	// change names and keep variables of interest
	rename (desde totinc) (thr total)
	gen year=`t' in 1
	gen total_net = total - importeded_3cat - importe_part_emp_qb - impuesto
	egen totalnetinc=total(total_net)
	replace totalnetinc=totalnetinc*1000
	egen totalpop=total(declarantes)
	gen average=(totalnetinc)/totalpop
	gen bracketavg = (total_net*1000)/declarantes
	
	order year average totalnetinc declarantes thr bracketavg 
	keep year average totalnetinc declarantes thr bracketavg
	
	
	}
	
	if inrange(`year',2003,2020) {
	use "Data/Tax-data/ARG/EstadisticasTributarias`year'/2.2.2.1.1.dta", ///
		clear
		
	// merge tabulations
	merge m:m desde using "Data/Tax-data/ARG/EstadisticasTributarias`year'/2.2.2.1.2.dta", nogenerate
	merge m:m desde using "Data/Tax-data/ARG/EstadisticasTributarias`year'/2.2.2.1.11.dta", nogenerate
	
	// change names and keep variables of interest
	rename (desde totinc importe_1cat importe_2cat ///
		importe_3cat importe_4cat importe_part_emp) ///
		(thr total rent capital mixed labour business)
	gen year=`t' in 1
	keep year thr declarantes total rent capital mixed labour business ///
		importeded_3cat importe_part_emp_qb impuesto
	gen total_net = total - importeded_3cat - importe_part_emp_qb - impuesto
	egen totalnetinc=total(total_net)
	egen totalpop=total(declarantes)
	
	if inrange(`year',2003,2012) {
		replace totalnetinc=totalnetinc*1000
		gen average=(totalnetinc)/totalpop
		gen bracketavg = (total_net*1000)/declarantes 
	}
	
	if inrange(`year',2013,2020) {
		replace totalnetinc=totalnetinc*1000000
		gen average=(totalnetinc)/totalpop
		gen bracketavg = (total_net*1000000)/declarantes 
	}
	
	foreach inc in "rent" "capital" "mixed" "labour" "business" {
		gen sh_`inc' =  `inc' / total
	}
	
	order year average totalnetinc declarantes thr bracketavg sh_rent sh_capital sh_mixed sh_labour sh_business
	keep year average totalnetinc declarantes thr bracketavg sh_rent sh_capital sh_mixed sh_labour sh_business

}	

	local iter = `iter' + 1
	
	quietly save ///
	"Data/Tax-data/ARG/EstadisticasTributarias`year'/tabulation_`t'.dta", ///
	replace 

	
}	

local iter = 1 
local years "2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020"
local time "2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019"

//Import total population data into tabulations
use "Data/Population/PopulationLatAm.dta", clear
mkmat year totalpop adultpop, matrix(_mat_sum)

scalar totalpop2000=_mat_sum[42, 2]
scalar totalpop2001=_mat_sum[43, 2]
scalar totalpop2002=_mat_sum[44, 2]	
scalar totalpop2003=_mat_sum[45, 2]
scalar totalpop2004=_mat_sum[46, 2]
scalar totalpop2005=_mat_sum[47, 2]
scalar totalpop2006=_mat_sum[48, 2]
scalar totalpop2007=_mat_sum[49, 2]
scalar totalpop2008=_mat_sum[50, 2]
scalar totalpop2009=_mat_sum[51, 2]
scalar totalpop2010=_mat_sum[52, 2]
scalar totalpop2011=_mat_sum[53, 2]
scalar totalpop2012=_mat_sum[54, 2]
scalar totalpop2013=_mat_sum[55, 2]
scalar totalpop2014=_mat_sum[56, 2]
scalar totalpop2015=_mat_sum[57, 2]
scalar totalpop2016=_mat_sum[58, 2]
scalar totalpop2017=_mat_sum[59, 2]
scalar totalpop2018=_mat_sum[60, 2]
scalar totalpop2019=_mat_sum[61, 2]
scalar totalpop2020=_mat_sum[62, 2]

foreach year in `years' {
	local t: word `iter' of `time'
	
	use "Data/Tax-data/ARG/EstadisticasTributarias`year'/tabulation_`t'.dta", ///
	clear
	
	gen totalpop=totalpop`t'
	tempvar freq cumfreq 
	//bracket frequency
	gsort -thr
	quietly gen `freq'=declarantes/totalpop
	//cumulative frequency
	quietly	gen `cumfreq' = sum(`freq')
	//percentiles
	quietly gen p = 1 - `cumfreq'
	sort thr
	keep average totalpop p
	tempfile p_`t'
	quietly save "`p_`t''"
	
	use "Data/Tax-data/ARG/EstadisticasTributarias`year'/tabulation_`t'.dta", ///
	clear
	
	merge m:m average using "`p_`t''", nogenerate
	
	gen country="ARG" in 1
	gen totalinc = totalnetinc 
	replace average = totalinc / totalpop
	
	if inrange(`year',2003,2020) {
		keep year country totalpop average p bracketavg sh_rent sh_capital ///
			sh_mixed sh_labour sh_business
		order year country totalpop average p bracketavg sh_rent sh_capital ///
			sh_mixed sh_labour sh_business
	}
	
	else {
		keep year country totalpop average p bracketavg
		order year country totalpop average p bracketavg
	}		

	local iter = `iter' + 1
	
	quietly save ///
	"Data/Tax-data/ARG/EstadisticasTributarias`year'/tabulation_`t'.dta", ///
	replace 
	
	cap export excel ///
			"Data/Tax-data/ARG/gpinterinputARG.xlsx", ///
			sheet("`t'", replace) firstrow(variables) keepcellfmt 
			
}

// one gpinter file per year (to be executed after gpinter output)
capture cd "C:/Users/Usuario/Dropbox/LATAM-WIL/"
capture cd "~/Dropbox/DINA-LatAm/"

forvalues year = 2000/2019 {
	
		import excel using "Data/Tax-data/ARG/gpinteroutputARG.xlsx", /// 
			sheet("ARG, `year'") firstrow clear ///			
		
		export excel using "Data/Tax-data/ARG/diverse_ARG_`year'.xlsx", /// 
			firstrow(variables) keepcellfmt replace
			
}

			
