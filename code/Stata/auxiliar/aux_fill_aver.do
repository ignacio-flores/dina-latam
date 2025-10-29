
//select countries to adjust
global aux_part  ""aver_countries"" 
quietly do "code/Stata/auxiliar/aux_general.do"

//create an exception for Argentina (not used for averages)
tempvar year_aux
gen `year_aux' = year if country != "ARG"

//flag inter/extrapolations
foreach c in $fix_countries {
	foreach v in $imput_vars {
		cap gen extsna_`v' = 0
		replace extsna_`v' = 1 if missing(`v') == 1 ///
			& (country == "`c'")	
		lab var extsna_`v' ///
			"Extrapolated based on remaining country/years"
	}
}

//identify last value and year for each country
foreach c in $fix_countries {
	foreach v in $imput_vars {
		quietly sum year if country == "`c'" & !missing(`v')
		local m`v'_`c'y = r(max) 
		quietly sum `v' if country == "`c'" & year == `m`v'_`c'y' 
		local m`v'_`c' = r(max) 
	}
}

//create country/year averages
distinct year
local num_years = r(ndistinct)
foreach v in $imput_vars {
	local mean_v = 0
	tempvar nodata_`v'
	qui gen `nodata_`v'' = 0

	tempvar `v'_aux1 `v'_aux2  
	qui gen ``v'_aux1' = 0
	qui gen ``v'_aux2' = 0
	
	
	foreach c in $fix_countries {		

		// signal countries with no data
		count if missing(`v') & (country == "`c'")
		qui replace `nodata_`v'' = 1 if (country == "`c'") & r(N) == `num_years'


		// create variable of average of own country
		qui sum `v' if !missing(`v') & (country == "`c'")
		local m_`v' = r(mean)
		qui replace ``v'_aux1' = `m_`v''  if country == "`c'"

		// average of all countries with data (to use in countries with no data)
		qui sum ``v'_aux1' if !missing(``v'_aux1') 
		local m_`v'_all = r(mean)
		qui replace ``v'_aux2' = `m_`v'_all'  /*if country == "`c'"*/
	}


	//replace missing years in countries with some data
	quietly replace `v' = ``v'_aux1' ///
		if missing(`v')  // & !missing(``v'_aux1')
		
}

//to replace countries with no data whatsoever
foreach c in $fix_countries {
	foreach v in $imput_vars {
		qui replace `v' = ``v'_aux2' ///
			if /*missing(`v')==1 & (country == "`c'") &*/ `nodata_`v'' == 1
	}
}

//extend for the future
foreach c in $fix_countries {
	foreach v in $imput_vars {
		replace `v' = `m`v'_`c'' ///
			if year >= `m`v'_`c'y' & country == "`c'"
	}
}

qui cap replace extsna_pre_kap = 0 
tempvar aux4
egen `aux4' = rowtotal(extsna*)
cap gen extrap_sca = 0
cap replace extrap_sca = 1 if `aux4' > 0


