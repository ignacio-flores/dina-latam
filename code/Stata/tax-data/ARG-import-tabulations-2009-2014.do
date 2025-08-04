//Transform Argentinian fiscal tabulations from html to xls

//General 
clear all

//Note: copy and paste the following tabulations 2.2.2.1.1 | 2.2.2.1.2 | 2.2.2.1.11 
forvalues year = 2009/2014 {
	foreach tab in "2.2.2.1.1" "2.2.2.1.2" "2.2.2.1.11" {
	
		//define range
		if ("`tab'" == "2.2.2.1.1") local cellr "B18:P35" 
		else if ("`tab'" == "2.2.2.1.2") local cellr "B18:Q35" 
		else local cellr "B18:H35"
	
		//import
		import excel ///
			"Data/Tax-data/ARG/EstadisticasTributarias`year'/`tab'.xlsx", ///
			sheet("Hoja1") cellrange(`cellr') clear
			
		if ("`tab'" == "2.2.2.1.1"){ 	
			//Change names	
			quietly rename (B C D E F G H I J K L M N O P) ///
				(desde hasta declarantes totinc tot_1_4cat ///
				casos_1cat importe_1cat casos_2cat importe_2cat ///
				casos_3cat importe_3cat casos_4cat importe_4cat ///
				casos_part_emp importe_part_emp)
			cap drop Q 
			drop hasta
		}
		
		else if ("`tab'" == "2.2.2.1.2"){ 	
			//Change names
			quietly rename (B C D E F G H I J K L M N O P Q) ///
			(desde hasta declarantes_ing declarantes_ded totded totded_1_4cat ///
			casosded_1cat importeded_1cat casosded_2cat importeded_2cat ///
			casosded_3cat importeded_3cat casosded_4cat importeded_4cat ///
			casos_part_emp_qb importe_part_emp_qb)		
			drop hasta
		}
		
		else {
			quietly rename (B C D E F G H) ///
				(desde hasta declarantes_tax totinc_tax casos_gan ///
				importe_gan impuesto)
			drop hasta
		}
		
		ds
		local vars "`r(varlist)'"
		foreach v in `vars' {
			quietly replace `v' = subinstr(`v', ".", "",.) 
			destring `v', replace
		}

		quietly save ///
			"Data/Tax-data/ARG/EstadisticasTributarias`year'/`tab'.dta", ///
			replace 
	}
}	
