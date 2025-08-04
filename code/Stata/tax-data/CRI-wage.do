*From monthly wage incomes to yearly (Costa Rica) 

//preliminary
global aux_part  ""preliminary"" 
quietly do "code/Do-files/auxiliar/aux_general.do"

forvalues y = 2001(1)2016 {
	
	//Wages 
	quietly import excel "${taxpath}CRI/monthly/wage_CRI_`y'.xlsx", ///
		firstrow clear 
	foreach var in "thr" "bracketavg" "topavg" "average" {
		quietly replace `var' = `var' * 12 
	}
	quietly export excel "${taxpath}CRI/wage_CRI_`y'.xlsx", replace ///
		firstrow(variables) keepcellfmt
		
	//Diverse income 	
	if `y' >= 2010 {
		/*
		quietly import excel "${taxpath}CRI/yearly/diverse_CRI_`y'.xlsx", ///
			firstrow clear	*/
		quietly import excel "${taxpath}CRI/diverse_CRI_`y'_mix_income.xlsx", ///
			firstrow clear	
		*quietly rename bckt_avg bracketavg 	
		quietly export excel "${taxpath}CRI/diverse_CRI_`y'.xlsx", ///
			firstrow(variables) keepcellfmt	replace 
	}
} 
