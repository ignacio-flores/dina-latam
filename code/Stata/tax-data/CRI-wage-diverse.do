*From monthly wage incomes to yearly (Costa Rica) 

//preliminary
global aux_part  ""preliminary"" 
quietly do "code/Stata/auxiliar/aux_general.do"

forvalues y = 2001(1)2016 {
	
	//Wages 
	quietly import excel "input_data/admin_data/CRI/wage-monthly/wage_CRI_`y'.xlsx", ///
		firstrow clear 
	foreach var in "thr" "bracketavg" "topavg" "average" {
		quietly replace `var' = `var' * 12 
	}
	
	
	quietly export excel "input_data/admin_data/CRI/wage_CRI_`y'.xlsx", replace ///
		firstrow(variables) keepcellfmt
		
	//Diverse income 	
	if `y' >= 2010 {
		quietly import excel "input_data/admin_data/CRI/diverse-mixinc/diverse_CRI_`y'_mix_income.xlsx", ///
			firstrow clear	
	
		quietly export excel "input_data/admin_data/CRI/diverse_CRI_`y'.xlsx", ///
			firstrow(variables) keepcellfmt	replace 
	}
} 
