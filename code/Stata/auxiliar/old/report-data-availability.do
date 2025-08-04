//Check what we have 
//N or something else? Agregar opción N1

//preliminary
clear all 
global aux_part  ""preliminary"" 
do "code/Do-files/auxiliar/aux_general.do"

*define first year 
local start_y = 1990

local iter = 1
foreach c in $really_all_countries {
	forvalues t = `start_y' / $last_y {
		
		//define path to files 
		local taxfile "$taxpath`c'/gpinter_`c'_`t'.xlsx"
		local wagfile "$taxpath`c'/wage_`c'_`t'.xlsx"
		local divfile "$taxpath`c'/diverse_`c'_`t'.xlsx"
		local svyfile "$svypath`c'/`c'_`t'N.dta"
		local svyfile2 "$svypath`c'/`c'_`t'N1.dta"
		
		//check tax data (total income)
		cap confirm file `taxfile'
		if !_rc local admin_total_`c'_`t' "File found"
		else local admin_total_`c'_`t' "x"
		
		//check admin data (wages)
		cap confirm file `wagfile' 
		if !_rc local admin_wage_`c'_`t' "File found" 
		else local admin_wage_`c'_`t' "x"
		
		//check admin data (diverse)
		cap confirm file `divfile'
		if !_rc local admin_other_`c'_`t' "File found"
		else local admin_other_`c'_`t' "x" 
		
		//check surveys 
		cap confirm file `svyfile' 
		if !_rc {
			local survey_`c'_`t' "File found" 
		}
		cap confirm file `svyfile2' 
		if !_rc {
			local survey_`c'_`t' "File found" 
		}
		if "`survey_`c'_`t''" != "File found" local survey_`c'_`t' "x"
		
		
		//add one to counter 
		local iter = `iter' + 1
		
	}	
}

local iter = `iter' - 1 
set obs `iter'

//make room for variables 
foreach var in  "country" "admin_total" "admin_wage" "admin_other" "survey" {
	quietly gen `var' = ""
}
quietly gen year = . 
order country year 

//loop again and fill matrix
local iter = 1  
foreach c in $really_all_countries {
	forvalues t = `start_y' / $last_y {
		
		//country and year 
		quietly replace country = "`c'" in `iter'
		quietly replace year = `t' in `iter'
		
		*real variables 
		foreach var in "admin_total" "admin_wage" "admin_other" "survey" {
			quietly replace `var' = "``var'_`c'_`t''" in `iter'
		}
		
 		local iter = `iter' + 1 
	}
}	

quietly export excel  "~/Dropbox/latam_availability.xlsx", ///
	replace keepcellfmt firstrow(variables)
