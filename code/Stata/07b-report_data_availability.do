//Check what we have 
//N or something else? Agregar opción N1


//preliminary
clear all 
global aux_part  ""preliminary"" 
do "code/Stata/auxiliar/aux_general.do"

*define first year 
local start_y = 2000
local end = 2024

local iter = 1
foreach c in $all_countries {
	forvalues t = `start_y' / `end' {
		
		//define path to files 
		local taxfile "input_data/admin_data/`c'/gpinter_`c'_`t'.xlsx"
		local wagfile "input_data/admin_data/`c'/wage_`c'_`t'.xlsx"
		local divfile "input_data/admin_data/`c'/diverse_`c'_`t'.xlsx"
		local svyfile "input_data/surveys_CEPAL/`c'/`c'_`t'N.dta"
		local svyfile2 "input_data/surveys_CEPAL/`c'/`c'_`t'N1.dta"
		
		//check tax data (total income)
		cap confirm file `taxfile'
		if !_rc local admin_total_`c'_`t' "yes"
		else local admin_total_`c'_`t' "no"
		
		//check admin data (wages)
		cap confirm file `wagfile' 
		if !_rc local admin_wage_`c'_`t' "yes" 
		else local admin_wage_`c'_`t' "no"
		
		//check admin data (diverse)
		cap confirm file `divfile'
		if !_rc local admin_other_`c'_`t' "yes"
		else local admin_other_`c'_`t' "no" 
		
		//check surveys 
		cap confirm file `svyfile' 
		if !_rc {
			local survey_`c'_`t' "yes" 
		}
		cap confirm file `svyfile2' 
		if !_rc {
			local survey_`c'_`t' "yes" 
		}
		if "`survey_`c'_`t''" != "yes" local survey_`c'_`t' "no"
		
		
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
foreach c in $all_countries {
	forvalues t = `start_y' / `end' {
		
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


* 3) Indicateur "has_tax" = au moins une admin_* == "YES"
ds admin_*
local adminvars `r(varlist)'
gen byte has_tax = 0
foreach v of local adminvars {
    replace has_tax = has_tax | (`v' == "yes")
}
label var has_tax "Any admin/tax data available (from admin_*)"

* 4) Construire la note "grade"
gen byte grade = .
label define grade_lbl ///
    0 "0. Imputed regional average (no data)" ///
    1 "1. Carry forward/backward" ///
    2 "2. Interpolation" ///
    3 "3. Surveys, but no tax data" ///
    4 "4. Tax tabulation and survey microdata" ///
    5 "5. Tax and survey microdata"
label values grade grade_lbl
label var grade "Quality grade for distributional income series"

* Règles de base
replace grade = 3 if survey == "yes" & has_tax == 0
replace grade = 4 if survey == "yes" & has_tax == 1
replace grade = 5 if inlist(country, "MEX", "URY", "CRI") & ///
	has_tax == 1 & survey == "yes"

*Indicateur d'observation "réelle" (déjà notée 3/4/5)
gen byte _obs = inrange(grade, 3, 5)

*  Année observée précédente (prev_obs_year)
bysort country (year): gen prev_obs_year = .
bysort country (year): replace prev_obs_year = year if _obs
bysort country (year): replace prev_obs_year = prev_obs_year[_n-1] if missing(prev_obs_year)

* Année observée suivante (next_obs_year)
gsort country -year
by country: gen next_obs_year = .
by country: replace next_obs_year = year if _obs
by country: replace next_obs_year = next_obs_year[_n-1] if missing(next_obs_year)
gsort country year

* 3) Règles d’attribution 2 (interpolation) et 1 (carry F/B)
replace grade = 2 if missing(grade) & survey=="no" & !missing(prev_obs_year) & !missing(next_obs_year)
replace grade = 1 if missing(grade) & survey=="no" & ///
                    ( (!missing(prev_obs_year) &  missing(next_obs_year)) ///
                   | ( missing(prev_obs_year) & !missing(next_obs_year)) )

qui drop _obs next* prev* 
decode grade, gen(grade_label)
order country year survey has_tax grade grade_label

quietly export excel  "output/data_reports/latam_availability.xlsx", ///
	replace keepcellfmt firstrow(variables) nolab

	