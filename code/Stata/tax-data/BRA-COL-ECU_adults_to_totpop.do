/////////////////////////////////////////////////////////////////////////////
// Adjust fractiles of adult populations to total populations in tax data ///
/////////////////////////////////////////////////////////////////////////////

//0. Define directory
clear all

//preliminary 
global aux_part  ""preliminary"" 
qui do "code/Stata/auxiliar/aux_general.do"

//define list of countries (for BRA, only pre 2007)
local ctries " "BRA" "ECU" " // "COL"

//open population data 
qui use country year totpop pct_adults_ie if ///
	inlist(country, "BRA", "COL", "ECU") using ///
	"intermediary_data/population/SurveyPop.dta", clear 

//store % of adults in memory 
foreach c in `ctries' {
	qui levelsof year if country == "`c'", local(`c'_years) clean 
	di as result "`c'" 
	foreach t in ``c'_years' {
		qui levelsof pct_adults_ie if country == "`c'" & year == `t', ///
			local(pct_ad_`c'_`t') clean
		qui levelsof totpop if country == "`c'" & year == `t', ///
			local(totpop_`c'_`t') clean	
		di as text "`t': `'" `pct_ad_`c'_`t''
	}
}	

*Graph
*graph twoway (connect pct_adults_ie year if country == "BRA")

//loop over gpinter files 
foreach c in `ctries' {
	foreach t in ``c'_years' {
		clear 
		if inlist("`c'", "COL", "ECU") {
			qui cap import excel ///
				"input_data/admin_data/`c'/gpinter_adults/gpinterinput_`c'.xlsx", ///
				firstrow sheet("`t'") clear
		}
		if inlist("`c'", "BRA") {
			qui cap import excel ///
				"input_data/admin_data/BRA/padu_2000-2002-2006.xlsx", ///
				firstrow sheet("`t'") clear 
		}
			
		*check if file exists...	
		qui cap assert _N == 0
		if _rc != 0 {
			
			*make frequencies explicit 
			qui gen freq = p[_n + 1] - p 
			qui replace freq = freq[_n - 1] if missing(freq) 
			qui replace freq = freq * (`pct_ad_`c'_`t'' / 100)
			di as result "Adults/Totalpop: `c' `t': " `pct_ad_`c'_`t'' / 100
			
			*freq was defined as ppl/adults, we multiply by adults/poptot
			qui gsort -p
			qui gen ptot = 1 - sum(freq)
			qui sort p
			qui drop freq p 
			qui rename ptot p 
			
			*adapt average 
			qui order year country average p 
			qui replace average = average * (`pct_ad_`c'_`t'' / 100)
				
			*export 
			if "`c'" == "BRA" {
				qui replace country = "BRA"
				qui export excel "input_data/admin_data/BRA/ptot_`t'.xlsx", ///
				firstrow(variables) replace	
			} 
			if inlist("`c'", "COL", "ECU") {
				
				// Create directory if it doesnt exist 
				local dirpath "input_data/admin_data/`c'/_clean"
				mata: st_numscalar("exists", direxists(st_local("dirpath")))
				if (scalar(exists) == 0) {
					mkdir "`dirpath'"
					display "Created directory: `dirpath'"
				}	
				
				qui export excel "input_data/admin_data/`c'/_clean/total-pos-`c'.xlsx", ///
				firstrow(variables) sheet("`t'", replace)	
			}
		}
	}
}
