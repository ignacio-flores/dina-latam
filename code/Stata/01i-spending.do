////////////////////////////////////////////////////////////////////////////////
//
// 						Title: IMPORT GOVT SPENDING TO MACRO DATABASE
//								(In-kind social transfers)
// 			 Authors: Mauricio DE ROSA, Ignacio FLORES, Marc MORGAN 
// 										Year: 2021
//
////////////////////////////////////////////////////////////////////////////////

clear all

//0. General settings ----------------------------------------------------------
local ext "_norep"
local mode "local" //"update" 

//which local version?
if "`mode'" == "local" {
	local date "1Aug2024"
}

//get list of paths 
global aux_part " "preliminary" " 
qui do "code/Do-files/auxiliar/aux_general.do"
global aux_part " "graph_basics" " 
qui do "code/Do-files/auxiliar/aux_general.do"

*bring expenditures from wid 
if ("`mode'" == "yes") {
	local date = subinstr("$S_DATE", " ", "", .)
	local spenvars meduge mheage mexpgo mcongo
	qui wid, areas(${areas_wid_latam}) ind(`spenvars' mgdpro)
	qui keep country variable year value 
	qui replace variable = subinstr(variable, "999i", "", .)
	qui rename value v_
	qui reshape wide v_, i(country year) j(variable) string
	foreach v in `spenvars' {
		qui rename v_`v' `v'
		qui replace `v' = `v' / v_mgdpro * 100
	}
	qui drop v_mgdpro 
	qui keep if year >= 2000
	qui kountry country, from(iso2c) to(iso3c)
	qui drop country 
	qui rename _ISO3C_ iso
	qui rename (`spenvars') (edu hea exp con)
	qui gen source = "WID_web"
	qui save "Data/national_accounts/WID/gov_expenditure_`date'.dta"
}

qui use "Data/national_accounts/WID/gov_expenditure_`date'.dta", clear
qui drop con 
qui rename exp con 
tempfile tfwid
qui save `tfwid' 

*bring un wb data 
qui use "${govt_exp}", clear 
qui append using `tfwid'

qui kountry iso, from(iso3c) geo(undet)
qui keep if inlist(GEO, "Central America", "Caribbean", "South America")

qui gen isit = .
foreach c in $all_countries {
	qui replace isit = 1 if iso == "`c'"
}

//keep variables of interest (use World Bank series due to greater coverage)
keep if isit ==  1
qui replace source = "WID_old" if source == "WID"
drop if inlist(source, "WID_old", "IMF")
qui replace source = "WID" if source == "WID_web"
drop if year < 1990
keep iso year source exp con hea edu 
rename iso country

//reahape to ccompare series 
qui replace source = lower(source) 
qui replace source = "_" + source
qui reshape wide hea edu con /*exp*/, i(country year) j(source) string 

foreach v in con /*exp*/ hea edu {
	qui rename `v'_wb `v'_gdp_wb
	qui rename `v'_wid `v'_gdp_wid
}

//interpolate (and extrapolate) missing values
foreach var in "con_gdp_wb" "hea_gdp_wb" "edu_gdp_wb" {
	tempvar aux_ipol
	by country: ipolate `var' year, generate(`aux_ipol') epolate
	replace `var' = `aux_ipol' if missing(`var')
}

//generate other in-kind transfers
foreach s in wb wid {
	qui egen hedu_gdp_`s' = rowtotal(hea_gdp_`s' edu_gdp_`s')
	qui gen oex_gdp_`s' = con_gdp_`s' - hedu_gdp_`s'

}
tempfile tf_exp
qui save `tf_exp', replace

//Merge to SNA-WID-OECD database
qui use "${sna_wid_oecd}", clear
cap drop __*
qui merge m:m country year using `tf_exp', nogen

//Keep relevant countries 
tempvar discriminate 
qui gen `discriminate' = . 
foreach c in $all_countries {
	qui replace `discriminate' = 1 if country == "`c'"
}
qui drop if `discriminate' != 1 

foreach i in edu hea oex {
	//graph item by source with legend
	graph twoway ///
		(connect `i'_gdp_wb  year if !missing(`i'_gdp_wb), msize(small)) ///
		(connect `i'_gdp_wid year if !missing(`i'_gdp_wid), msize(small)) ///
		if year >= 2000 & country != "ARG", by(country_long) $graph_scheme ///
		legend(order(1 "Old" 2 "New")) ///
		ylabel(, $lab_opts) xlab(,$lab_opts) xtit("") ytit("Share of GDP")
		//save
		qui graph export "figures/spending/`i'_wid_vs_old.pdf", replace
}	
qui drop *_wb

//save dataset
qui save "${sna_wid_oecd_wb}", replace

