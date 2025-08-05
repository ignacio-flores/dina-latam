////////////////////////////////////////////////////////////////////////////////
// 			Bring tax data from OECD-CIAT-CEPAL 
////////////////////////////////////////////////////////////////////////////////

//General 
clear all

//Paths to files 
global aux_part  ""preliminary"" 
qui do "code/Stata/auxiliar/aux_general.do"

//define language 
local lang $lang
local unit $unit 

//0. Clean data ----------------------------------------------------------------

*bring newest 
qui import delimited "input_data/OECD-CIAT-CEPAL/tot_tax_pct_gdp_2024.csv" ,  clear
qui keep ref_area time_period obs_value revenue_code 
qui rename (ref_area time_period obs_value)(country year c) 
qui reshape wide c, i(country year) j(revenue_code) string
qui drop if inlist(country, "OECD_REP", "A9")
qui renvars, subst("c" "_")
qui rename _ountry iso   
qui levelsof iso, clean local(isos)
qui gen iso_long = ""
foreach i in `isos' {
	di as result "`i': ${lname_`i'_eng}"
	replace iso_long = "${lname_`i'_eng}" if iso == "`i'"
}
qui gen la_g10 = .
foreach i in $all_countries {
	qui replace la_g10 = 1 if iso == "`i'"
}
qui gen country = iso_long 
qui drop iso_long 
qui keep if year >= 2022
tempfile tf_newer 
qui rename _TOTALTAX tax_total
qui drop if missing(country) 
qui save `tf_newer'

*import longer series 
qui import excel "input_data/OECD-CIAT-CEPAL/tot_tax_pct_gdp_2023.xlsx" , ///
	cellrange(A4:BQ998) sheet("OECD.Stat export") firstrow clear

*rename variables 
drop D E
qui rename (Government B C Total) ///
	(group country year tax_total)	
foreach i in `c(ALPHA)'{
	if "`i'" >= "G" local list1 "`list1' `i'"
	if inlist("`i'", "A", "B") {
		foreach j in `c(ALPHA)'{
			if "`i'" == "A" local list`i' "`list`i'' `i'`j'"
			if "`i'" == "B" & "`j'" <= "Q" local list`i' "`list`i'' `i'`j'"
		}
	}
}

*Display list of codes and rename more variables 
foreach i in `list1' `listA' `listB' {
	qui levelsof `i' in 1, local(full_name_`i') clean 
	local code_`i' = substr("`full_name_`i''", 1, 4)
	*count 0s 
	local count0_`code_`i'' = ///
		length("`code_`i''") - length(subinstr("`code_`i''", "0", "", .))
	*add space 
	local space ""
	local m = 4 - `count0_`code_`i''' 
	forvalues n = 1/`m' {
		local space "`space'   "
	}
	qui label var `i' "`full_name_`i''"
	qui replace `i' = "0" if `i' == ".."
	qui local code_name_`code_`i'' "`full_name_`i''" 
	qui rename `i' _`code_`i''
	di as result "`space'`full_name_`i''"
}

*destring 
qui drop if _n < 3
qui replace tax_total = "" if tax_total == ".."
qui destring tax_total year _* `v', replace 

*clean countries and groups
qui replace country = group if !missing(group) & missing(country) 
qui replace group = "OECD - Economies" in 1 
qui replace group = "Other - Groups" if group == "Other Groups"
qui replace group = "" if !strpos(group, "-")
foreach v in group country {
	qui replace `v' = `v'[_n - 1] if missing(`v')
}

*harmonise country names 
qui kountry country, from(other) stuck marker
qui rename _ISO3N_ iso3 
qui kountry iso3, from(iso3n) to(iso3c) 
qui rename _ISO3C_ iso
drop iso3 MARKER 

*marker for LA-G10 countries 
qui gen la_g10 = .
qui replace la_g10 = 1 if country == "Latin America and the Caribbean" 
foreach c in $countries_tax {
	qui replace la_g10 = 1 if iso == "`c'"
}

qui append using `tf_newer'
sort iso year 
ds _* 
foreach tax in `r(varlist)' tax_total {
	replace `tax' = 0 if missing(`tax')
}

//build residual variables 
*Other taxes on products 
qui gen _5999 = _5000 - _5110 
*Other on property
qui egen _auxprop = rowtotal(_4100 _4200 _4300) 
qui gen _4999 = _4000 - _auxprop

//make adhoc arrangement for comparability 
tempvar pers_over_inc 
qui gen `pers_over_inc' = _1100 / _1000 
forvalues t = 2000 / $last_y {
	qui sum `pers_over_inc' if inlist(country, "Chile", "Colombia", "Peru") ///
		& year == `t'
	local avg_ccp = r(mean) 
	qui replace _1100 = _1000 * `avg_ccp' ///
		if country == "Ecuador" & year == `t'
	qui replace _1200 = _1000 * (1-`avg_ccp') ///
		if country == "Ecuador" & year == `t'
	qui replace `pers_over_inc' = `avg_ccp' ///
		if country == "Ecuador" & year == `t'
}
/*
xtline `pers_over_inc' if ///
	inlist(country, "Chile", "Colombia", "Peru", "Ecuador"), ///
	i(country) t(year) overlay
*/	

*1. Exploratory graphs ---------------------------------------------------------
global aux_part  ""graph_basics"" 
qui do "code/Stata/auxiliar/aux_general.do"

*define list of taxes 
*capital gains is always very low (incl in 1100)
*donde hay ssc de self employed, siempre es muy bajo (2300)
*impuestos mineros 
local tax_codes "4200 4100 4300 1200 1300 4999 1100 3000 2000 5110 5999 6000" 
*local tax_codes "4200 4100 4300 4999 1000 3000 2000 5110 5999 6000" 
foreach x in `tax_codes' {
	local tax_vars `tax_vars' _`x'
}

*get total 
foreach v in `tax_vars' {
	foreach f in posi nega {
		if "`f'" == "posi" local sign > 
		if "`f'" == "nega" local sign < 
		qui gen `f'`v' = 0 
		qui replace `f'`v' = `v' if `v' `sign' 0 & !missing(`v')
	}
}

*stack
local iter = 1 
foreach f in posi nega {
	foreach v in `tax_vars' {
		*prepare lists for graph 
		local `f'_stacklist "`f'`v' ``f'_stacklist'"
		local `f'_areas ``f'_areas' ///
			(area `f'_f_`f'`v' year if `f'_f_`f'`v' != 0, color(${c`v'}) lwidth(none) barwidth(.9)) 
		if "`f'" == "posi" local leg_`f' `leg_`f'' `iter' "${lab`v'_`lang'}"
		local iter = `iter' + 1
	}
}
genstack `posi_stacklist' , gen(posi_f_)
genstack `nega_stacklist' , gen(nega_f_)

// Create directory if it doesnt exist 
local dirpath "output/figures/total-taxes"
mata: st_numscalar("exists", direxists(st_local("dirpath")))
if (scalar(exists) == 0) {
	mkdir "`dirpath'"
	display "Created directory: `dirpath'"
}

* without legend
preserve
graph twoway `posi_areas' `nega_areas' ///
	(line tax_total year, lcolor(black) mfcolor(white) ///
	mcolor(black) msize(medsmall)) if la_g10 == 1 ///
	, by(country) ylab(, $lab_opts) xlab(,$lab_opts) ylab(,$lab_opts) ///
	xtit("") ytit("Share of GDP") legend(order(/*`leg_posi'*/ 0 "")) ///
	$graph_scheme 
	//save
qui graph export "output/figures/total-taxes/decomp_taxes_gdp.pdf", replace

* with legend
graph twoway `posi_areas' `nega_areas' ///
	(line tax_total year, lcolor(black) mfcolor(white) ///
	mcolor(black) msize(medsmall)) if la_g10 == 1 ///
	, by(country) ylab(, $lab_opts) xlab(,$lab_opts) ylab(,$lab_opts) ///
	xtit("") ytit("Share of GDP") legend(order(`leg_posi')) ///
	$graph_scheme 
	//save
qui graph export "output/figures/total-taxes/legend.pdf", replace
restore

//2. Merge OECD to UN data and compare totals------------------------------

// Recover 3-letter iso codes and gdp 
preserve
	qui use year country iso_long if !missing(country) ///
		using "intermediary_data/national_accounts/sna-all-countries.dta", clear
	sort country year
	drop if year==year[_n-1]
	rename country iso
	rename iso_long country
	tempfile iso_codes
	qui save `iso_codes', replace
restore

//Merge country year to get 3-letter iso
qui merge m:1 country year using `iso_codes', nogenerate

// Prepare to merge with UN data
qui keep country year tax_total _* iso
rename country country_long
rename iso country

drop if inlist(country_long, ///
	"OECD - Average", "Non-OECD Economies", /// 
	 "Other Groups", "Latin America and the Caribbean")
tempfile tax_tots	 
qui save `tax_tots', replace

//Merge OECD and UN databases
qui use "intermediary_data/national_accounts/UNDATA-WID-Merged.dta", clear
qui merge m:m country year using `tax_tots', nogen

//Keep relevant countries 
tempvar discriminate 
qui gen `discriminate' = . 
foreach c in $all_countries {
	qui replace `discriminate' = 1 if country == "`c'"
}
qui drop if `discriminate' != 1 

//Keep most recent series by year
//Too little detail or empty, while better options are available
drop if series == 1000 & (country == "BRA" & inrange(year, 2000, 2009)) 
drop if series == 200 & (country == "BRA") 
*drop if (country == "ARG" & year >= 2000) & series == 1100 & year != 2016
*drop if (iso == "AR" & year < 2004)
drop if (country == "MEX" & year >= 2000 & year <= 2002) & series == 300

//save country-years in memory 
qui egen country_year = concat(country year)
qui levelsof country, local(all_ctries)
foreach c in `all_ctries' {
	qui levelsof country_year if country == "`c'", local(ctry_yrs_`c')
	foreach cy in `ctry_yrs_`c'' {
		qui sum series if country_year=="`cy'"
		qui drop if country_year=="`cy'" & series!=r(max)
	}		
}	
drop if year < 1990

//get national income adjuster (one of the probably is in real terms)
qui gen gdp_to_gni = gdp_wid / TOT_B5g_wid
qui la var gdp_to_gni "GDP/GNI, from WID.world"

//negative variables 
qui gen neg_4500 = -(_4500)
qui gen neg_4300 = -(_4300)

//Build variables for comparison 

*details 
label var country_long "country"

*tax totals  
local un_rowt_tot TOT_D2D3_U TOT_D5_U TOT_D61_U
local oecd_rowt_tot tax_total neg_4500 neg_4300
*property and income taxes - Total
local un_rowt_pit_tot TOT_D5_U
local oecd_rowt_pit_tot _1100 _1200 _4100 _4200 _4600 _6000 _5200 _5127
*property and income taxes - Corporate sector
local un_rowt_pit_corp NFC_D5_ FC_D5_
local oecd_rowt_pit_corp _1200 _4220
*property and income taxes - Households
local un_rowt_pit_hh HH_D5_U
local oecd_rowt_pit_hh "_1100 _4110 _4210"
*social security contributions 
local un_rowt_ssc TOT_D61_U
local oecd_rowt_ssc _2000
*indirect taxes on goods and services 
local un_rowt_indg TOT_D2_U
local oecd_rowt_indg _3000 _4400 _5000 _6200
*indirect taxes on goods and services (net)
local un_rowt_indn TOT_D2D3_U


*More detailed composition:
local oecd_rowt_pit _1100 _1300 //personal (and other) income tax
local oecd_rowt_cit _1200 //corporate income tax
*social security contributions already defined
local oecd_rowt_prl _3000 //payroll
local oecd_rowt_imo _4100 //immovable property (all)
local oecd_rowt_wea _4200 //wealth of individuals and corp (agregar 4999?)
local oecd_rowt_est _4300 //estate taxes 
local oecd_rowt_otp _4999 //other taxes on property 
local oecd_rowt_gog _5110 //general on goods and services
local oecd_rowt_goo _5999 //other on goods and services
local oecd_rowt_oth _6000 //other 


//loop over fine-detail variables (only in OECD)
foreach v in "pit" "cit" "prl" "imo" "wea" "est" "otp" "gog" "goo" "oth"  {
	*define as % of macro totals 
	qui egen tax_`v'_oecd_gdp = rowtotal(`oecd_rowt_`v'') 
	qui replace tax_`v'_oecd_gdp = tax_`v'_oecd_gdp / 100
	qui label var tax_`v'_oecd_gdp "${labtax_`v'}, % of GDP (OECD)"
	qui gen tax_`v'_oecd_gni = tax_`v'_oecd_gdp * gdp_to_gni
	qui label var tax_`v'_oecd_gni "${labtax_`v'}, % of GNI (OECD)"
}

//loop over broader variables comparing un and oecd
foreach v in "tot" "pit_tot" "pit_corp" "pit_hh" "ssc" "indg" {
	
	*define as % of macro totals (oecd & un)
	qui egen tax_`v'_un = rowtotal(`un_rowt_`v'') 
	qui label var tax_`v'_un "${lab_`v'}, (Current LCU)"
	qui gen tax_`v'_un_gdp = tax_`v'_un / TOT_B1g_R
	qui label var tax_`v'_un_gdp "${lab_`v'}, % of GDP (UN-Data)"
	qui egen tax_`v'_oecd_gdp = rowtotal(`oecd_rowt_`v'') 
	qui replace tax_`v'_oecd_gdp = tax_`v'_oecd_gdp / 100
	qui label var tax_`v'_oecd_gdp "${lab_`v'}, % of GDP (OECD)"
	qui gen tax_`v'_oecd_gni = tax_`v'_oecd_gdp * gdp_to_gni
	qui label var tax_`v'_oecd_gni "${lab_`v'}, % of GNI (OECD)"

	*graph comparison 
	graph twoway ///
		(line tax_`v'_un_gdp year ///
		if tax_`v'_un_gdp != 0, lcolor(black)) ///
		(line tax_`v'_oecd_gdp year ///
		if tax_`v'_oecd_gdp != 0, lcolor(red)) ///
		, by(country_long) ylab(, ${lab_opts}) xlab(, ${lab_opts}) ///
		legend(order(1 "UN" 2 "OECD")) xtit("") ///
		ytit("${lab_`v'}, % of GDP") $graph_scheme 
	//save
	qui graph export "output/figures/total-taxes/`v'.pdf", replace
}

*also the ratio 
qui gen ratio_tax_tot = tax_tot_un_gdp / tax_tot_oecd_gdp

//save dataset
qui save "intermediary_data/national_accounts/UNDATA-WID-OECD-Merged.dta", replace








	
