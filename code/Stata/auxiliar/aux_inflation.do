


* Auxiliary dofile: plots and creates inflation dataset based on WB data

clear all
global data "Data/World_Bank/inflation"
global results 	"figures/inflation"

*previous: argentina------------------------------------------------------------
/*
import excel "$data/ARG_IMF_inflation.xlsx", sheet("Sheet1") firstrow clear
rename C year
rename D inflation
gen country = "ARG"
tempvar aux
ipolate inflation year, gen(`aux') e
replace inflation = `aux' if year > 1999
tempfile arg
save `arg'
*/
*-------------------------------------------------------------------------------
*https://api.worldbank.org/v2/en/indicator/FP.CPI.TOTL.ZG?downloadformat=csv

// load World Bank Data
import delimited "$data/API_FP.CPI.TOTL.ZG_DS2_en_csv_v2_3731334.csv", ///
	varnames(1) rowrange(4) clear

rename ïdatasource country_long
rename worlddevelopmentindicators country

keep if (country_long == "Brazil") | (country_long == "Uruguay") ///
		| (country_long == "El Salvador") | (country_long == "Colombia") ///
		| (country_long == "Chile") | (country_long == "Ecuador") ///
		| (country_long == "Mexico") | (country_long == "Peru")
		
keep country v*
drop v3 v4
		
reshape long v, i(country) j(aux)
rename v inflation
drop aux
bysort country: g year = 1959 + _n

// append ARG
*append using `arg'

// save data
save "$data/inflation_latam", replace

// plot-------------------------------------------------------------------------
keep if year >= $first_y & year <= $last_y
global countries ""ARG" "BRA" "URY" "SLV" "COL" "CHL" "ECU" "MEX" "PER" "

qui foreach country in $countries {
	tempvar inf_`country'
	gen `inf_`country'' = inflation  if country=="`country'"
	label var `inf_`country'' "`country'"
}

global aux_part  ""graph_basics"" 
do "code/Do-files/auxiliar/aux_general.do"

qui twoway 	(connected `inf_ARG' year) ///
			(connected `inf_BRA' year) ///
			(connected `inf_URY' year) ///
			(connected `inf_SLV' year) ///
			(connected `inf_COL' year) ///
			(connected `inf_CHL' year) ///
			(connected `inf_ECU' year) ///
			(connected `inf_MEX' year) ///
			(connected `inf_PER' year) ///
			, ytitle("Inflation") ///
			ylabel(0(20)100, $ylab_opts) ///
			xlabel(2000(5)2020, $xlab_opts) ///
			$graph_scheme ///	
			xtitle("Year")
			graph export "$results/inflation_latam.pdf", replace
