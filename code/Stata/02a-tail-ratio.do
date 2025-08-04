

clear all

global aux_part  ""graph_basics"" 
qui do "code/Do-files/auxiliar/aux_general.do"  
global aux_part  ""preliminary"" 
qui do "code/Do-files/auxiliar/aux_general.do"  


*global all_countries "ARG"

// 1. Tax data
local 	total_BRA "gpinter"
local 	total_PER "gpinter"
local 	total_ECU "gpinter"
local 	total_COL "gpinter"
local 	total_CHL "total"
*local 	total_ARG "diverse"
local 	total_MEX "diverse"
local 	total_URY "gpinter"
local 	total_SLV "gpinter"
local 	total_CRI "diverse" // diverse_CRI_2016_mix_income
local 	total_DOM "gpinter" 

local 	wage_CHL "wages"
local 	wage_CRI "wage"
local 	wage_MEX "wage"
local 	wage_URY "wage"
local 	wage_SLV "wage"
local 	wage_ARG "wage"


foreach inc in "total" "wage" {
	foreach country in  $all_countries  { //   
		forvalues year = $first_y/$last_y {
			local name "``inc'_`country''"

			tempfile taxthres_`country'_`inc'_`year' 
			global route "$taxpath/`country'"

			if inlist("`country'","CRI") { // ,"SLV"
				if "`inc'"=="total" global excel "`name'_`country'_`year'_mix_income"
				else global excel "`name'_`country'_`year'" 
			}

			else {
				global excel "`name'_`country'_`year'"
			}

			cap confirm file "$route/$excel.xlsx"
			if _rc == 0 {
				qui cap erase "$route/$excel.dta"

				qui xls2dta, save($taxpath/`country') : /// 
					import excel "$route/$excel.xlsx" ,  firstrow cellrange(A1)
					qui	use 	"$route/$excel.dta"
					
					// Adjustments and exceptions
					if inlist("`country'","CRI") {
						if "`inc'" == "wage" { 
							qui replace thr  	= thr // * 13
							qui replace topavg  = topavg // * 13
						}
					}
					
					if inlist("`country'","BRA") { 
						qui cap rename topavg_rtot_eqx_imp topavg // for Brasil
					}

					qui rename 	thr threshold_t
					qui rename 	topavg topavg_t
					qui gen 	data = "tax"
					qui gen 	ftile_r = p * 10000

					cap drop country
					gen country = "`country'"
					
					qui	save 	`taxthres_`country'_`inc'_`year''
					display 	"Thresholds in tax data calculated for `country' - `year'"
			}
			else {
				display "There is no `name' tax data for `country' - `year'"
			}
			
			qui cap erase "$route/$excel.dta"
		}			
	}
}

*ARG CHL COL CRI ECU MEX PER SLV bracketavg
*BRA bracketavg_rtot_eqx_imp // bracketavg

// 2. Survey data
foreach country in $all_countries {  
	forvalues year = $first_y/$last_y {  
	    

		qui cap confirm file "$svypath/`country'/raw/`country'_`year'_raw.dta"
		if _rc == 0 {
		    * ARG 2020 exception!
		    if ("`country'"=="ARG" & `year' == 2020) {
			    continue
			}
			display as result "Calculating thresholds for `country' - `year'"
			foreach income in "wage" "total" {
				qui cap use "$svypath/`country'/raw/`country'_`year'_raw.dta", clear
				cap drop __*
				gen data = "svy"

				qui cap drop wage
				qui cap drop total
				if inlist("`country'","BRA","CRI")  {
				   qui gen wage = ind_pre_wag
				   qui gen total = ind_pre_tot
				}
				else {
					qui gen wage = ind_pos_wag // * 12
					qui gen total = ind_pos_tot // * 12
				}

				qui replace _fep = round(_fep)
				qui sum total [fw=_fep], d
				*local mean_income = r(mean)
				local agg_pop = r(N)

				local country = "`country'" 
				qui include "$auxpath/aux_tail_micro.do"
			}
		}
		else {
			display "There is no survey data for `country' - `year'"
		}
	}
}


/*
foreach country in $all_countries  { 
	forvalues year = $first_y/$last_y {  

		qui cap confirm file "$svypath/`country'/raw/`country'_`year'_raw.dta"
		if _rc == 0 {

			qui cap use "$svypath/`country'/raw/`country'_`year'_raw.dta", clear

			gen data = "svy"

			if inlist("`country'","CRI","ARG","MEX") {
				qui cap drop income
				qui gen income = ind_pos_wag 
			}

			if inlist("`country'","BRA","CHL","URY","ECU","COL","SLV") {
				qui cap drop income
				qui gen income = ind_pos_tot 
			}

			if inlist("`country'","PER") {
				qui cap drop income
				qui gen income = $y_postax_tot_per 
			}

			qui replace _fep = round(_fep)
			qui sum income [fw=_fep], d
			local mean_income = r(mean)
			local agg_pop = r(N)
				
			local country = "`country'" 

			qui include "$auxpath/aux_tail_micro.do"

			display 	"Thresholds in survey data calculated for `country' - `year'"
		}
		else {
			display "There is no survey data for `country' - `year'"
		}
	}
}
*/



// 3 . Merge both and calculate ratio
foreach inc in "total" "wage" {
	foreach country in $all_countries  {  
		forvalues year = $first_y/$last_y {  
			tempfile tax_survey_`country'_`inc'_`year' p99_`country'_`inc'_`year'
			clear 

			qui cap confirm file "`taxthres_`country'_`inc'_`year''"
			if _rc == 0 {
				qui use "`taxthres_`country'_`inc'_`year''"
				qui cap confirm file "``country'_`inc'_`year''"
					if _rc == 0 {
						qui cap drop if missing(ftile_r)
						qui merge 	1:1 ftile_r using 	"``country'_`inc'_`year''"
						qui gen 	ratio_ts_t = threshold_t / threshold_s
						qui gen 	ratio_ts_a = topavg_t / topavg_s
						
						qui replace ftile = ftile_r / 10000
						 
						qui cap drop year
						qui gen 	year = `year'
						
						qui save 	`tax_survey_`country'_`inc'_`year'' 	
						display 	"Ratio calculated for `country' - `year'"

						qui keep 	if ftile_r == 9900 	// -----------------------------
						qui save 	`p99_`country'_`inc'_`year''
						
					}
				else {
					display 	"Could not merge `inc' income for `country' - `year'"
				}
			}
			else {
				display 	"Could not merge `inc' income for `country' - `year'"			
			}
		}		
	}
}	
 

// 4. create P99 data sets to plot evolution of ratio over time
foreach inc in "total" "wage" {
	foreach country in $all_countries  { 
		forvalues year = $first_y/$last_y {  
			tempfile ev_taxsur_`country'_`inc'
			qui cap confirm file "`p99_`country'_`inc'_`year''"
			if _rc == 0 {
				u "`p99_`country'_`inc'_`year''", clear
				local x = `year' + 1
				forvalues i = `x'/$last_y {
					cap append using "`p99_`country'_`inc'_`i''" 	
				}
				qui keep ftile country year ratio_ts_t ratio_ts_a
				save `ev_taxsur_`country'_`inc''
				continue, break
			}
			else {
			}
		}
	}
}
			
// 5. Figures 

// Plot ratios: by country-year

global xlab xlabel(1"P95" 5"P99" 10"P99.5" 14"P99.99" 19"P99.995" 23 "P99.999", $xlab_opts)
foreach inc in "total" "wage" {
	foreach country in $all_countries {  
		forvalues year = $first_y/$last_y {  

			qui cap confirm file "`tax_survey_`country'_`year'_`inc''"
			if _rc == 0 {
				u "`tax_survey_`country'_`year'_`inc''", clear

				qui keep if ftile >= .94 & ftile <= .9999
				qui cap drop num
				qui sort country ftile
				qui bysort country: gen num = _n

				twoway ///
					(line 	ratio_ts_a num)				 		///
					, 											///
					$graph_scheme 								///
					ylabel(0(2)30, $ylab_opts) 					///
					$xlab 										///
					xtitle("f-tile")							///
					ytitle("Tax / Survey ratio")				///
					yline(1, lc(black))							///
					aspect(.4)
					qui graph export "$figs_tail/ratio_`country'_`year'_`inc'.pdf", replace
					
			}
			else {
				display "There is no combined tax-survey data for `country' - `year'"			
			}	
		}
	}
}





// plot all circa 2010
local 	year_BRA = 2011
local 	year_PER = 2016
local 	year_ECU = 2010
local 	year_COL = 2010
local 	year_CHL = 2009
local 	year_ARG = 2010
local 	year_MEX = 2010
local 	year_URY = 2010
local 	year_SLV = 2010
local 	year_CRI = 2010
local 	year_DOM = 2012

qui use 				"`tax_survey_BRA_total_`year_BRA''", clear
 	qui append using 	"`tax_survey_PER_total_`year_PER''"
 	qui append using 	"`tax_survey_ECU_total_`year_ECU''"
 	qui append using 	"`tax_survey_COL_total_`year_COL''"
 	qui append using 	"`tax_survey_CHL_total_`year_CHL''"
 	*qui append using 	"`tax_survey_ARG_total_`year_ARG''"
 	qui append using 	"`tax_survey_MEX_total_`year_MEX''"
 	qui append using 	"`tax_survey_URY_total_`year_URY''"
 	qui append using 	"`tax_survey_SLV_total_`year_SLV''"
 	qui append using 	"`tax_survey_CRI_total_`year_CRI''"
	qui append using 	"`tax_survey_DOM_total_`year_DOM''"


qui keep if ftile >= .94 & ftile <= .9999
qui cap drop num
qui sort country ftile
qui bysort country: gen num = _n

tempvar v1 v2
qui gen `v1' = 0
qui gen `v2' = 1

twoway ///
	(line 	ratio_ts_a num if country == "ECU", lc($c_ecu)) 		///
	(line 	ratio_ts_a num if country == "COL" & num<=19, lc($c_col)) 		///
	(line 	ratio_ts_a num if country == "CHL", lc($c_chl)) 		///
	(line 	ratio_ts_a num if country == "URY", lc($c_ury)) 		///
	(line 	ratio_ts_a num if country == "BRA" & num<=19, lc($c_bra)) 		///
	(line 	ratio_ts_a num if country == "PER", lc($c_per)) 		///
	(line 	ratio_ts_a num if country == "SLV" & num<=19, lc($c_slv)) 		///
	(line 	ratio_ts_a num if country == "MEX", lc($c_mex)) 		///
	(line 	ratio_ts_a num if country == "CRI", lc($c_cri)) 		///
	/*(line 	ratio_ts_a num if country == "ARG", lc($c_arg)) */		///
	/*(line 	ratio_ts_a num if country == "DOM", lc($c_dom))*/ 		///
	(rarea 	`v1' `v2' num,  color(gray%30))  		///
	, 															///
	legend(order(	1  "ECU" 2  "COL" 		///
					3  "CHL" 4  "URY" 		///
					5  "BRA" 6  "PER*" 		///
					7  "SLV" 8  "MEX"		///
					9  "CRI" /*10  "ARG" 11 "DOM"*/))		///
	$graph_scheme 								///
	ylabel(0(10)80, $ylab_opts) 					///
	$xlab 										///
	xtitle("f-tile")							///
	ytitle("Tax / Survey ratio")				///
	aspect(.4)
	qui graph export "$figs_tail/ratio_total.pdf", replace



qui use 				"`tax_survey_CHL_wage_`year_CHL''", clear
 	qui append using 	"`tax_survey_ARG_wage_`year_ARG''"
 	qui append using 	"`tax_survey_MEX_wage_`year_MEX''"
 	qui append using 	"`tax_survey_SLV_wage_`year_SLV''"
 	qui append using 	"`tax_survey_CRI_wage_`year_CRI''"
	qui append using 	"`tax_survey_URY_wage_`year_URY''"

qui keep if ftile >= .94 & ftile <= .9999
qui cap drop num
qui sort country ftile
qui bysort country: gen num = _n

tempvar v1 
gen `v1' = 0

twoway ///
	(line 	ratio_ts_a num if country == "CHL", lc($c_chl)) 		///
	(line 	ratio_ts_a num if country == "SLV", lc($c_slv)) 		///
	(line 	ratio_ts_a num if country == "MEX", lc($c_mex)) 		///
	(line 	ratio_ts_a num if country == "CRI", lc($c_cri)) 		///
	(line 	ratio_ts_a num if country == "ARG", lc($c_arg)) 		///
	(line 	ratio_ts_a num if country == "URY", lc($c_ury)) 		///
	(line 	`v1'  	   num, color(gray%0))  		///
	(line 	`v1'  	   num, color(gray%0))  		///
	(line 	`v1'  	   num, color(gray%0))  		///
	(line 	`v1'  	   num, color(gray%0))  		///
	(line 	`v1'  	   num, color(gray%0))  		///
	(line 	`v1'  	   num, color(gray%0))  		///
	, 															///
	legend(order(	1  "CHL" 2  "SLV" 		///
					3  "MEX" 4  "CRI" 		///
					5  "ARG" 6  "URY"		///
					7 " " 8 " " 9 " " 10 " " 11 " " 12 " " ))		///
	$graph_scheme 								///
	ylabel(0(10)80, $ylab_opts) 					///
	$xlab 										///
	xtitle("f-tile")							///
	ytitle("Tax / Survey ratio")				///
	aspect(.4)
	qui graph export "$figs_tail/ratio_wage.pdf", replace


// Ratios over time		
foreach inc in "total" "wage" {
	foreach country in $all_countries {  
			qui cap confirm file "`ev_taxsur_`country'_`inc''"
			if _rc == 0 {
				qui u "`ev_taxsur_`country'_`inc''", clear

				twoway ///
				(line 	ratio_ts_a year)				 		///
				, 											///
				$graph_scheme 								///
				ylabel(0(1)10, $ylab_opts) 					///
				xlabel($first_y(2)$last_y, $xlab_opts) 					///
				xtitle("Year")							///
				ytitle("Tax / Survey top 1% ratio")				///
				aspect(.4)
				qui graph export "$figs_tail/ratio_p99_`country'_`inc'.pdf", replace
			}	 
			else {
			}
	}
}


qui u "`ev_taxsur_ECU_total'", clear
	qui append using "`ev_taxsur_COL_total'"
	qui append using "`ev_taxsur_CHL_total'"
	qui append using "`ev_taxsur_URY_total'"
	qui append using "`ev_taxsur_BRA_total'"
	qui append using "`ev_taxsur_PER_total'"
	qui append using "`ev_taxsur_SLV_total'"
	qui append using "`ev_taxsur_CRI_total'"
	*qui append using "`ev_taxsur_ARG_total'"
	qui append using "`ev_taxsur_MEX_total'"
	*qui append using "`ev_taxsur_DOM_total'"
	
	twoway ///
	(line 	ratio_ts_a year if country == "ECU", lc($c_ecu))	///
	(line 	ratio_ts_a year if country == "COL", lc($c_col))	///
	(line 	ratio_ts_a year if country == "CHL", lc($c_chl))	///
	(line 	ratio_ts_a year if country == "URY", lc($c_ury))	///
	(line 	ratio_ts_a year if country == "BRA", lc($c_bra))	///
	(line 	ratio_ts_a year if country == "PER", lc($c_per))	///	
	(line 	ratio_ts_a year if country == "SLV", lc($c_slv))	///
	(line 	ratio_ts_a year if country == "MEX", lc($c_mex))	///
	(line 	ratio_ts_a year if country == "CRI", lc($c_cri))	///
	/*(line 	ratio_ts_a year if country == "DOM", lc($c_dom))*/	///
	/*(line 	ratio_ts_a year if country == "ARG", lc($c_arg))*/	///
	, 											///
	$graph_scheme 								///
	ylabel(0(1)10, $ylab_opts) 					///
	xlabel($first_y(2)$last_y, $xlab_opts) 					///
	xtitle("Year")							///
	ytitle("Tax / Survey top 1% ratio")				///
	legend(order(	1  "ECU" 2  "COL" 		///
					3  "CHL" 4  "URY" 		///
					5  "BRA" 6  "PER*"		///
					7  "SLV" 8 "MEX"		///
					9 "CRI"  /*10 "DOM"10 "ARG"*/ ))		///
	aspect(.4)
	qui graph export "$figs_tail/ratio_p99_total.pdf", replace

qui u "`ev_taxsur_CHL_wage'", clear
	qui append using "`ev_taxsur_SLV_wage'"
	qui append using "`ev_taxsur_CRI_wage'"
	qui append using "`ev_taxsur_ARG_wage'"
	qui append using "`ev_taxsur_MEX_wage'"
	qui append using "`ev_taxsur_URY_wage'"

	tempvar v1 
	gen `v1' = 0

	
	twoway ///
	(line 	ratio_ts_a year if country == "CHL", lc($c_chl))	///
	(line 	ratio_ts_a year if country == "SLV", lc($c_slv))	///
	(line 	ratio_ts_a year if country == "MEX", lc($c_mex))	///
	(line 	ratio_ts_a year if country == "CRI", lc($c_cri))	///
	(line 	ratio_ts_a year if country == "ARG", lc($c_arg))	///
	(line 	ratio_ts_a year if country == "URY", lc($c_ury))	///
	(line 	`v1'  	   year, color(gray%0))  		///
	(line 	`v1'  	   year, color(gray%0))  		///
	(line 	`v1'  	   year, color(gray%0))  		///
	(line 	`v1'  	   year, color(gray%0))  		///
	(line 	`v1'  	   year, color(gray%0))  		///
	(line 	`v1'  	   year, color(gray%0))  		///
	, 											///
	$graph_scheme 								///
	ylabel(0(1)10, $ylab_opts) 					///
	xlabel($first_y(2)$last_y, $xlab_opts) 					///
	xtitle("Year")							///
	ytitle("Tax / Survey top 1% ratio")				///
	legend(order(	1  "CHL" 2  "SLV" 		///
					3  "MEX" 4  "CRI" 		///
					5  "ARG" 6  "URY"		///
					7 " " 8 " " 9 " " 10 " " 11 " " 12 " " ))		///
	aspect(.4)
	qui graph export "$figs_tail/ratio_p99_wage.pdf", replace

