clear all

global aux_part  ""graph_basics"" 
qui do "code/Do-files/auxiliar/aux_general.do"  
global aux_part  ""preliminary"" 
qui do "code/Do-files/auxiliar/aux_general.do"  


foreach country in $countries_tax { // 
	forvalues year = $first_y/$last_y {  
		
		qui cap confirm file "$svypath/`country'/bfm_norep_pre/`country'_`year'_bfm_norep_pre.dta"
		if _rc == 0 {
			display as result "Calculating S-function for `country' - `year'"
			foreach income in "wage" "total" {
				qui cap use "$svypath/`country'/bfm_norep_pre/`country'_`year'_bfm_norep_pre.dta", clear
				qui cap drop __*

				gen data = "svy"

				qui cap drop wage
				qui gen wage = ind_pre_wag // * 12
				qui cap drop total
				qui gen total = ind_pre_tot // * 12

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

// figures by country

display as result "Plotting everything."

// For all
local 	top_5_thres 	= ln(1/(0.05))
local 	top_1_thres 	= ln(1/(0.01))
local 	top_01_thres 	= ln(1/(0.001))

foreach inc in "wage" "total" {
	mat coeff_all_`inc' = J(21,12,.)
	local y = 1
	qui foreach country in $countries_tax { 
		local y = `y' + 1
		local x = 0     
		forvalues year = $first_y/$last_y {
			local x = `x' + 1
			mat coeff_all_`inc'[`x',1] = `year'
			qui cap confirm file "``country'_`inc'_`year''"
			if _rc == 0 {
				use 			"``country'_`inc'_`year''", clear
				twoway ///
					(scatter 	ln_inc_`year' ln_s, mc($color_1) msize(vsmall)) 	///
					(qfit 		ln_inc_`year' ln_s, lc($color_1) range(1 12) )		///
					, 																///
					legend(order(1 "`country' - `year'")) 		///
					$graph_scheme 								///
					ylabel(-4(1)10, $ylab_opts) 					///
					xlabel(0(2)12, $xlab_opts) 					///
					xtitle("Ln(1/S)")							///
					ytitle("Ln(income/mean of income)")			///
					xline(`top_5_thres' `top_1_thres' `top_01_thres')
					qui graph export "$figs_tail/up_`country'_`inc'_`year'_svy.pdf", replace

					tempvar ln_s2
					gen `ln_s2' = ln_s * ln_s
					reg ln_inc_`year' `ln_s2' ln_s if country == "`country'" 
					local b2_`country'_`year' 	= _b[`ln_s2']
					mat coeff_all_`inc'[`x',`y'] = `b2_`country'_`year''
			}
			else {			
			}
		}
	}
} 


clear
qui svmat coeff_all_total, name(coeff_all)
keep coeff_all*
twoway 	(line coeff_all2 	coeff_all1, lc($c_dom)) 	///
		/*(line coeff_all4 	coeff_all1, lc($c_arg))*/ 	///
		, $graph_scheme							///
		ylabel(-0.12(.02).24, 	$ylab_opts) 	///
		xlabel(1990(2)2020, $xlab_opts)  		///
		ytitle("Coeficiente cuadrático") 		///
		xtitle("Año")  						///
		yline(0, lcolor(black)) 				///
		legend(order(1 "DOM"   ))		
		qui graph export "$figs_tail/quad_coef_total_svy_DOM.pdf", replace


clear
qui svmat coeff_all_total, name(coeff_all)
keep coeff_all*
twoway 	(line coeff_all3 	coeff_all1, lc($c_bra)) 	///
		(line coeff_all4 	coeff_all1, lc($c_chl)) 	///
		(line coeff_all12 	coeff_all1, lc($c_ecu)) 	///
		(line coeff_all6 	coeff_all1, lc($c_col)) 	///
		(line coeff_all7 	coeff_all1, lc($c_per)) 	///
		(line coeff_all8 	coeff_all1, lc($c_ury)) 	///
		(line coeff_all9 	coeff_all1, lc($c_mex)) 	///
		(line coeff_all10 	coeff_all1, lc($c_slv)) 	///
		(line coeff_all11 	coeff_all1, lc($c_cri)) 	///	
		/*(line coeff_all5 	coeff_all1, lc($c_arg))*/ 	///
		/*(line coeff_all2 	coeff_all1, lc($c_dom))*/ 	///
		, $graph_scheme							///
		ylabel(-0.12(.02).12, 	$ylab_opts) 	///
		xlabel(2000(2)2020, $xlab_opts)  		///
		ytitle("Quadratic coefficient") 		///
		xtitle("Year")  						///
		yline(0, lcolor(black)) 				///
		legend(order(1  "BRA" 2 "CHL" 	///
					 3  "ECU" 4  "COL" 			///
					 5  "PER*" 6 "URY"      	///
					 7  "MEX" 8 "SLV" 			///
					 9  "CRI" /*10 "ARG" 11 "DOM"*/))		
		qui graph export "$figs_tail/quad_coef_total_svy.pdf", replace


clear 
qui svmat coeff_all_wage, name(coeff_all)
keep coeff_all*
twoway 	///
		(line coeff_all9 	coeff_all1, lc($c_mex)) 	///
		(line coeff_all11 	coeff_all1, lc($c_cri)) 	///
		(line coeff_all5 	coeff_all1, lc($c_arg)) 	///
		(line coeff_all10 	coeff_all1, lc($c_slv)) 	///	
		(line coeff_all4 	coeff_all1, lc($c_chl)) 	///	
		, $graph_scheme							///
		ylabel(-0.12(.02).12, 	$ylab_opts) 	///
		xlabel(2000(2)2020, $xlab_opts)  		///
		ytitle("Quadratic coefficient") 		///
		xtitle("Year")  						///
		yline(0, lcolor(black)) 				///
		legend(order(1  "MEX" 2  "CRI" 		///
					 3  "ARG" 4 "SLV"		///
					 5  "CHL" ))		
		qui graph export "$figs_tail/quad_coef_wage_svy.pdf", replace



*--------------------------------------------------------------
* ----------- PLOTS BY YEAR----------------
*--------------------------------------------------------------


local 	year_BRA = 2016
local 	year_PER = 2017
local 	year_ECU = 2011
local 	year_COL = 2010
local 	year_CHL = 2017
local 	year_ARG = 2013
local 	year_MEX = 2014
local 	year_URY = 2016
local 	year_SLV = 2017
local 	year_CRI = 2016


qui 	use 			`BRA_total_`year_BRA'', clear
qui 	append using 	`PER_total_`year_PER''
qui 	append using 	`ECU_total_`year_ECU''
qui 	append using 	`COL_total_`year_COL''
qui 	append using 	`CHL_total_`year_CHL''
*qui 	append using 	`ARG_total_`year_ARG''
qui 	append using 	`MEX_total_`year_MEX''
qui 	append using 	`URY_total_`year_URY''
qui 	append using 	`SLV_total_`year_SLV''
qui 	append using 	`CRI_total_`year_CRI''


twoway ///
	(scatter 	ln_inc_`year_ECU' ln_s 	if country == "ECU", mc($c_ecu)  msize(tiny)) 	///
	(qfit 		ln_inc_`year_ECU' ln_s	if country == "ECU", lc($c_ecu)  range(1 12))		///
	(scatter 	ln_inc_`year_COL' ln_s 	if country == "COL", mc($c_col)  msize(tiny)) 	///
	(qfit 		ln_inc_`year_COL' ln_s	if country == "COL", lc($c_col)  range(1 12))		///
	(scatter 	ln_inc_`year_CHL' ln_s 	if country == "CHL", mc($c_chl)  msize(tiny)) 	///
	(qfit 		ln_inc_`year_CHL' ln_s	if country == "CHL", lc($c_chl)  range(1 12))		///
	(scatter 	ln_inc_`year_URY' ln_s 	if country == "URY", mc($c_ury)  msize(tiny)) 	///
	(qfit 		ln_inc_`year_URY' ln_s	if country == "URY", lc($c_ury)  range(1 12))		///
	(scatter 	ln_inc_`year_BRA' ln_s 	if country == "BRA", mc($c_bra) msize(tiny)) 	///
	(qfit 		ln_inc_`year_BRA' ln_s	if country == "BRA", lc($c_bra) range(1 12))		///
	(scatter 	ln_inc_`year_PER' ln_s 	if country == "PER", mc($c_per) msize(tiny)) 	///
	(qfit 		ln_inc_`year_PER' ln_s	if country == "PER", lc($c_per) range(1 12))		///
	(scatter 	ln_inc_`year_SLV' ln_s 	if country == "SLV", mc($c_slv)  msize(tiny)) 	///
	(qfit 		ln_inc_`year_SLV' ln_s	if country == "SLV", lc($c_slv)  range(1 12))		///
	(scatter 	ln_inc_`year_MEX' ln_s 	if country == "MEX", mc($c_mex)  msize(tiny)) 	///
	(qfit 		ln_inc_`year_MEX' ln_s	if country == "MEX", lc($c_mex)  range(1 12))		///
	(scatter 	ln_inc_`year_CRI' ln_s 	if country == "CRI", mc($c_cri)  msize(tiny)) 	///
	(qfit 		ln_inc_`year_CRI' ln_s	if country == "CRI", lc($c_cri)  range(1 12))		///
	/*(scatter 	ln_inc_`year_ARG' ln_s 	if country == "ARG", mc($c_arg)  msize(tiny))*/ 	///
	/*(qfit 		ln_inc_`year_ARG' ln_s	if country == "ARG", lc($c_arg)  range(1 12))*/		///
	,																	///
	legend(order(	1  "ECU - `year_ECU'" 3  "COL - `year_COL'" 		///
					5  "CHL - `year_CHL'" 7  "URY - `year_URY'" 		///
					9  "BRA - `year_BRA'" 11 "PER(*) - `year_PER'" 		///
					13 "SLV - `year_SLV'" 15 "MEX - `year_MEX'"			///
					17 "CRI - `year_CRI'" /*19 "ARG - `year_ARG'"*/))		///
	$graph_scheme 														///
	ylabel(-4(2)10, $ylab_opts) 											///
	xlabel(0(2)12, $xlab_opts) 											///
	xtitle("Ln(1/S)")													///
	ytitle("Ln(income/mean of income)")									///
	xline(`top_5_thres' `top_1_thres' `top_01_thres')
	qui graph export "$figs_tail/up_total_svy.pdf", replace


qui 	use 			`CHL_wage_`year_CHL'', clear
qui 	append using 	`ARG_wage_`year_ARG''
qui 	append using 	`MEX_wage_`year_MEX''
qui 	append using 	`SLV_wage_`year_SLV''
qui 	append using 	`CRI_wage_`year_CRI''

twoway ///
	(scatter 	ln_inc_`year_MEX' ln_s 	if country == "MEX", mc($c_mex)  msize(tiny)) 	///
	(qfit 		ln_inc_`year_MEX' ln_s	if country == "MEX", lc($c_mex)  range(1 12))		///
	(scatter 	ln_inc_`year_CRI' ln_s 	if country == "CRI", mc($c_cri)  msize(tiny)) 	///
	(qfit 		ln_inc_`year_CRI' ln_s	if country == "CRI", lc($c_cri)  range(1 12))		///
	(scatter 	ln_inc_`year_ARG' ln_s 	if country == "ARG", mc($c_arg)  msize(tiny)) 	///
	(qfit 		ln_inc_`year_ARG' ln_s	if country == "ARG", lc($c_arg)  range(1 12))		///
	(scatter 	ln_inc_`year_SLV' ln_s 	if country == "SLV", mc($c_slv)  msize(tiny)) 	///
	(qfit 		ln_inc_`year_SLV' ln_s	if country == "SLV", lc($c_slv)  range(1 12))		///
	(scatter 	ln_inc_`year_CHL' ln_s 	if country == "CHL", mc($c_chl)  msize(tiny)) 	///
	(qfit 		ln_inc_`year_CHL' ln_s	if country == "CHL", lc($c_chl)  range(1 12))		///
	,																	///
	legend(order(	1  "MEX - `year_MEX'" 3  "CRI - `year_CRI'" 		///
					5 "ARG - `year_ARG'" 7 "SLV - `year_SLV'"			///
					9 "CHL - `year_CHL'"))			///
	$graph_scheme 														///
	ylabel(-4(1)10, $ylab_opts) 											///
	xlabel(0(2)12, $xlab_opts) 											///
	xtitle("Ln(1/S)")													///
	ytitle("Ln(income/mean of income)")									///
	xline(`top_5_thres' `top_1_thres' `top_01_thres')
	qui graph export "$figs_tail/up_wage_svy.pdf", replace

