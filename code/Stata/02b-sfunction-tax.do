clear all

global aux_part  ""graph_basics"" 
qui do "code/Do-files/auxiliar/aux_general.do"  
global aux_part  ""preliminary"" 
qui do "code/Do-files/auxiliar/aux_general.do"  


* ARG------------------------------------------------------------
//wages
display as result "Calculating S-function for Argentina."
local x = 0
foreach name in "total" "wages"  {
	if "`name'" == "wages" {
		local inc "wage"
		qui forvalues year = 2000/2015 {
			local x = `x' + 1
			tempfile ARG_`inc'_`year'
			global excel "$taxpath/ARG/wage_ARG_`year'.xlsx"
			qui cap erase "$taxpath/ARG/wage_ARG_`year'_1.dta" 
			qui xls2dta, sheet("Sheet1") save($taxpath/ARG) : /// //  allsheets
				import excel $excel ,  firstrow cellrange(A1)
				qui u "$taxpath/ARG/wage_ARG_`year'_1.dta", clear
					
			local country = "ARG"	
			include "$auxpath/aux_tail_tabul.do"	
			
			clear
			qui cap erase "$taxpath/ARG/gpinter_ARG_`year'_1.dta"
			 
		}
	}
	if "`name'" == "total" {
		local inc "total"	
		qui forvalues year = 2000/2017 {
			local x = `x' + 1
			tempfile ARG_`inc'_`year'
			global excel "$taxpath/ARG/gpinterinputARG.xlsx"
			qui cap erase "$taxpath/ARG/gpinterinputARG_`x'.dta"
			qui xls2dta, sheet("`year'") save($taxpath/ARG) : /// 
				import excel $excel ,  firstrow cellrange(A1)
				qui u "$taxpath/ARG/gpinterinputARG_`x'", clear
			local country = "ARG"	
			include "$auxpath/aux_tail_tabul.do"	
			qui cap erase "$taxpath/ARG/gpinterinputARG_`x'.dta" 
		}
	}
}

* PER------------------------------------------------------------
display as result "Calculating S-function for Peru."
local x = 0
qui forvalues year = 2016/2018 {
	local x = `x' + 1
	local inc "total"
	tempfile PER_`inc'_`year'
	global excel "$taxpath/PER/gpinter_input/total-PER.xlsx"
	qui cap erase "$taxpath/PER/gpinter_input/total-PER_`x'.dta"
	qui xls2dta, sheet("`year'") save($taxpath/PER/gpinter_input) : /// 
		import excel $excel ,  firstrow cellrange(A1)
		qui u "$taxpath/PER/gpinter_input/total-PER_`x'", clear
	local country = "PER"	
	include "$auxpath/aux_tail_tabul.do"	
	qui cap erase "$taxpath/PER/gpinter_input/total-PER_`x'.dta" 

}


* BRA------------------------------------------------------------
display as result "Calculating S-function for Brazil."
local x = 2
forvalues year = 2006/2019 {
	local x = `x' + 1
	local inc "total"
	tempfile BRA_`inc'_`year'
	global excel "$taxpath/BRA/gpinter_input/total-pre-BRA.xlsx"
	qui cap erase "$taxpath/BRA/gpinter_input/total-pre-BRA_`x'.dta"
	qui xls2dta, sheet("`year'") save($taxpath/BRA/gpinter_input) : /// 
		import excel $excel ,  firstrow cellrange(A1)
		qui u "$taxpath/BRA/gpinter_input/total-pre-BRA_`x'", clear
	local country = "BRA"	
	include "$auxpath/aux_tail_tabul.do"	
	*qui erase "$taxpath/BRA/gpinter_input_rtot_eq_`x'.dta" 

}

/*
* BRA------------------------------------------------------------
display as result "Calculating S-function for Brazil."
local x = 2
forvalues year = 2006/2016 {
	local x = `x' + 1
	local inc "total"
	tempfile BRA_`inc'_`year'
	global excel "$taxpath/BRA/gpinter_input/gpinter_input_rtot_eq.xlsx"
	qui cap erase "$taxpath/BRA/gpinter_input/gpinter_input_rtot_eq_`x'.dta"
	qui xls2dta, sheet("`year'_rank_rtot") save($taxpath/BRA/gpinter_input) : /// 
		import excel $excel ,  firstrow cellrange(A1)
		qui u "$taxpath/BRA/gpinter_input/gpinter_input_rtot_eq_`x'", clear
	local country = "BRA"	
	include "$auxpath/aux_tail_tabul.do"	
	*qui erase "$taxpath/BRA/gpinter_input_rtot_eq_`x'.dta" 

}

local x = 0
forvalues year = 2017/2019 {
	local x = `x' + 1
	local inc "total"
	tempfile BRA_`inc'_`year'
	global excel "$taxpath/BRA/gpinter_input/gpinter_input_BRA_2017-2019.xlsx"
	qui cap erase "$taxpath/BRA/gpinter_input/gpinter_input_BRA_2017-2019_`x'.dta"
	qui xls2dta, sheet("`year'") save($taxpath/BRA/gpinter_input) : /// 
		import excel $excel ,  firstrow cellrange(A1)
		qui u "$taxpath/BRA/gpinter_input/gpinter_input_BRA_2017-2019_`x'", clear
	local country = "BRA"	
	include "$auxpath/aux_tail_tabul.do"	
	*qui erase "$taxpath/BRA/gpinter_input_rtot_eq_`x'.dta" 

}
*/

* CHL------------------------------------------------------------
display as result "Calculating S-function for Chile."
foreach name in "total-pre" "wages"  {
	
	if "`name'" == "wages" {
		global first = 1998
		global last  = 2009
		local inc "wage"
	}
	if "`name'" == "total-pre" {
		global first = 2005
		global last  = 2019
		local inc "total"
	}

	local x = 0
	qui forvalues year = $first/$last  {
		local x = `x' + 1
		tempfile CHL_`inc'_`year'
		global excel "${taxpath}CHL/gpinter_input/`name'-CHL.xlsx"
		qui cap erase "${taxpath}CHL/`name'-CHL_`x'.dta" 
		qui xls2dta,  sheet("`year'") save($taxpath/CHL) : /// // allsheets
			import excel $excel ,  firstrow cellrange(A1)
			qui u "$taxpath/CHL/`name'-CHL_`x'", clear
		
		local country = "CHL"	
		include "$auxpath/aux_tail_tabul.do"	
		qui cap erase "$taxpath/CHL/`name'-CHL_`x'.dta" 
	}
}


* CRI------------------------------------------------------------

display as result "Calculating S-function for Costa Rica."
foreach name in "total" "wages"  {
	if "`name'" == "wages" {
		local inc "wage"
		local x = 0
		qui forvalues year = 2001/2016 {
			local x = `x' + 1
			tempfile CRI_`inc'_`year'
			global excel "$taxpath/CRI/wage_CRI_`year'.xlsx"
			qui cap erase "$taxpath/CRI/wage_CRI_`year'.dta" 
			qui xls2dta,  save($taxpath/CRI) : /// // allsheets
				import excel $excel ,  firstrow cellrange(A1)
				qui u "$taxpath/CRI/wage_CRI_`year'", clear
				preserve
					qui u  "Data/Population/PopulationLatAm.dta", clear
					sum totalpop if country == "            Costa Rica" & year == `year'
					local pop_CRI = r(mean)
				restore
				qui gen population = `pop_CRI'
			local country = "CRI"	
			include "$auxpath/aux_tail_tabul.do"	
			qui cap erase "$taxpath/CRI/wage_CRI_`year'.dta" 
		}
	}
	if "`name'" == "total" {
		local inc "total"
		local x = 0
		qui forvalues year = 2010/2016 {
			local x = `x' + 1
			tempfile CRI_`inc'_`year'
			global excel "$taxpath/CRI/diverse_CRI_`year'_mix_income.xlsx"
			qui cap erase "$taxpath/CRI/diverse_CRI_`year'_mix_income.dta" 
			qui xls2dta,  save($taxpath/CRI) : /// // allsheets
				import excel $excel ,  firstrow cellrange(A1)
				qui u "$taxpath/CRI/diverse_CRI_`year'_mix_income", clear
			
			local country = "CRI"	
			include "$auxpath/aux_tail_tabul.do"	
			qui cap erase "$taxpath/CRI/diverse_CRI_`year'_mix_income.dta" 
		}
	}
}

* COL------------------------------------------------------------
display as result "Calculating S-function for Colombia."

local x = 0
qui forvalues year = 2002/2010 {
	if !inlist(`year',2004,2005) {		

		local x = `x' + 1
		local inc "total"
		tempfile COL_`inc'_`year'
		global excel "$taxpath/COL/old/gpinterinput_CO.xlsx"
		qui cap erase "$taxpath/COL/old/gpinterinput_CO_`x'.dta" 
		qui xls2dta, sheet("`year'")  save($taxpath/COL/old) : /// //  allsheets
			import excel $excel ,  firstrow cellrange(A1)
			qui u "$taxpath/COL/old/gpinterinput_CO_`x'.dta", clear
			
		local country = "COL"	
		include "$auxpath/aux_tail_tabul.do"	

		clear
		qui cap erase "$taxpath/COL/old/gpinterinput_CO_`x'.dta"
	} 
}

/*
local x = 0
qui forvalues year = 1993/2010 {
	local x = `x' + 1
	local inc "total"
	tempfile COL_`inc'_`year'
	global excel "$taxpath/COL/gpinterinput_CO.xlsx"
	qui cap erase "$taxpath/COL/gpinterinput_CO_`x'.dta" 
	qui xls2dta, sheet("`year'_WID")  save($taxpath/COL) : /// //  allsheets
		import excel $excel ,  firstrow cellrange(A1)
		qui u "$taxpath/COL/gpinterinput_CO_`x'", clear
			
	local country = "COL"	
	include "$auxpath/aux_tail_tabul.do"	

	clear
	qui cap erase "$taxpath/COL/gpinterinput_CO_`x'.dta"
}
*/
/*
display as result "Calculating S-function for Colombia."
local x = 0
qui forvalues year = 2002/2010 {
	if !inlist(`year',2004,2005) {		

		local x = `x' + 1
		local inc "total"
		tempfile COL_`inc'_`year'
		global excel "$taxpath/COL/gpinter_input/total-pos-COL.xlsx"
		qui cap erase "$taxpath/COL/gpinter_input/total-pos-COL_`x'.dta" 
		qui xls2dta, sheet("`year'")  save($taxpath/COL/gpinter_input) : /// //  allsheets
			import excel $excel ,  firstrow cellrange(A1)
			qui u "$taxpath/COL/gpinter_input/total-pos-COL_`x'.dta", clear
			
		local country = "COL"	
		include "$auxpath/aux_tail_tabul.do"	

		clear
		qui cap erase "$taxpath/COL/gpinter_input/total-pos-COL_`x'.dta"
	} 
}
*/

* ECU------------------------------------------------------------
display as result "Calculating S-function for Ecuador."
local x = 0
qui forvalues year = 2008/2011 {
	local x = `x' + 1
	local inc "total"
	tempfile ECU_`inc'_`year'
	global excel "$taxpath/ECU/gpinter_input/total-pos-ECU.xlsx"
	qui cap erase "$taxpath/ECU/gpinter_input/total-pos-ECU_`x'.dta" 
	qui xls2dta, sheet("`year'")  save($taxpath/ECU/gpinter_input) : /// //  allsheets
		import excel $excel ,  firstrow cellrange(A1)
		qui u "$taxpath/ECU/gpinter_input/total-pos-ECU_`x'.dta", clear
			
	local country = "ECU"	
	include "$auxpath/aux_tail_tabul.do"	
	
	clear
	qui cap erase "$taxpath/ECU/gpinter_input/total-pos-ECU_`x'.dta"
	 
}

* SLV------------------------------------------------------------

//diverse
display as result "Calculating S-function for El Salvador."
foreach name in "wages" "total"  {

	if "`name'" == "wages" {
		local inc "wage"
	}
	if "`name'" == "total" {
		local inc "total"
	}

	local x = 0
	qui forvalues year = 2000/2017 {
		if !inlist(`year',2008,2011) {	
			local x = `x' + 1
			tempfile SLV_`inc'_`year'
			global excel "$taxpath/SLV/gpinter_input/`name'-SLV.xlsx"
			qui cap erase "$taxpath/SLV/gpinter_input/`name'-SLV_`x'.dta" 
			qui xls2dta,  sheet("`year'") save($taxpath/SLV/gpinter_input) : /// // allsheets
				import excel $excel ,  firstrow cellrange(A1)
				qui cap u "$taxpath/SLV/gpinter_input/`name'-SLV_`x'.dta", clear
			
			local country = "SLV"	
			include "$auxpath/aux_tail_tabul.do"	
			qui cap erase "$taxpath/SLV/gpinter_input/`name'-SLV_`x'.dta" 
		}
	}  
}


* URY------------------------------------------------------------
display as result "Calculating S-function for Uruguay."
local x = 0
qui forvalues year = 2009/2016 {
	local x = `x' + 1
	local inc "total"
	tempfile URY_`inc'_`year'
	global excel "$taxpath/URY/gpinter_URY_`year'.xlsx"
	qui cap erase "$taxpath/URY/gpinter_URY_`year'_1.dta" 
	qui xls2dta, sheet("Sheet1") save($taxpath/URY) : /// //  allsheets
		import excel $excel ,  firstrow cellrange(A1)
		qui u "$taxpath/URY/gpinter_URY_`year'_1.dta", clear
			
	local country = "URY"	
	include "$auxpath/aux_tail_tabul.do"	
	
	clear
	qui cap erase "$taxpath/URY/gpinter_URY_`year'_1.dta"
	 
}


* MEX------------------------------------------------------------

display as result "Calculating S-function for Mexico."
foreach name in "gpinter" "wage"  {

	if "`name'" == "wage" {
		local inc "wage"
	}
	if "`name'" == "gpinter" {
		local inc "total"
	}

	local x = 0
	qui forvalues year = 2009/2014 {
		local x = `x' + 1
		tempfile MEX_`inc'_`year'
		global excel "$taxpath/MEX/`name'_MEX_`year'.xlsx"
		qui cap erase "$taxpath/MEX/`name'_MEX_`year'_1.dta" 
		qui xls2dta, sheet("Sheet1") save($taxpath/MEX) : /// //  allsheets
			import excel $excel ,  firstrow cellrange(A1)
			qui u "$taxpath/MEX/`name'_MEX_`year'_1.dta", clear
				
		local country = "MEX"	
		include "$auxpath/aux_tail_tabul.do"	
		
		clear
		qui cap erase "$taxpath/MEX/`name'_MEX_`year'_1.dta"		 
	}
}

* DOM------------------------------------------------------------
display as result "Calculating S-function for Dominican Republic."

local x = 0
qui forvalues year = 2012/2020 {

	local x = `x' + 1
	local inc "total"
	tempfile DOM_`inc'_`year'
	global excel "$taxpath/DOM/gpinter_input/total-pre-DOM.xlsx"
	qui cap erase "$taxpath/DOM/gpinter_input/total-pre-DOM_`x'.dta" 
	qui xls2dta, sheet("`year'")  save($taxpath/DOM/gpinter_input) : /// //  allsheets
		import excel $excel ,  firstrow cellrange(A1)
		qui u "$taxpath/DOM/gpinter_input/total-pre-DOM_`x'.dta", clear
			
	local country = "DOM"	
	include "$auxpath/aux_tail_tabul.do"	

	clear
	qui cap erase "$taxpath/DOM/gpinter_input/total-pre-DOM_`x'.dta" 
}

/*
*save info on total income declared
preserve
	clear 
	*count countries 
	local iter = 0 
	foreach c in $countries_tax {
		local iter = `iter' + 1 
	}
	*make room 
	local setobs = `iter' * ($last_y - $first_y + 1)
	qui gen country = ""
	qui gen year = . 
	qui gen declared_tot = . 
	set obs `setobs'

	*fill variables 
	local iter = 1 
	foreach c in $countries_tax {
		forvalues t = $first_y / $last_y {
			qui replace country = "`c'" in `iter'
			qui replace year = `t' in `iter'
			if "${agg_inc_`c'_`t'}" != "" {
				qui replace declared_tot = ${agg_inc_`c'_`t'} in `iter'
			} 
			local iter = `iter' + 1 
		}
	}
	*save 
	qui export excel using "${taxpath}declared_tot.xlsx", ///
		firstrow(variables) sheet("Sheet1", modify)
*/		
*--------------------------------------------------------------
* ----------- PLOTS BY COUNTRY----------------
*--------------------------------------------------------------

display as result "Plotting everything."

// For all
local 	top_5_thres 	= ln(1/(0.05))
local 	top_1_thres 	= ln(1/(0.01))
local 	top_01_thres 	= ln(1/(0.001))

// plots by country and coefficients


foreach inc in "total" "wage" {
	mat coeff_all_`inc' = J(31,12,.)
	local y = 1
	qui foreach country in $countries_tax {  
		local y = `y' + 1
		local x = 0  
		forvalues year = $first_y/$last_y {
			local x = `x' + 1
			mat coeff_all_`inc'[`x',1] = `year'
			qui cap confirm file "``country'_`inc'_`year''"
			if _rc == 0 {
				qui use 			"``country'_`inc'_`year''", clear
				cap twoway ///
					(scatter 	ln_inc_`year' ln_s, mc($color_1) msize(vsmall)) 	///
					(qfit 		ln_inc_`year' ln_s, lc($color_1) range(1 12) )		///
					, 																///
					legend(order(1 "`country' - `year'")) 		///
					$graph_scheme 								///
					ylabel(-4(2)10, $ylab_opts) 					///
					xlabel(0(2)12, $xlab_opts) 					///
					xtitle("Ln(1/S)")							///
					ytitle("Ln(income/mean of income)")			///
					xline(`top_5_thres' `top_1_thres' `top_01_thres')
					qui cap graph export "$figs_tail/up_`country'_`inc'_`year'.pdf", replace
				
				tempvar ln_s2
				qui gen `ln_s2' = ln_s * ln_s
				qui cap reg ln_inc_`year' `ln_s2' ln_s if country == "`country'" 
				cap local b2_`country'_`year' 	= _b[`ln_s2']
				qui cap mat coeff_all_`inc'[`x',`y'] = `b2_`country'_`year''
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
		qui graph export "$figs_tail/quad_coef_total_tax_DOM.pdf", replace
		
		
clear
qui svmat coeff_all_total, name(coeff_all)
keep coeff_all*
twoway 	/*(line coeff_all2 	coeff_all1, lc($c_dom)) */ 	///
		(line coeff_all3 	coeff_all1, lc($c_bra)) 	///
		(line coeff_all4 	coeff_all1, lc($c_chl)) 	///
		(line coeff_all6 	coeff_all1, lc($c_col)) 	///
		(line coeff_all7 	coeff_all1, lc($c_per)) 	///
		(line coeff_all8 	coeff_all1, lc($c_ury)) 	///
		(line coeff_all9 	coeff_all1, lc($c_mex)) 	///
		(line coeff_all10 	coeff_all1, lc($c_slv)) 	///
		(line coeff_all11 	coeff_all1, lc($c_cri)) 	///
		(line coeff_all12 	coeff_all1, lc($c_ecu)) 	///
		/*(line coeff_all4 	coeff_all1, lc($c_arg))*/ 	///
		, $graph_scheme							///
		ylabel(-0.12(.03).12, 	$ylab_opts) 	///
		xlabel(1990(2)2020, $xlab_opts)  		///
		ytitle("Quadratic coefficient") 		///
		xtitle("Year")  						///
		yline(0, lcolor(black)) 				///
		legend(order(1  "BRA" 			///
					 2  "CHL" 3  "COL" 			///
					 4  "PER" 					///
					 5  "URY" 6  "MEX"      	///
					 7 "SLV"  8  "CRI" 			///
					 9 "ECU"  /*10  "ARG" 1 "DOM"*/ ))		
		qui graph export "$figs_tail/quad_coef_total_tax.pdf", replace


		
clear
qui svmat coeff_all_wage, name(coeff_all)
keep coeff_all*
twoway 													///
		(line coeff_all4 	coeff_all1, lc($c_chl)) 	///
		(line coeff_all5 	coeff_all1, lc($c_arg)) 	///
		(line coeff_all9 	coeff_all1, lc($c_mex)) 	///
		(line coeff_all10 	coeff_all1, lc($c_slv)) 	///
		(line coeff_all11 	coeff_all1, lc($c_cri)) 	///
		, $graph_scheme							///
		ylabel(-0.12(.03).12, 	$ylab_opts) 	///
		xlabel(1990(2)2020, $xlab_opts)  		///
		ytitle("Quadratic coefficient") 		///
		xtitle("Year")  						///
		yline(0, lcolor(black)) 				///
		legend(order(1  "CHL" 2  "ARG" 			///
					 3  "MEX" 4  "SLV" 			///
					 5  "CRI" 					))		
		qui graph export "$figs_tail/quad_coef_wage_tax.pdf", replace

*--------------------------------------------------------------
* ----------- PLOTS BY YEAR----------------
*--------------------------------------------------------------

local 	year_BRA = 2016
local 	year_PER = 2017
local 	year_ECU = 2011
local 	year_COL = 2010
local 	year_CHL = 2009
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
qui 	append using 	`MEX_total_`year_MEX''
qui 	append using 	`URY_total_`year_URY''
qui 	append using 	`SLV_total_`year_SLV''
qui 	append using 	`CRI_total_`year_CRI''
*qui 	append using 	`ARG_total_`year_ARG''


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
	/*(scatter 	ln_inc_`year_ARG' ln_s 	if country == "ARG", mc($c_arg)  msize(tiny)) 	///
	(qfit 		ln_inc_`year_ARG' ln_s	if country == "ARG", lc($c_arg)  range(1 12))	*/	///
	,																	///
	legend(order(	1  "ECU - `year_ECU'" 3  "COL - `year_COL'" 		///
					5  "CHL - `year_CHL'" 7  "URY - `year_URY'" 		///
					9  "BRA - `year_BRA'" 11 "PER - `year_PER'" 		///
					13 "SLV - `year_SLV'" 15 "MEX - `year_MEX'"			///
					17 "CRI - `year_CRI'" /*19 "ARG - `year_ARG'"*/))		///
	$graph_scheme 														///
	ylabel(-4(2)10, $ylab_opts) 											///
	xlabel(0(2)12, $xlab_opts) 											///
	xtitle("Ln(1/S)")													///
	ytitle("Ln(income/mean of income)")									///
	xline(`top_5_thres' `top_1_thres' `top_01_thres')
	qui graph export "$figs_tail/up_total_tax.pdf", replace


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
	qui graph export "$figs_tail/up_wage_tax.pdf", replace



