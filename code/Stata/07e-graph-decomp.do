
clear all 

//General settings -------------------------------------------------------------


global aux_part  ""graph_basics"" 
qui do "code/Do-files/auxiliar/aux_general.do"  
global aux_part  ""preliminary"" 
qui do "code/Do-files/auxiliar/aux_general.do"  

*local droper qui drop if year >= 2021

* Within-between decomposition--------------------------------------------------

* (I) Selected decompositions by step and country
// within-between shares of total inequality

* create datasets per country (w-b)-----------------------
foreach step in  $steps_dec  {
	qui import excel "results/summary/ineqstats_`step'_pch.xlsx", ///
		sheet("gtheils") firstrow clear	
	`droper'	
	foreach c in  $all_countries { 
		preserve
			qui keep if country == "`c'"
			tempfile b_theil_`step'_`c'
			qui gen b_theil_`step' = b_theil/(b_theil + w_theil) 
			qui gen t1_gini_`step' = t1_gini
			qui gen b99_gini_`step' =b99_gini
			qui keep b_theil_`step' t1_gini_`step' b99_gini_`step' year
			qui drop if missing(year)
			qui save `b_theil_`step'_`c''
		restore
	}
}

foreach c in  $all_countries {
	foreach step in  $steps_dec  { 
		if "`step'"=="raw"{
			qui use `b_theil_`step'_`c'', clear
		} 
		else {
			qui merge 1:1 year using `b_theil_`step'_`c'', nogen 
		}
	}
*-----------------------------------------------------

* betweeen by step and country
	twoway 	connect b_theil_raw year, color($c_raw)				||	///
			connect b_theil_bfm_norep_pre year, color($c_bfm) 	||	///
			connect b_theil_natinc year, color($c_nat) 			||	///
			connect b_theil_psp year, color($c_psp) msymbol(X)			||	///
			,									///	
	legend(off)		///
	$graph_scheme 								///
	ylabel(0.1(0.1)0.7, $ylab_opts) 				///
	xlabel($first_y(2)$last_y, $xlab_opts) 		///
	xtitle("year")								///
	ytitle("Between inequality share")	///
	//aspect(.4)
	qui graph export "$figs_decomp/with_betw/summary/b_`c'.pdf", replace	

* betweeen by step and country (legend)
if inlist("`c'","ARG") {
	twoway 	connect b_theil_raw year, color($c_raw)				||	///
			connect b_theil_bfm_norep_pre year, color($c_bfm) 	||	///
			connect b_theil_natinc year, color($c_nat) 			||	///
			connect b_theil_psp year, color($c_psp) msymbol(X)			||	///
			,									///	
	legend(order(1 "raw" 2 "adjusted" 3  "nat. inc." 4  "post. tax. sp." ))		///
	$graph_scheme 								///
	ylabel(0.1(0.1)0.7, $ylab_opts) 				///
	xlabel($first_y(2)$last_y, $xlab_opts) 		///
	xtitle("year")								///
	ytitle("Between inequality share")	///
	//aspect(.4)
	qui graph export "$figs_decomp/with_betw/summary/legend.pdf", replace
}

* betweeen by step and country
	twoway 	connect t1_gini_raw year, color($c_raw)				||	///
			connect t1_gini_bfm_norep_pre year, color($c_bfm) 	||	///
			connect t1_gini_natinc year, color($c_nat) 		||	///
			connect t1_gini_psp year, color($c_psp) msymbol(X)		||	///
			,									///	
	legend(off)		///
	$graph_scheme 								///
	ylabel(0.1(0.1)0.7, $ylab_opts) 				///
	xlabel($first_y(2)$last_y, $xlab_opts) 		///
	xtitle("year")								///
	ytitle("Gini within top 1%")	///
	//aspect(.4)
	qui graph export "$figs_decomp/with_betw/summary/t1_`c'.pdf", replace
	

* betweeen by step and country
	twoway 	connect b99_gini_raw year, color($c_raw)				||	///
			connect b99_gini_bfm_norep_pre year, color($c_bfm) 	||	///
			connect b99_gini_natinc year, color($c_nat) 		||	///
			connect b99_gini_psp year, color($c_psp) msymbol(X)		||	///
			,									///	
	legend(off)		///
	$graph_scheme 								///
	ylabel(0.3(0.1)0.7, $ylab_opts) 				///
	xlabel($first_y(2)$last_y, $xlab_opts) 		///
	xtitle("year")								///
	ytitle("Gini within bottom 99%")	///
	//aspect(.4)
	qui graph export "$figs_decomp/with_betw/summary/b99_`c'.pdf", replace

}

*exit 1

* create datasets per country (source)-----------------------
foreach step in   "raw" "bfm_norep_pre" "rescaled" "natinc"   {
	qui import excel "results/summary/ineqstats_`step'_pch.xlsx", sheet("sginis") firstrow clear
	qui include "code/Do-files/auxiliar/aux_7d.do"
	`droper'
	foreach c in  $all_countries { 
		preserve
			qui keep if country == "`c'"
			tempfile sgini_`step'_`c'
			qui gen kap_c_`step' = `kap_c'
			qui gen wag_g_`step' = `wag_g'
			qui keep kap_c_`step' wag_g_`step' year
			qui drop if missing(year)
			qui save `sgini_`step'_`c''
		restore
	}
}

foreach c in  $all_countries {
	foreach step in  "raw" "bfm_norep_pre" "rescaled" "natinc"  { 
		if "`step'"=="raw"{
			qui use `sgini_`step'_`c'', clear
		} 
		else {
			qui merge 1:1 year using `sgini_`step'_`c'', nogen 
		}
	}

* capital contribution by step and country
	twoway 	connect kap_c_raw year, color($c_raw)				||	///
			connect kap_c_bfm_norep_pre year, color($c_bfm) 	||	///
			connect kap_c_natinc year, color($c_nat) 			||	///
			,									///	
	legend(off)		///
	$graph_scheme 								///
	ylabel(0(0.1).4, $ylab_opts) 				///
	xlabel($first_y(2)$last_y, $xlab_opts) 		///
	xtitle("year")								///
	ytitle("Capital contribution")	///
	//aspect(.4)
	qui graph export "$figs_decomp/source/summary/kap_c_`c'.pdf", replace
	
* capital contribution by step and country (legend)
if inlist("`c'","ARG") {
	twoway 	connect kap_c_raw year, color($c_raw)				||	///
			connect kap_c_bfm_norep_pre year, color($c_bfm) 	||	///
			connect kap_c_natinc year, color($c_nat) 			||	///
			,									///	
	legend(order(1 "raw" 2 "adjusted" 3 "nat. inc."))		///
	$graph_scheme 								///
	ylabel(0(0.1).4, $ylab_opts) 				///
	xlabel($first_y(2)$last_y, $xlab_opts) 		///
	xtitle("year")								///
	ytitle("Capital contribution")	///
	//aspect(.4)
	qui graph export "$figs_decomp/source/summary/legend.pdf", replace
}
	
* wage inequality by step and country
	twoway 	connect wag_g_raw year, color($c_raw)				||	///
			connect wag_g_bfm_norep_pre year, color($c_bfm) 	||	///
			connect wag_g_natinc year, color($c_nat) 			||	///
			,									///	
	legend(off)		///
	$graph_scheme 								///
	ylabel(.4(0.1).8, $ylab_opts) 				///
	xlabel($first_y(2)$last_y, $xlab_opts) 		///
	xtitle("year")								///
	ytitle("Wages Gini")	///
	//aspect(.4)
	qui graph export "$figs_decomp/source/summary/wag_g_`c'.pdf", replace
}
*-----------------------------------------------------

*(II) Full decompositions
// within-between shares of total inequality
/*	
foreach step in  $steps_06b  {
	qui import excel "results/summary/ineqstats_`step'_pch.xlsx", sheet("gtheils") firstrow clear
	foreach c in  $all_countries { // 
		twoway 	area `sh_bet' year if country == "`c'", 				///
				color($c_bet%70)  ||									///
				rarea 	`sh_bet' `sh_wit' year if country == "`c'",		///
				color($c_wit%70)  ||									///
				,									///	
		legend(order(1 "between" 2 "within"))		///
		$graph_scheme 								///
		ylabel(0(0.1)1, $ylab_opts) 				///
		xlabel($first_y(2)$last_y, $xlab_opts) 		///
		xtitle("year")								///
		ytitle("Within-between inequality")	///
		aspect(.4)
		qui graph export "$figs_decomp/with_betw/shares/w_b_sh_`c'_`step'.pdf", replace	
	}
}
*/

// within-between theils' shares
foreach step in  $steps_06b  {
	qui import excel "results/summary/ineqstats_`step'_pch.xlsx", sheet("gtheils") firstrow clear
	`droper'
	
	tempvar tot_theil sh_wit sh_bet
	qui gen `tot_theil' = b_theil + w_theil
	qui gen `sh_bet'	= b_theil / `tot_theil'
	qui gen `sh_wit'	= `sh_bet' + (w_theil / `tot_theil')

	foreach c in  $all_countries { // 
		twoway 	area `sh_bet' year if country == "`c'", 				///
				color($c_bet%70)  ||									///
				rarea 	`sh_bet' `sh_wit' year if country == "`c'",		///
				color($c_wit%70)  ||									///
				,									///	
		legend(order(1 "between" 2 "within"))		///
		$graph_scheme 								///
		ylabel(0(0.1)1, $ylab_opts) 				///
		xlabel($first_y(2)$last_y, $xlab_opts) 		///
		xtitle("year")								///
		ytitle("Within-between inequality")	///
		aspect(.4)
		qui graph export "$figs_decomp/with_betw/shares/w_b_sh_`c'_`step'.pdf", replace	
	}
}

// within-between theils
foreach step in  $steps_06b  {
	qui import excel "results/summary/ineqstats_`step'_pch.xlsx", sheet("gtheils") firstrow clear
	`droper'
	
	foreach c in  $all_countries { // 
			twoway 	connect b_theil year if country == "`c'", 		///
					color($c_bet)  ||								///
					connect w_theil year if country == "`c'", 		///
					color($c_wit)  ||								///
					,									///	
			legend(order(1 "between" 2 "within"))		///
			$graph_scheme 								///
			ylabel(0(0.1)1, $ylab_opts) 				///
			xlabel($first_y(2)$last_y, $xlab_opts) 		///
			xtitle("year")								///
			ytitle("Within-between inequality")	///
			aspect(.4)
			qui graph export "$figs_decomp/with_betw/theils/w_b_dec_`c'_`step'.pdf", replace	
		}
}
 
// top 1 and bottom 99 ginis
foreach step in  $steps_06b  {
	
	

	foreach c in  $all_countries { // 
		twoway 	connect t1_gini  year if country == "`c'", 	///
				color($c_top1)  ||							///
				connect b99_gini year if country == "`c'", 	///
				color($c_bot99) ||							///
				,									///	
		legend(order(1 "top 1%" 2 "bottom 99%"))	///
		$graph_scheme 								///
		ylabel(0(0.1)1, $ylab_opts) 				///
		xlabel($first_y(2)$last_y, $xlab_opts) 		///
		xtitle("year")								///
		ytitle("Gini within groups")	///
		aspect(.4)
		qui graph export "$figs_decomp/with_betw/whithin/within_bottop_`c'_`step'.pdf", replace	
	}
}

* Source decomposition----------------------------------------------------------


foreach step in  "raw" "bfm_norep_pre" "rescaled" "natinc"  {
    
    qui import excel "results/summary/ineqstats_`step'_pch.xlsx", sheet("sginis") firstrow clear
	`droper'
    qui include "code/Do-files/auxiliar/aux_7d.do"

		// Inequality decomposition
	if "`step'"=="raw" | "`step'"=="bfm_norep_pre" | "`step'"=="rescaled"  {
		foreach c in  $all_countries { 
			twoway 	area `var1_c' 		year	if country == "`c'", 	///
					color($c_wag%70) ||									///
					rarea `var1_c' `var2_c' year	if country == "`c'", 	///
					color($c_mix%70) ||									///
					rarea `var2_c' `var3_c' year	if country == "`c'", 	///
					color($c_cap%70) ||									///
					rarea `var3_c' `var4_c' year	if country == "`c'", 	///
					color($c_pen%70) ||									///
					rarea `var4_c' `var5_c' year	if country == "`c'", 	///
					color($c_imp%70) ||									///
					,									///	
			legend(order(1 "wages" 2 "mixed"			///
			3 "capital" 4 "pensions" 5 "imputed"))			///
			$graph_scheme 								///
			ylabel(0(0.1)1, $ylab_opts) 				///
			xlabel($first_y(2)$last_y, $xlab_opts) 		///
			xtitle("year")								///
			ytitle("Source inequality decomposition")	///
			aspect(.4)
			qui graph export "$figs_decomp/source/decomp/s_dec_`c'_`step'.pdf", replace
		}
	}
	

	if "`step'"=="uprofits"   {
		foreach c in  $all_countries { 
			twoway 	area `var1_c' 		year	if country == "`c'", 	///
					color($c_wag%70) ||									///
					rarea `var1_c' `var2_c' year	if country == "`c'", 	///
					color($c_mix%70) ||									///
					rarea `var2_c' `var3_c' year	if country == "`c'", 	///
					color($c_cap%70) ||									///
					rarea `var3_c' `var4_c' year	if country == "`c'", 	///
					color($c_pen%70) ||									///
					rarea `var4_c' `var5_c' year	if country == "`c'", 	///
					color($c_imp%70) ||									///
					rarea `var5_c' `var6_c' year	if country == "`c'", 	///
					color($c_upr%70) ||									///
					,									///	
			legend(order(1 "wages" 2 "mixed"			///
			3 "capital" 4 "pensions" 5 "imputed"		///
			6 "und. prof."))		///
			$graph_scheme 								///
			ylabel(0(0.1)1, $ylab_opts) 				///
			xlabel($first_y(2)$last_y, $xlab_opts) 		///
			xtitle("year")								///
			ytitle("Source inequality decomposition")	///
			aspect(.4)
			qui graph export "$figs_decomp/source/decomp/s_dec_`c'_`step'.pdf", replace
		}
	}
	if "`step'"=="natinc"   {
		foreach c in  $all_countries { 
			twoway 	area `var1_c' 		year	if country == "`c'", 	///
					color($c_wag%70) ||									///
					rarea `var1_c' `var2_c' year	if country == "`c'", 	///
					color($c_mix%70) ||									///
					rarea `var2_c' `var3_c' year	if country == "`c'", 	///
					color($c_cap%70) ||									///
					rarea `var3_c' `var4_c' year	if country == "`c'", 	///
					color($c_pen%70) ||									///
					rarea `var4_c' `var5_c' year	if country == "`c'", 	///
					color($c_imp%70) ||									///
					rarea `var5_c' `var6_c' year	if country == "`c'", 	///
					color($c_upr%70) ||									///
					rarea `var6_c' `var7_c' year	if country == "`c'", 	///
					color($c_lef%70) ||									///
					rarea `var7_c' `var8_c' year	if country == "`c'", 	///
					color($c_indg%70) ||								///
					,									///	
			legend(order(1 "wages" 2 "mixed"			///
			3 "capital" 4 "pensions" 5 "imputed"		///
			6 "und. prof." 7 "other" 8 "tax. products"))		///
			$graph_scheme 								///
			ylabel(0(0.1)1, $ylab_opts) 				///
			xlabel($first_y(2)$last_y, $xlab_opts) 		///
			xtitle("year")								///
			ytitle("Source inequality decomposition")	///
			aspect(.4)
			qui graph export "$figs_decomp/source/decomp/s_dec_`c'_`step'.pdf", replace
		}
	}
	
		// Ginis by source
	if "`step'"=="raw" | "`step'"=="bfm_norep_pre" | "`step'"=="rescaled"  {
		foreach c in  $all_countries { 
			twoway 	connect `var1_g' year	if country == "`c'", 	///
					color($c_wag) ||									///
					connect `var2_g' year	if country == "`c'", 	///
					color($c_mix) ||									///
					connect `var3_g' year	if country == "`c'", 	///
					color($c_cap) ||									///
					connect `var4_g' year	if country == "`c'", 	///
					color($c_pen) ||									///
					connect `var5_g' year	if country == "`c'", 	///
					color($c_imp) ||									///
					,									///	
			legend(off)			///
			$graph_scheme 								///
			ylabel(.4(0.1)1, $ylab_opts) 				///
			xlabel($first_y(2)$last_y, $xlab_opts) 		///
			xtitle("year")								///
			ytitle("Gini by income source")	///
			//aspect(.4)
			qui graph export "$figs_decomp/source/ineq/s_gini_`c'_`step'.pdf", replace
			
			//legend
			if inlist("`c'","ARG") {
				twoway 	connect `var1_g' year	if country == "`c'", 	///
						color($c_wag) ||									///
						connect `var2_g' year	if country == "`c'", 	///
						color($c_mix) ||									///
						connect `var3_g' year	if country == "`c'", 	///
						color($c_cap) ||									///
						connect `var4_g' year	if country == "`c'", 	///
						color($c_pen) ||									///
						connect `var5_g' year	if country == "`c'", 	///
						color($c_imp) ||									///
						,									///	
				legend(order(1 "wages" 2 "mixed"			///
				3 "capital" 4 "pensions" 5 "imputed"))			///
				$graph_scheme 								///
				ylabel(.4(0.1)1, $ylab_opts) 				///
				xlabel($first_y(2)$last_y, $xlab_opts) 		///
				xtitle("year")								///
				ytitle("Gini by income source")	///
				//aspect(.4)
				qui graph export "$figs_decomp/source/ineq/legend_hhinc.pdf", replace
			}
		}
	}
	

	if "`step'"=="uprofits"   {
		foreach c in  $all_countries { 
			twoway 	connect `var1_g' 		year	if country == "`c'", 	///
					color($c_wag) ||									///
					connect `var2_g' year	if country == "`c'", 	///
					color($c_mix) ||									///
					connect `var3_g' year	if country == "`c'", 	///
					color($c_cap) ||									///
					connect `var4_g' year	if country == "`c'", 	///
					color($c_pen) ||									///
					connect `var5_g' year	if country == "`c'", 	///
					color($c_imp) ||									///
					connect `var6_g' year	if country == "`c'", 	///
					color($c_upr) ||									///
					,									///	
			legend(off)		///
			$graph_scheme 								///
			ylabel(.4(0.1)1, $ylab_opts) 				///
			xlabel($first_y(2)$last_y, $xlab_opts) 		///
			xtitle("year")								///
			ytitle("Gini by income source")	///
			//aspect(.4)
			qui graph export "$figs_decomp/source/ineq/s_gini_`c'_`step'.pdf", replace
		}
	}
	if "`step'"=="natinc"   {
		foreach c in  $all_countries { 
			twoway 	connect `var1_g' 		year	if country == "`c'", 	///
					color($c_wag) ||									///
					connect `var2_g' year	if country == "`c'", 	///
					color($c_mix) ||									///
					connect `var3_g' year	if country == "`c'", 	///
					color($c_cap) ||									///
					connect `var4_g' year	if country == "`c'", 	///
					color($c_pen) ||									///
					connect `var5_g' year	if country == "`c'", 	///
					color($c_imp) ||									///
					connect `var6_g' year	if country == "`c'", 	///
					color($c_upr) ||									///
					connect `var7_g' year	if country == "`c'", 	///
					color($c_lef) ||									///
					connect `var8_g' year	if country == "`c'", 	///
					color($c_indg) ||								///
					,									///	
			legend(off)		///
			$graph_scheme 								///
			ylabel(.4(0.1)1, $ylab_opts) 				///
			xlabel($first_y(2)$last_y, $xlab_opts) 		///
			xtitle("year")								///
			ytitle("Gini by income source")	///
			//aspect(.4)
			qui graph export "$figs_decomp/source/ineq/s_gini_`c'_`step'.pdf", replace
			
			//legend
			if inlist("`c'","ARG") {
			twoway 	connect `var1_g' 		year	if country == "`c'", 	///
					color($c_wag) ||									///
					connect `var2_g' year	if country == "`c'", 	///
					color($c_mix) ||									///
					connect `var3_g' year	if country == "`c'", 	///
					color($c_cap) ||									///
					connect `var4_g' year	if country == "`c'", 	///
					color($c_pen) ||									///
					connect `var5_g' year	if country == "`c'", 	///
					color($c_imp) ||									///
					connect `var6_g' year	if country == "`c'", 	///
					color($c_upr) ||									///
					connect `var7_g' year	if country == "`c'", 	///
					color($c_lef) ||									///
					connect `var8_g' year	if country == "`c'", 	///
					color($c_indg) ||								///
					,									///	
			legend(order(1 "wages" 2 "mixed"			///
			3 "capital" 4 "pensions" 5 "imputed"		///
			6 "und. prof." 7 "other" 8 "tax. products"))		///
			$graph_scheme 								///
			ylabel(.4(0.1)1, $ylab_opts) 				///
			xlabel($first_y(2)$last_y, $xlab_opts) 		///
			xtitle("year")								///
			ytitle("Gini by income source")	///
			//aspect(.4)
			qui graph export "$figs_decomp/source/ineq/legend_natinc.pdf", replace
			}
		}
	}	
	
}

	
