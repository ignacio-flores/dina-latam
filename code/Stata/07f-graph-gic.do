

clear all 

//General settings -------------------------------------------------------------

global aux_part  ""graph_basics"" 
qui do "code/Do-files/auxiliar/aux_general.do"  

global aux_part  ""preliminary"" 
qui do "code/Do-files/auxiliar/aux_general.do"  


// Growth incidende curves
foreach c in  $all_countries {
	// define first and last years for each country
	if inlist("`c'","CRI","MEX","PER") {
		local y_f_`c' = 2004
	}
	else {
		local y_f_`c' = 2003
	}
	local y_f_DOM = 2012
	
	if inlist("`c'","MEX") {	
		local y_l_`c' = 2014
	}
	else {
		local y_l_`c' = 2013
	}

	foreach step in  $steps_06b  {
		foreach y in `y_f_`c'' `y_l_`c'' {
		//bring in inflation ratess 
		qui import excel "Data/infl_xrates_wid_wb.xlsx", ///
			sheet("inflation-xrates") firstrow clear
			qui keep if country == "`c'" & year == `y'
			local def_`c'_`y' = defl_xxxx 
				

		tempfile `c'`step'`y' `c'`step'
		qui import excel "results/summary/ineqstats_`step'_pch.xlsx", sheet("`c'`y'") firstrow clear
			
			qui replace p = round(p * 10000)
			qui drop if p > 9900
			qui gen 	av_p_`step'_`y' = avg / `def_`c'_`y''
			qui replace av_p_`step'_`y' = topavg / `def_`c'_`y'' if p == 9900
			qui egen 	average_`step'_`y' = mean(average / `def_`c'_`y'')
			qui keep av_p_`step'_`y' p average_`step'_`y'
						
			qui save ``c'`step'`y''
		}
	qui use ``c'`step'`y_f_`c'''
	qui merge 1:1 p using ``c'`step'`y_l_`c''', nogen 
	qui gen gic_`step' = (av_p_`step'_`y_l_`c'' / av_p_`step'_`y_f_`c'') - 1
	*qui drop if gic_`step' < 0
	*qui replace gic_`step' = 0 if gic_`step' < 0
	qui replace gic_`step' = gic_`step' * 100
	qui replace p = p / 10000 + 0.01
	qui gen aver_gic_`step' = (average_`step'_`y_l_`c'' / average_`step'_`y_f_`c'') - 1
	qui replace aver_gic_`step' = aver_gic_`step' * 100
	qui save ``c'`step'' 
	}

qui use ``c'raw' 
qui merge 1:1 p using ``c'bfm_norep_pre', nogen 
qui merge 1:1 p using ``c'natinc', nogen
qui drop if p < .1

// GIC (no legend)
	twoway 	line gic_natinc 		p ///
			if gic_natinc < 200, color($c_nat) ||	///
			line aver_gic_natinc	p, color(black) symbol(x) 			||	///
			,									///	
	legend(off)		///
	$graph_scheme 								///
	ylabel(-25(25)200, $ylab_opts) 				///
	xlabel(0.1(0.1)1, $xlab_opts) 		///
	yline(0, lpattern(dash) lcolor(black)) ///
	xtitle("percentiles")								///
	ytitle("Average growth rate (%)")	///
	//aspect(.4)
	qui graph export "$figs_gic/gic_`c'.pdf", replace
	
	twoway 	line gic_natinc 		p ///
			if gic_natinc < 200, color($c_nat) ||	///
			line aver_gic_natinc	p, color(black) symbol(x) 			||	///
			line gic_raw 		p ///
			if gic_raw < 200, color($c_raw) ||	///
			line aver_gic_raw	p, color(black*0.5) symbol(x) 			||	///
			,									///	
	legend(off)		///
	$graph_scheme 								///
	ylabel(-25(25)200, $ylab_opts) 				///
	xlabel(0.1(0.1)1, $xlab_opts) 		///
	yline(0, lpattern(dash) lcolor(black)) ///
	xtitle("percentiles")								///
	ytitle("Average growth rate (%)")	///
	//aspect(.4)
	qui graph export "$figs_gic/gic_`c'_comp.pdf", replace
	
	
// GIC (legend)
if inlist("`c'","ARG") {
		twoway 	line gic_natinc 		p ///
				if gic_natinc < 200, color($c_nat) ||	///
				line aver_gic_natinc	p, color(black) symbol(x) 			||	///
				,									///	
		legend(order( 1  "Percentile growth"  2 "Average growth" ))		///
		$graph_scheme 								///
		ylabel(-25(25)200, $ylab_opts) 				///
		xlabel(0.1(0.1)1, $xlab_opts) 		///
		xtitle("percentiles")								///
		ytitle("Average growth rate (%)")	///
		//aspect(.4)
		qui graph export "$figs_gic/legend.pdf", replace
		
		twoway 	line gic_natinc 		p ///
				if gic_natinc < 200, color($c_nat) ||	///
				line aver_gic_natinc	p, color(black) symbol(x) 			||	///
				line gic_raw		p ///
				if gic_raw < 200, color($c_raw) ||	///
				line aver_gic_raw	p, color(black*0.5) symbol(x) 			||	///
				,									///	
		legend(order( 1  "National income growth"  2 "Average national income growth"  3 "Survey income growth" 4 "Average survey income growth") col(1))		///
		$graph_scheme 								///
		ylabel(-25(25)200, $ylab_opts) 				///
		xlabel(0.1(0.1)1, $xlab_opts) 		///
		xtitle("percentiles")								///
		ytitle("Average growth rate (%)")	///
		//aspect(.4)
		qui graph export "$figs_gic/legend_comp.pdf", replace
	}
}

		
		
