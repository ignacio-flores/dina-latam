
	clear all
	global data "input_data/admin_data/MEX"
	*global results "output/figures/eff_tax_rates/test"
	global results "output/figures/eff_tax_rates"
	global codes "code/Do-files"


*forvalues i = 0/3 {
	quietly forvalues year = 2009/2014 { // 2009
	
		import excel "$data/gpinter_MEX_`year'.xlsx", ///
			cellrange(C1:J31) firstrow clear
			qui keep p eff_tax_rate
			
	//call graph parameters 
	global aux_part  ""graph_basics"" 
	do "code/Stata/auxiliar/aux_general.do"

	order p eff_tax_rate
	set obs 127
	replace p = 0 if p == .
	sort p
	replace p = (_n-1)/100 if p == 0
	replace eff_tax_rate = 0 if eff_tax_rate == .
	
	gen eff_tax_rate_ipol = eff_tax_rate
	gen p_merge = round(p,.00001)

				
	* plot
	form p %15.1fc
	twoway (connected eff_tax_rate p),  ///
			ytitle("Effective tax rate") ///
			xtitle("x-tiles") ///
			yline(100, lpattern(dash) lcolor(black*0.5)) ///
			ylabel(0(0.05)0.3, $ylab_opts) ///
			xlabel(0(0.1)1, $xlab_opts) ///
			$graph_scheme			
			graph export "$results/MEX_`t'.pdf", replace

			
	form p %15.3fc
	twoway (connected eff_tax_rate p if p>=0.99), ///
			ytitle("Effective tax rate") ///
			xtitle("x-tiles") ///
			yline(100, lpattern(dash) lcolor(black*0.5)) ///
			ylabel(0(0.05)0.3, $ylab_opts) ///
			xlabel(0.99(0.001)1, $xlab_opts) ///
			$graph_scheme 
			graph export "$results/MEX_`t'_top1.pdf", replace

	qui replace p_merge = p_merge*10000
	qui duplicates drop p_merge, force
	qui drop if p_merge > 9999
	qui format p_merge %9.0g
	save "$data/eff-tax-rate/MEX_effrates_`year'", replace 
		
		
	}
*}
		
		
		
