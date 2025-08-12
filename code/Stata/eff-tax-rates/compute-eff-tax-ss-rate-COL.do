clear all

global data "input_data/admin_data/COL"
global results 	"output/figures/eff_tax_rates"

forvalues x = 2014/2023 {
	import excel "$data/eff-tax-rate/eff_tax_rates_COL.xlsx", ///
		sheet("`x'") cellrange(G3:I129) clear
		rename G p
		rename H eff_tax_rate
		rename I eff_ss_rate
		replace eff_tax_rate = 0 if eff_tax_rate == . & p < .6 // .85 // where the tax begins
		replace eff_ss_rate = 0 if eff_ss_rate == . & p < .6 // .85
					
	ipolate eff_tax_rate p, gen(eff_tax_rate_ipol) e
	ipolate eff_ss_rate p , gen(eff_ss_rate_ipol) e
	
	gen p_merge = round(p,.00001)
	

		
	//call graph parameters 
	global aux_part  ""graph_basics"" 
	do "code/Stata/auxiliar/aux_general.do"
			
	label var eff_tax_rate_ipol "Effective tax rate"
	label var eff_ss_rate_ipol "Effective soc.sec rate"
				
	form p %15.3fc
	twoway 	(connected eff_tax_rate_ipol p if p >= .99) ///
			(connected eff_ss_rate_ipol p if p >= .99), ///
			ytitle("Tax and SS effective rates") ///
			xtitle("x-tiles") ///
			yline(100, lpattern(dash) lcolor(black*0.5)) ///
			ylabel(0(0.05)0.3, $ylab_opts) ///
			xlabel(0.99(0.001)1, $xlab_opts) ///
			$graph_scheme 
			graph export "$results/COL_`x'_top1.pdf", replace
	twoway 	(connected eff_tax_rate_ipol p), ///
			ytitle("Tax and SS effective rates") ///
			xtitle("x-tiles") ///
			yline(100, lpattern(dash) lcolor(black*0.5)) ///
			ylabel(0(0.05)0.3, $ylab_opts) ///
			xlabel(0(0.1)1, $xlab_opts) ///
			$graph_scheme 
			graph export "$results/COL_`x'.pdf", replace

	qui replace p_merge = p_merge*10000
	qui duplicates drop p_merge, force
	qui drop if p_merge > 9999
	qui format p_merge %9.0g

	save "$data/eff-tax-rate/COL_effrates_`x'", replace
		

}

