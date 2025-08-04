

clear all

global data "Data/Tax-data/ECU"
global results 	"figures/eff_tax_rates"

forvalues x = 2008/2011 {
	import excel "$data/eff-tax-rate/eff-tax-rates-ECU.xlsx", ///
		sheet("`x'") cellrange(G6:I131) clear
		rename G p
		rename H eff_tax_rate
		rename I eff_ss_rate
		replace eff_tax_rate = 0 if eff_tax_rate == . & p < .85
		replace eff_ss_rate = 0 if eff_ss_rate == . & p < .85
					
	ipolate eff_tax_rate p, gen(eff_tax_rate_ipol) e
	ipolate eff_ss_rate p , gen(eff_ss_rate_ipol) e
		
	gen p_merge = round(p,.00001)
	
		
	//call graph parameters 
	global aux_part  ""graph_basics"" 
	do "code/Do-files/auxiliar/aux_general.do"
			
	label var eff_tax_rate_ipol "Effective tax rate"
	label var eff_ss_rate_ipol "Effective soc.sec rate"
	

	twoway 	(connected eff_tax_rate_ipol p) ///
			(connected eff_ss_rate_ipol p),  ///
			ytitle("Tax and SS effective rates") ///
			xtitle("x-tiles") ///
			yline(100, lpattern(dash) lcolor(black*0.5)) ///
			ylabel(0(0.05)0.3, $ylab_opts) ///
			xlabel(0(0.1)1, $xlab_opts) ///
			$graph_scheme 
			graph export "$results/ECU_`x'.pdf", replace
				
	form p %15.3fc
	twoway 	(connected eff_tax_rate_ipol p if p >= .99) ///
			(connected eff_ss_rate_ipol p if p >= .99), ///
			ytitle("Tax and SS effective rates") ///
			xtitle("x-tiles") ///
			yline(100, lpattern(dash) lcolor(black*0.5)) ///
			ylabel(0(0.05)0.3, $ylab_opts) ///
			xlabel(0.99(0.001)1, $xlab_opts) ///
			$graph_scheme 
			graph export "$results/ECU_`x'_top1.pdf", replace
				
	qui replace p_merge = p_merge*10000
	qui duplicates drop p_merge, force
	qui drop if p_merge > 9999
	qui format p_merge %9.0g
	save "$data/eff-tax-rate/ECU_effrates_`x'", replace
			
}
