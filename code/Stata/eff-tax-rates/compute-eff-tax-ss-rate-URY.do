

clear all
global data 	"Data/Tax-data/URY"
global results 	"figures/eff_tax_rates"

global years " "09" "10" "11" "12" "13" "14" "15" "16" " // 

//call graph parameters 
global aux_part  ""graph_basics"" 
do "code/Do-files/auxiliar/aux_general.do"

qui foreach x in $years { // 
	use "$data/eff-tax-rate/URY_effrates_20`x'", clear

	replace eff_tax_rate_ipol 	= 0 if ftile < .6
	replace eff_ss_rate_ipol 	= 0  if ftile < .6
		
	*set dp com
	twoway 	(connected eff_tax_rate_ipol ftile) ///
			(connected eff_ss_rate_ipol ftile),  ///
			ytitle("Tax and SS effective rates") ///
			xtitle("x-tiles") ///
			yline(100, lpattern(dash) lcolor(black*0.5)) ///
			ylabel(0(0.05)0.3, $ylab_opts) ///
			xlabel(0(0.1)1, $xlab_opts) ///
			$graph_scheme 
			graph export "$results/URY_20`x'.pdf", replace
	
	twoway 	(connected eff_tax_rate_ipol ftile if ftile >= .99) ///
			(connected eff_ss_rate_ipol ftile if ftile >= .99), ///
			ytitle("Tax and SS effective rates") ///
			xtitle("x-tiles") ///
			yline(100, lpattern(dash) lcolor(black*0.5)) ///
			ylabel(0(0.05)0.3, $ylab_opts) ///
			xlabel(0.99(0.001)1, $xlab_opts) ///
			$graph_scheme 
			graph export "$results/URY_20`x'_top1.pdf", replace		
}
