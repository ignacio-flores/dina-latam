

clear all
global data "input_data/admin_data/CHL"
global results 	"output/figures/eff_tax_rates/test"
	
//call graph parameters 
global aux_part  ""graph_basics"" 
do "code/Stata/auxiliar/aux_general.do"

forvalues x = 2005/2022 {
	foreach name in "pre" "pos" {
		tempfile `name'_CHL_`x'
		* import income distribution from gpinter 
		import excel "$data/gpinter_output/total-`name'-CHL.xlsx", ///
			sheet("`name'tax, CHL, `x'") cellrange(E1:I128) firstrow clear
			qui keep p bracketavg
			qui rename bracketavg `name'_bracketavg
			qui save ``name'_CHL_`x''
	}

	qui use "`pre_CHL_`x''"
	qui merge 1:1 p using "`pos_CHL_`x''", nogen
	qui gen eff_tax_rate =  (pre_bracketavg - pos_bracketavg) / pre_bracketavg
	qui replace eff_tax_rate = 0 if p == 0 
	*qui save "$data/eff-tax-rate/CHL_effrates_`x'", replace

	form p %15.1fc
	twoway (connected eff_tax_rate p),  ///
			ytitle("Effective tax rate") ///
			xtitle("x-tiles") ///
			yline(100, lpattern(dash) lcolor(black*0.5)) ///
			ylabel(0(0.05)0.3, $ylab_opts) ///
			xlabel(0(0.1)1, $xlab_opts) ///
			$graph_scheme			
	graph export "$results/CHL_`x'.pdf", replace		
	
	form p %15.3fc
	twoway (connected eff_tax_rate p if p>=0.99), ///
			ytitle("Effective tax rate") ///
			xtitle("x-tiles") ///
			yline(100, lpattern(dash) lcolor(black*0.5)) ///
			ylabel(0(0.05)0.3, $ylab_opts) ///
			xlabel(0.99(0.001)1, $xlab_opts) ///
			$graph_scheme 
	graph export "$results/CHL_`x'_top1.pdf", replace
}	
