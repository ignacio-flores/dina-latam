clear all

//set directories
global data 	"input_data/admin_data/COL"
global results 	"output/figures/eff_tax_rates"

 local x = 0
 forvalues y=2014/2023 {
	
	local x = `x' + 1
	global route "$data/_clean"
	global excel "total-pos-COL"
	qui cap erase "$route/total-pos-COL_`x'.dta"
	qui xls2dta, sheet("`y'") save($route) : /// 
		import excel  "$route/$excel.xlsx" ,  firstrow cellrange(A1)
		qui	use 	  	  "$route/total-pos-COL_`x'.dta", clear
		qui cap erase     "$route/total-pos-COL_`x'.dta"	
				
	ipolate eff_tax_rate p, gen(eff_tax_rate_ipol) e
	
	gen p_merge = round(p,.00001)
	
	//call graph parameters 
	global aux_part  ""graph_basics"" 
	do "code/Stata/auxiliar/aux_general.do"
			
	label var eff_tax_rate_ipol "Effective tax rate"
				
	form p %15.3fc
	twoway 	(connected eff_tax_rate_ipol p if p >= .99), ///
			ytitle("Effective tax rate") ///
			xtitle("x-tiles") ///
			yline(100, lpattern(dash) lcolor(black*0.5)) ///
			ylabel(0(0.05)0.3, $ylab_opts) ///
			xlabel(0.99(0.001)1, $xlab_opts) ///
			$graph_scheme 
			graph export "$results/COL_`y'_top1.pdf", replace
			
			
	twoway 	(connected eff_tax_rate_ipol p), ///
			ytitle("Effective tax rate") ///
			xtitle("x-tiles") ///
			yline(100, lpattern(dash) lcolor(black*0.5)) ///
			ylabel(0(0.05)0.3, $ylab_opts) ///
			xlabel(0(0.1)1, $xlab_opts) ///
			$graph_scheme 
			graph export "$results/COL_`y'.pdf", replace

	qui replace p_merge = p_merge*10000
	qui duplicates drop p_merge, force
	qui drop if p_merge > 9999
	qui format p_merge %9.0g
	
	// Create directory if it doesnt exist 
	local dirpath "$data/eff-tax-rate"
	mata: st_numscalar("exists", direxists(st_local("dirpath")))
	if (scalar(exists) == 0) {
		mkdir "`dirpath'"
		display "Created directory: `dirpath'"
	}


	save "$data/eff-tax-rate/COL_effrates_`y'", replace
	
		
 }
	



