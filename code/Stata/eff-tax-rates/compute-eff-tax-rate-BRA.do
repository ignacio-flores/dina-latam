

clear all
global data "input_data/admin_data/BRA"
global results 	"output/figures/eff_tax_rates"

forvalues x = 2007/2023 {

	* import effective tax rates for known points
	if `x' < 2014 {

		import excel "$data/eff-tax-rate/eff-tax-rates-BRA.xlsx", ///
				sheet("`x'") cellrange(N25:O36) firstrow clear
				
		forvalues i = 1/11 {
			local income_`i' 	=  income[`i'] 
			local tax_`i'		=  eff_tax_rate[`i']
		}

		* import income distribution from gpinter 
		import excel "$data/gpinter_BRA_`x'.xlsx", ///
		sheet("BRA – equal split, `x'") cellrange(E1:F128) firstrow clear
		save "$data/gpinter_BRA_`x'.dta", replace
		
		forvalues i = 1/11 {
			
			gen income_`i' 	= `income_`i'' 
			gen dif_`i'			= abs(thr - income_`i')
			egen dif_min_`i'	= min(dif_`i')
			gen indic_`i'		= 0
			replace indic_`i'	= 1 if dif_min_`i' == dif_`i'

			local z	= `i'-1 // trick in case the x-tile coincide
			cap gen aux_`i'	= 1 if indic_`i' == indic_`z' &  indic_`z'==1
			cap replace indic_`i'	= 0 if aux_`i'	== 1
			cap replace indic_`i'	= 1 if aux_`i'[_n-1] == 1
				
			gen tax_`i'		=  0
			replace tax_`i'	= `tax_`i'' if indic_`i'==1
		}
		
		keep thr p tax*
		egen eff_tax_rate= rowtotal(tax_1 - tax_11)
		drop (tax_1 - tax_11)
	}
	
	else {
		import excel "$data/eff-tax-rate/eff-tax-rates-BRA.xlsx", ///
				sheet("`x'") cellrange(N25:O42) firstrow clear
								
		forvalues i = 1/17 {
			local income_`i' 	=  income[`i'] 
			local tax_`i'		=  eff_tax_rate[`i']
		}

		* import income distribution from gpinter 
		import excel "$data/gpinter_BRA_`x'.xlsx", ///
		sheet("BRA – equal split, `x'") cellrange(E1:F128) firstrow clear
		save "$data/gpinter_BRA_`x'.dta", replace
		
		forvalues i = 1/17 {
			
			gen income_`i' 	= `income_`i'' 
			gen dif_`i'			= abs(thr - income_`i')
			egen dif_min_`i'	= min(dif_`i')
			gen indic_`i'		= 0
			replace indic_`i'	= 1 if dif_min_`i' == dif_`i'

			local z	= `i'-1 // trick in case the x-tile coincide
			cap gen aux_`i'	= 1 if indic_`i' == indic_`z' &  indic_`z'==1
			cap replace indic_`i'	= 0 if aux_`i'	== 1
			cap replace indic_`i'	= 1 if aux_`i'[_n-1] == 1
				
			gen tax_`i'		=  0
			replace tax_`i'	= `tax_`i'' if indic_`i'==1
		}
		
				
		keep thr p tax*
		egen eff_tax_rate= rowtotal(tax_1 - tax_17)
		drop (tax_1 - tax_17)
		
	}
	
	

	* Interpolate the tax rates for the remaining p-tiles up to the last known
	replace eff_tax_rate = . if eff_tax_rate == 0 & [_n]!=1
	ipolate eff_tax_rate p, gen(eff_tax_rate_ipol) e
	
	// Create directory if it doesnt exist 
	local dirpath "$data/eff-tax-rate/"
	mata: st_numscalar("exists", direxists(st_local("dirpath")))
	if (scalar(exists) == 0) {
		mkdir "`dirpath'"
		display "Created directory: `dirpath'"
	}
		
	gen p_merge = round(p,.00001)
	
	save "$data/eff-tax-rate/BRA_effrates_`x'", replace
		
	
	//call graph parameters 
	global aux_part  ""graph_basics"" 
	do "code/Stata/auxiliar/aux_general.do"
	
	* plot
	form p %15.1fc
	twoway (connected eff_tax_rate_ipol p),  ///
			ytitle("Effective tax rate") ///
			xtitle("x-tiles") ///
			yline(100, lpattern(dash) lcolor(black*0.5)) ///
			ylabel(0(0.05)0.3, $ylab_opts) ///
			xlabel(0(0.1)1, $xlab_opts) ///
			$graph_scheme			
			graph export "$results/BRA_`x'.pdf", replace
	
	form p %15.3fc
	twoway (connected eff_tax_rate_ipol p if p>=0.99), ///
			ytitle("Effective tax rate") ///
			xtitle("x-tiles") ///
			yline(100, lpattern(dash) lcolor(black*0.5)) ///
			ylabel(0(0.05)0.3, $ylab_opts) ///
			xlabel(0.99(0.001)1, $xlab_opts) ///
			$graph_scheme 
			graph export "$results/BRA_`x'_top1.pdf", replace
			
	
}
