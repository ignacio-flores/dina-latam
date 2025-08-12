

clear all
global data "input_data/admin_data/CHL"
global results 	"output/figures/eff_tax_rates"

forvalues x = 2005/2022 {
	
	* import effective tax rates for known points
	import excel "$data/eff-tax-rate/eff_tax_rate_CHL.xlsx", ///
		sheet("`x'") cellrange(B2:D10) firstrow clear
	drop Averagetax
	rename Averageincome av_income
	rename Averagetaxrate eff_tax_rate			
	
	forvalues i = 1/8 {
		local income_`i' = av_income[`i'] 
		local tax_`i' =  eff_tax_rate[`i']
	}

	* import income distribution from gpinter 
	import excel "$data/gpinter_output/total-pre-CHL.xlsx", ///
		sheet("pretax, CHL, `x'") cellrange(E1:I128) firstrow clear
		
	* Find the income p-tile for the effective tax rates I know			
	forvalues i = 1/8 {
		
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
	egen eff_tax_rate= rowtotal(tax_1 - tax_8)
	drop (tax_1 - tax_8)


	* Interpolate the tax rates for the remaining p-tiles up to the last known
	replace eff_tax_rate = . if eff_tax_rate == 0 & [_n]!=1 & p>0.8 // impone el 0.8 porque abajo no hay nada
	ipolate eff_tax_rate p, gen(eff_tax_rate_ipol) e
	*gen eff_tax_rate_ipol = eff_tax_rate
	
	*Save data base
	gen p_merge = round(p,.00001)
	
	//call graph parameters 
	global aux_part  ""graph_basics"" 
	do "code/Do-files/auxiliar/aux_general.do"
	
	* plot
	form p %15.1fc
	twoway (connected eff_tax_rate_ipol p),  ///
			ytitle("Effective tax rate") ///
			xtitle("x-tiles") ///
			yline(100, lpattern(dash) lcolor(black*0.5)) ///
			ylabel(0(0.05)0.3, $ylab_opts) ///
			xlabel(0(0.1)1, $xlab_opts) ///
			$graph_scheme			
			*graph export "$results/CHL_`x'.pdf", replace
		
	
	form p %15.3fc
	twoway (connected eff_tax_rate_ipol p if p>=0.99), ///
			ytitle("Effective tax rate") ///
			xtitle("x-tiles") ///
			yline(100, lpattern(dash) lcolor(black*0.5)) ///
			ylabel(0(0.05)0.3, $ylab_opts) ///
			xlabel(0.99(0.001)1, $xlab_opts) ///
			$graph_scheme 
			*graph export "$results/CHL_`x'_top1.pdf", replace

	qui replace p_merge = p_merge*10000
	qui duplicates drop p_merge, force
	qui drop if p_merge > 9999
	qui format p_merge %9.0g
	qui	save "$data/eff-tax-rate/CHL_effrates_`x'", replace

}	
		
