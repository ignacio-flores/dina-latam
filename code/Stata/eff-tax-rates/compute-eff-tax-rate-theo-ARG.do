
clear all
global data 	"Data/Tax-data/ARG"
global results 	"figures/eff_tax_rates"

forvalues x = 2003/2018 {  //  2003/2018
 
	* Calculate effective tax rates for known values
	use "$data/EstadisticasTributarias`x'/2.2.2.1.11.dta", clear 
	
	local year = `x' - 1
	
	gen aver_income = 0
	cap replace aver_income = (desde + desde[_n+1]) / 2
	replace aver_income = desde + desde/2 if _n == _N
	
	gen eff_tax_rate = impuesto / totinc_tax 
	
	forvalues i = 1/18 {
		local income_`i' 	= aver_income[`i'] 
		local tax_`i'		= eff_tax_rate[`i']
	}
	
	*save "$data/eff-tax-rate/eff-tax-rate-`year'", replace

	* import income distribution from gpinter 
	import excel "$data/gpinter_ARG_`year'.xlsx", ///
				sheet("ARG, `year'") cellrange(D1:G128) firstrow clear
				keep p topavg

				* Find the income p-tile for the effective tax rates I know			
	forvalues i = 1/18 {
		
		gen income_`i' 	= `income_`i'' 
		gen dif_`i'			= abs(topavg - income_`i')
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

	keep topavg p tax*
	egen eff_tax_rate= rowtotal(tax_1 - tax_18)
	drop (tax_1 - tax_18)

	* Interpolate the tax rates for the remaining p-tiles up to the last known
	replace eff_tax_rate = . if eff_tax_rate == 0 & [_n]!=1
	ipolate eff_tax_rate p, gen(eff_tax_rate_ipol)

	
	*Save data base
	save "$data/eff-tax-rate/ARG_`year'", replace
	
	
	* plot
	twoway (connected eff_tax_rate_ipol p), ytitle(Effective tax rate) ///
			xtitle(x-tiles)
			gr_edit .style.editstyle boxstyle(shadestyle(color(white))) editcopy
			gr_edit .style.editstyle boxstyle(linestyle(color(white))) editcopy
			gr_edit .yaxis1.reset_rule 0 .3 .05 , tickset(major) ruletype(range)
			graph export "$results/ARG_`year'.pdf", replace
	twoway (connected eff_tax_rate_ipol p if p>0.99), ///
			ytitle(Effective tax rate) xtitle(x-tiles) 
			gr_edit .style.editstyle boxstyle(shadestyle(color(white))) editcopy
			gr_edit .style.editstyle boxstyle(linestyle(color(white))) editcopy
			gr_edit .yaxis1.reset_rule 0 .3 .05 , tickset(major) ruletype(range) 
			graph export "$results/ARG_`year'_top1.pdf", replace
		

	
}

