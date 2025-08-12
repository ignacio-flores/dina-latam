

clear all
global data "input_data/admin_data/CRI"
global results 	"output/figures/eff_tax_rates"

// Calculate effective tax rates for diverse income
*bracketavg

qui forvalues year = 2010/2016 {
	tempfile tax_`year'

	// (1) taxes paid (from microdata) 
	global excel "$data/eff-tax-rate/BFM_CR_`year'_mix_tax.xlsx"
	qui cap erase "$data/eff-tax-rate/BFM_CR_`year'_mix_tax_1.dta" 
	qui xls2dta, sheet("Sheet1") save($data/eff-tax-rate) : /// //  allsheets
		import excel $excel ,  firstrow cellrange(A1)
		qui u "$data/eff-tax-rate/BFM_CR_`year'_mix_tax_1.dta", clear
			qui rename bckt_avg tax_bckt_avg
			qui rename thr 		tax_thr

			qui replace tax_bckt_avg 	= tax_bckt_avg 	// /13
			qui replace tax_thr 		= tax_thr 		// /13
			
		qui save `tax_`year''
		qui cap erase "$data/eff-tax-rate/BFM_CR_`year'_mix_tax_1.dta" 

	// (2) gpinter (from microdata)
	
	global excel "$data/diverse_CRI_`year'_mix_income.xlsx"
	qui cap erase "$data/diverse_CRI_`year'_mix_income_1.dta" 
	qui xls2dta, sheet("Sheet1") save($data) : /// //  allsheets
		import excel $excel ,  firstrow cellrange(A1)
		qui u "$data/diverse_CRI_`year'_mix_income_1.dta", clear
		qui cap erase "$data/diverse_CRI_`year'_mix_income_1.dta"
		
	/*
	global excel "$data/BFM_CR_`year'_mix_tax.xlsx"
	qui cap erase "$data/BFM_CR_`year'_mix_tax_1.dta" 
	qui xls2dta, sheet("Sheet1") save($data) : /// //  allsheets
		import excel $excel ,  firstrow cellrange(A1)
		qui u "$data/BFM_CR_`year'_mix_tax_1.dta", clear
		qui cap erase "$data/BFM_CR_`year'_mix_tax_1.dta"
	*/
	
	
	// (3) merge both and check all g-tiles merge
	qui merge 1:1 p using `tax_`year''
	qui sum p
	local num_frac = r(N)
	qui sum _merge if _merge == 3
	assert `num_frac' == r(N)


	// (4) calculate effective tax rates and replace missing with 0 for the 127 g-tiles
	qui gen eff_tax_rate_ipol = tax_bckt_avg / bracketavg //bckt_avg
	qui mvencode eff_tax_rate_ipol, mv(0) override
	*qui gen eff_tax_rate_ipol = tax_thr / thr

	set obs 127
	order year p eff_tax_rate_ipol
	keep  year p eff_tax_rate_ipol	
			
	replace p = 0 if p == .
	sort p
	replace p = (_n-1)/100 if p == 0
	replace eff_tax_rate_ipol = 0 if eff_tax_rate_ipol == .	
	
	*Save data base
	gen p_merge = round(p,.00001)
	assert !missing(eff_tax_rate_ipol)
	
	
	
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
			graph export "$results/CRI_`year'.pdf", replace
	
	form p %15.3fc
	twoway (connected eff_tax_rate_ipol p if p>=0.99), ///
			ytitle("Effective tax rate") ///
			xtitle("x-tiles") ///
			yline(100, lpattern(dash) lcolor(black*0.5)) ///
			ylabel(0(0.05)0.3, $ylab_opts) ///
			xlabel(0.99(0.001)1, $xlab_opts) ///
			$graph_scheme 
			graph export "$results/CRI_`x'_top1.pdf", replace

	qui replace p_merge = p_merge * 10000
	qui duplicates drop p_merge, force
	qui drop if p_merge > 9999
	qui format p_merge %9.0g
	save "$data/eff-tax-rate/CRI_effrates_`year'", replace
}

