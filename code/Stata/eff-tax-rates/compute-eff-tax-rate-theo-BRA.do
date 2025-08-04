

clear all
global data 	"Data/Tax-data/BRA"
global data_s 	"Data/CEPAL/surveys/BRA/raw"
global results 	"figures/eff_tax_rates"

local j=0
forvalues x = 2015/2016 {
local j=`j'+1

		/*
		* With tax data
		use "$data/gpinter_BRA_`x'.dta", replace
		*/
		
	use "$data_s/BRA_`x'_raw.dta", clear 
	keep if edad > 19 & edad != .
	gen income = y_fwag_svy_y
	cap drop fractiles ftile
	xtile fractiles = y_tot_svy_y [fw=_fep], n(1000)
	replace fractiles = fractiles / 1000
	egen ftile = cut(fractiles), at(0(0.01)0.99 0.991(0.001)1)
	

		
	local fir_rate = 0
	local sec_rate = 0.075
	local thi_rate = 0.15
	local fou_rate = 0.225
	local fif_rate = 0.275

	
	if `x' == 2015 {
		local fir_thr = 22499
		local sec_thr = 33477
		local thi_thr = 44476
		local fou_thr = 55373
	}
	
	if `x' == 2016 {
		local fir_thr = 22847
		local sec_thr = 33919
		local thi_thr = 45012
		local fou_thr = 55976
	}		
		
	*gen income = thr 
		
	gen 	tax_paid = 0
	replace tax_paid = income*`fir_rate' 											///
			if income < `fir_thr'
	replace tax_paid = `fir_thr'*`fir_rate' +  (income-`fir_thr')*`sec_rate' 		///
			if income > `fir_thr' & income < `sec_thr'
	replace tax_paid = `fir_thr'*`fir_rate' + (`sec_thr'-`fir_rate')*`sec_rate' + 	///
			(income-`sec_thr')*`thi_rate' 											///
			if income > `sec_thr' & income < `thi_thr'
	replace tax_paid = `fir_thr'*`fir_rate' + (`sec_thr'-`fir_rate')*`sec_rate' + 	///
			(`thi_thr'-`sec_thr')*`thi_rate' + (income-`thi_thr')*`fou_rate' 		///
			if income > `thi_thr' & income < `fou_thr'
	replace tax_paid = `fir_thr'*`fir_rate' + (`sec_thr'-`fir_rate')*`sec_rate' + 	///
			(`thi_thr'-`sec_thr')*`thi_rate' + (`fou_thr'-`thi_thr')*`fou_rate' + 	///
			(income - `fou_thr')*`fif_rate'											///
			if income > `fou_thr' 

	
	gen eff_tax_rate = 0
	replace eff_tax_rate = tax_paid / income
	collapse eff_tax_rate [fw=_fep], by(ftile)
	gen p=ftile

	*save "$data/eff-tax-rates/eff-tax-rates-BRA_`x'.dta", replace
	
	twoway (connected eff_tax_rate p), ytitle(Effective tax rate) ///
			xtitle(x-tiles)
			gr_edit .style.editstyle boxstyle(shadestyle(color(white))) editcopy
			gr_edit .style.editstyle boxstyle(linestyle(color(white))) editcopy
			gr_edit .yaxis1.reset_rule 0 .3 .05 , tickset(major) ruletype(range)
			graph export "$results/BRA_`x'_theo.pdf", replace
			
	twoway (connected eff_tax_rate p if p>0.99), ///
			ytitle(Effective tax rate) xtitle(x-tiles) 
			gr_edit .style.editstyle boxstyle(shadestyle(color(white))) editcopy
			gr_edit .style.editstyle boxstyle(linestyle(color(white))) editcopy
			gr_edit .yaxis1.reset_rule 0 .3 .05 , tickset(major) ruletype(range) 
			graph export "$results/BRA_`x'_top1_theo.pdf", replace
			
	
}
