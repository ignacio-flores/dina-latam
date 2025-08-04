
clear all

global data_svy 	"Data/CEPAL/surveys/PRY/raw"
global data_tax 	"Data/Tax-data/PRY/eff-tax-rate"
global results 		"figures/eff_tax_rates"

forvalues x = 2012/2015 {  
	
	* Survey data
	use "$data_svy/PRY_`x'_raw.dta", clear 
	keep if edad > 19 & edad != .

	* Min. wage (used for thresholds in Paraguay tax system)
	local sm_2012 = 1658232
	local sm_2013 = 1658232
	local sm_2014 = 1824055
	local sm_2015 = 1824055
	local sm_2016 = 1824055
	local sm_2017 = 1964507
	
	* Income to calculate theoretical tax
	gen income = y_fwag_svy_y / `sm_`x'' // annual formal wages in terms of min. wage
	
	
	* Effective tax rate
	if `x' == 2012 {
		* Thresholds and marginal tax rates
		local fir_rate = 0
		local sec_rate = 0.1

		local fir_thr = 120

		* Tax 
		cap drop tax_paid
		gen 	tax_paid = 0
		replace tax_paid = income*`fir_rate' 											///
				if income < `fir_thr'
		replace tax_paid = `fir_thr'*`fir_rate' +  (income-`fir_thr')*`sec_rate' 		///
				if income > `fir_thr' 

	}
	else {
		* Thresholds and marginal tax rates
		local fir_rate = 0
		local sec_rate = 0.08
		local thi_rate = 0.1

		local fir_thr = 108
		local sec_thr = 120

		* Tax 
		cap drop tax_paid
		gen 	tax_paid = 0
		replace tax_paid = income*`fir_rate' 											///
				if income < `fir_thr'
		replace tax_paid = `fir_thr'*`fir_rate' +  (income-`fir_thr')*`sec_rate' 		///
				if income > `fir_thr' & income < `sec_thr'
		replace tax_paid = `fir_thr'*`fir_rate' + (`sec_thr'-`fir_rate')*`sec_rate' + 	///
				(income-`sec_thr')*`thi_rate'  if income > `sec_thr' 
	}
	* Effective tax rate
	gen 	eff_tax_rate = 0
	replace eff_tax_rate = tax_paid / income
	
	/*
	*expand _fep
	cap drop fractiles ftile
	xtile fractiles = y_tot_svy_y [fw=_fep], n(1000)
	replace fractiles = fractiles / 1000
	egen ftile = cut(fractiles), at(0(0.01)0.99 0.991(0.001)1)
	*egen ftile = cut(F), at(0(0.01)0.99 0.991(0.001)0.999 0.9991(0.0001)0.9999 0.99991(0.00001)0.99999 1)
	*/
	
	
	expand _fep
	keep if edad > 19 & edad != .
	cap drop fractiles ftile
	sort income
	gen fractiles = 0
	
	recast float fractiles
	*format fractiles %8.5g
	
	cap drop aux
	xtile aux = income , n(100)
	replace fractiles = aux / 100

	
	cap drop aux
	xtile aux = income  ///	
		if fractiles == 1, n(10)
	replace fractiles = 0.99 + ((aux) / 1000) ///
		if fractiles == 1

		
	cap drop aux
	xtile aux = income  ///	
		if fractiles == 1, n(10)
	replace fractiles = 0.999 + ((aux) / 10000) ///
		if fractiles == 1

	
	cap drop aux
	cap xtile aux = income  ///	
		if fractiles == 1, n(10)
	cap replace fractiles = 0.9999 + ((aux) / 100000) ///
		if fractiles == 1
	
	gen p_merge = round(fractiles, .00001)
	
	
	collapse eff_tax_rate , by(fractiles)
	
	* Plot
	/*
	twoway (connected eff_tax_rate ftile), ytitle(Effective tax rate) ///
			xtitle(x-tiles)
			gr_edit .style.editstyle boxstyle(shadestyle(color(white))) editcopy
			gr_edit .style.editstyle boxstyle(linestyle(color(white))) editcopy
			gr_edit .yaxis1.reset_rule 0 .3 .05 , tickset(major) ruletype(range)
			graph export "$results/PRY_`x'.pdf", replace
	*/
	gen p_merge = fractiles
	gen eff_tax_rate_ipol = eff_tax_rate	
	save "$data_tax/PRY_effrates_`x'", replace
		
	twoway (connected eff_tax_rate fractiles if fractiles>0.99), ///
			ytitle(Effective tax rate) xtitle(x-tiles) 
			gr_edit .style.editstyle boxstyle(shadestyle(color(white))) editcopy
			gr_edit .style.editstyle boxstyle(linestyle(color(white))) editcopy
			gr_edit .yaxis1.reset_rule 0 .3 .05 , tickset(major) ruletype(range) 
			graph export "$results/PRY_`x'_top1.pdf", replace
		

	
}

