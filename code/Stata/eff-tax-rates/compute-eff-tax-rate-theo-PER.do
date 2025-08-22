

clear all

global data_tax "input_data/admin_data/PER"
global data_sur "intermediary_data/microdata/raw/PER"
global results 	"output/figures/eff_tax_rates"

forvalues x = 2016/2017 {
	
	
	*import gpinter data
	import 	excel "$data_tax/gpinter_PER_`x'.xlsx", firstrow clear
	*cellrange(D1:H128) sheet("PER, `x'")	
	qui keep p thr topsh topavg bracketavg

	/*
	use "$data_sur/PRY_`x'_raw.dta", clear 
	keep if edad > 19 & edad != .
	* Income to calculate theoretical tax
	*/
	*Calculate theoretical tax rates

	local uit_2017 = 4050
	local uit_2016 = 3950

	* for tax data	
	cap gen income = topavg / `uit_`x''
	* for surveys
	cap gen income = y_fwag_svy_y / `uit_`x'' // annual formal wages in terms of min. wage

	local fir_rate = 0.08
	local sec_rate = 0.14
	local thi_rate = 0.17
	local fou_rate = 0.20
	local fif_rate = 0.30

	local fir_thr = 5
	local sec_thr = 20
	local thi_thr = 35
	local fou_thr = 45

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
			
	gen 	eff_tax_rate = 0
	replace eff_tax_rate = tax_paid / income
	
		*Save data base
	gen p_merge = round(p,.00001)
	gen eff_tax_rate_ipol = eff_tax_rate
	
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
			graph export "$results/PER_`x'.pdf", replace
				
	form p %15.3fc
	twoway (connected eff_tax_rate_ipol p if p>=0.99), ///
			ytitle("Effective tax rate") ///
			xtitle("x-tiles") ///
			yline(100, lpattern(dash) lcolor(black*0.5)) ///
			ylabel(0(0.05)0.3, $ylab_opts) ///
			xlabel(0.99(0.001)1, $xlab_opts) ///
			$graph_scheme 
			graph export "$results/PER_`x'_top1.pdf", replace

	qui replace p_merge = p_merge*10000
	qui duplicates drop p_merge, force
	qui drop if p_merge > 9999
	qui format p_merge %9.0g
	
	// Create directory if it doesnt exist 
	local dirpath "$data_tax/eff-tax-rate/"
	mata: st_numscalar("exists", direxists(st_local("dirpath")))
	if (scalar(exists) == 0) {
		mkdir "`dirpath'"
		display "Created directory: `dirpath'"
	} 
	
	qui save "$data_tax/eff-tax-rate/PER_effrates_`x'", replace
}
/*
	global c_bra "midgreen"
	global c_chl "cranberry"
	global c_col "gold"
*/
