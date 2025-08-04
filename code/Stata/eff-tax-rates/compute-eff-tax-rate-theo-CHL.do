

clear all
global data "Data/Tax-data/CHL"
global results 	"figures/eff_tax_rates"

forvalues x = 2013/2017 {

	set trace on
	* import income distribution from gpinter 
	import excel "$data/gpinter_CHL_`x'.xlsx", ///
				sheet("gpinter_Chile_`x'") cellrange(E1:F128) firstrow clear
	
	local uta_2005 = 30610 *12 // all taken in June
	local uta_2006 = 31791 *12
	local uta_2007 = 35529 *12
	local uta_2008 = 35225 *12
	local uta_2009 = 36792 *12
	local uta_2010 = 37083 *12
	local uta_2011 = 38288 *12
	local uta_2012 = 39689 *12
	local uta_2013 = 40085 *12
	local uta_2014 = 42052 *12
	local uta_2015 = 43760 *12
	local uta_2016 = 45633 *12
	local uta_2017 = 46740 *12
	
	
	if `x' <= 2016 & `x' >= 2013 {
		local fir_rate = 0
		local sec_rate = 0.04
		local thi_rate = 0.08
		local fou_rate = 0.135
		local fif_rate = 0.23
		local six_rate = 0.304
		local sev_rate = 0.355
		local eig_rate = 0.4

		local fir_thr = 13.5
		local sec_thr = 30
		local thi_thr = 50
		local fou_thr = 70
		local fif_thr = 90
		local six_thr = 120
		local sev_thr = 150
		
		gen income = thr / `uta_`x''
		
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
				if income > `fou_thr' & income < `fif_thr'
		replace tax_paid = `fir_thr'*`fir_rate' + (`sec_thr'-`fir_rate')*`sec_rate' + 	///
				(`thi_thr'-`sec_thr')*`thi_rate' + (`fou_thr'-`thi_thr')*`fou_rate' + 	///
				(`fif_thr' - `fou_thr')*`fif_rate' + (income - `fif_thr')*`six_rate'											///
				if income > `fif_thr' & income < `six_thr'
		replace tax_paid = `fir_thr'*`fir_rate' + (`sec_thr'-`fir_rate')*`sec_rate' + 	///
				(`thi_thr'-`sec_thr')*`thi_rate' + (`fou_thr'-`thi_thr')*`fou_rate' + 	///
				(`fif_thr' - `fou_thr')*`fif_rate' + (`six_thr' - `fif_thr')*`six_rate'	+ ///								///							+  ///
				(income - `six_thr')*`sev_rate'											///
				if income > `six_thr' & income < `sev_thr'
		replace tax_paid = `fir_thr'*`fir_rate' + (`sec_thr'-`fir_rate')*`sec_rate' + 	///
				(`thi_thr'-`sec_thr')*`thi_rate' + (`fou_thr'-`thi_thr')*`fou_rate' + 	///
				(`fif_thr' - `fou_thr')*`fif_rate' + (`six_thr' - `fif_thr')*`six_rate'	+ ///								///							+  ///
				(`sev_thr' - `six_thr')*`sev_rate' + (income - `six_thr')*`eig_rate'											///
				if income > `sev_thr' 

	}

	
	if `x' == 2017 {
		local fir_rate = 0
		local sec_rate = 0.04
		local thi_rate = 0.08
		local fou_rate = 0.135
		local fif_rate = 0.23
		local six_rate = 0.304
		local sev_rate = 0.355

		local fir_thr = 13.5
		local sec_thr = 30
		local thi_thr = 50
		local fou_thr = 70
		local fif_thr = 90
		local six_thr = 120
		
		gen income = thr / `uta_`x''
		
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
				if income > `fou_thr' & income < `fif_thr'
		replace tax_paid = `fir_thr'*`fir_rate' + (`sec_thr'-`fir_rate')*`sec_rate' + 	///
				(`thi_thr'-`sec_thr')*`thi_rate' + (`fou_thr'-`thi_thr')*`fou_rate' + 	///
				(`fif_thr' - `fou_thr')*`fif_rate' + (income - `fif_thr')*`six_rate'											///
				if income > `fif_thr' & income < `six_thr'
		replace tax_paid = `fir_thr'*`fir_rate' + (`sec_thr'-`fir_rate')*`sec_rate' + 	///
				(`thi_thr'-`sec_thr')*`thi_rate' + (`fou_thr'-`thi_thr')*`fou_rate' + 	///
				(`fif_thr' - `fou_thr')*`fif_rate' + (`six_thr' - `fif_thr')*`six_rate'	+ ///								///							+  ///
				(income - `six_thr')*`sev_rate'											///
				if income > `six_thr' 

	}
	
	gen eff_tax_rate = 0
	replace eff_tax_rate = tax_paid / income
	
	* plot
	twoway (connected eff_tax_rate p), ytitle(Effective tax rate) ///
			xtitle(x-tiles)
			gr_edit .style.editstyle boxstyle(shadestyle(color(white))) editcopy
			gr_edit .style.editstyle boxstyle(linestyle(color(white))) editcopy
			gr_edit .yaxis1.reset_rule 0 .3 .05 , tickset(major) ruletype(range)
			graph export "$results/CHL_`x'_theo.pdf", replace
	twoway (connected eff_tax_rate p if p>0.99), ///
			ytitle(Effective tax rate) xtitle(x-tiles) 
			gr_edit .style.editstyle boxstyle(shadestyle(color(white))) editcopy
			gr_edit .style.editstyle boxstyle(linestyle(color(white))) editcopy
			gr_edit .yaxis1.reset_rule 0 .3 .05 , tickset(major) ruletype(range) 
			graph export "$results/CHL_`x'_top1_theo.pdf", replace
		
}	
		
