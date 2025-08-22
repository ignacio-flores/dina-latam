


clear all
global data "input_data/admin_data/SLV"
global results 	"output/figures/eff_tax_rates"

forvalues x = 2000/2017 {
	clear
	import excel "$data/Tabulaciones_SLV.xls", ///
					sheet("`x'") cellrange(E9:L19) clear first
					rename TOTALRENTASGRAVADAS 	wages_inc
					rename IMPUESTOCOMPUTADO	wage_tax
					rename J					other_inc
					rename L					othinc_tax
					keep wages_inc wage_tax other_inc othinc_tax
	
	gen tot_inc 		= wages_inc + other_inc
	gen tax				= wage_tax + othinc_tax
	gen eff_tax_rate 	= tax / tot_inc
	
	
	gen tramo = _n
	gen thr = .
	replace thr = 0 if tramo==1
	replace thr = 2514 if tramo==2
	replace thr = 5000 if tramo==3
	replace thr = 15000 if tramo==4
	replace thr = 30000 if tramo==5
	replace thr = 60000 if tramo==6
	replace thr = 120000 if tramo==7
	replace thr = 150000 if tramo==8
	replace thr = 500000 if tramo==9
	replace thr = 1000000 if tramo==10
	
	gen aver_income = 0
	cap replace aver_income = (thr + thr[_n+1]) / 2
	cap replace aver_income = thr + thr/2 if tramo == 10
	drop tramo thr
	
	forvalues i = 1/10 {
		local income_`i' 	= aver_income[`i'] 
		local tax_`i'		= eff_tax_rate[`i']
	}
	

	* import income distribution from gpinter 
	cap confirm file "$data/diverse/diverse_SLV_`x'.xlsx"
	*cap confirm file "$data/wage_SLV_`x'.xlsx"
		if _rc==0 {
			import excel "$data/diverse/diverse_SLV_`x'.xlsx", ///
			cellrange(D1:G128) firstrow clear
			*import excel "$data/wage_SLV_`x'.xlsx", ///
			*cellrange(D1:G128) firstrow clear
		
			keep p topavg thr
				
			
				* Find the income p-tile for the effective tax rates I know			
			forvalues i = 1/10 {
				
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

		keep topavg p thr tax* 
		egen eff_tax_rate= rowtotal(tax_1 - tax_10)
		drop (tax_1 - tax_10)

		replace eff_tax_rate = 0 if thr <= 0 // so that simulated effective tax rat starts above thr > 0
		
		* Interpolate the tax rates for the remaining p-tiles up to the last known
		replace eff_tax_rate = . if eff_tax_rate == 0 & [_n]!=1
		replace eff_tax_rate = 0 if thr <= 0 // again, so it starts ipolate from thr > 0

		ipolate eff_tax_rate p, gen(eff_tax_rate_ipol) e

		*Save data base
		gen p_merge = round(p,.00001)	
		
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
				graph export "$results/SLV_`x'.pdf", replace
			
				
		form p %15.3fc
		twoway (connected eff_tax_rate_ipol p if p>=0.99), ///
				ytitle("Effective tax rate") ///
				xtitle("x-tiles") ///
				yline(100, lpattern(dash) lcolor(black*0.5)) ///
				ylabel(0(0.05)0.3, $ylab_opts) ///
				xlabel(0.99(0.001)1, $xlab_opts) ///
				$graph_scheme 
				graph export "$results/SLV_`x'_top1.pdf", replace

				
		}

		else {
			continue	
		}

	
	qui replace p_merge = p_merge*10000
	qui duplicates drop p_merge, force
	qui drop if p_merge > 9999
	qui format p_merge %9.0g
	
	// Create directory if it doesnt exist 
	local dirpath "$data/eff-tax-rate/"
	mata: st_numscalar("exists", direxists(st_local("dirpath")))
	if (scalar(exists) == 0) {
		mkdir "`dirpath'"
		display "Created directory: `dirpath'"
	}
	
	save "$data/eff-tax-rate/SLV_effrates_`x'", replace
					
}

