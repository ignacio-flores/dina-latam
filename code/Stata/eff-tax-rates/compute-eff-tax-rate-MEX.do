
	clear all
	global data "Data/Tax-data/MEX"
	global results "figures/eff_tax_rates"
	global codes "code/Do-files"

	set matsize 1200

*forvalues i = 0/3 {
	quietly forvalues t = 2009/2014 { // 2009
		
		if !inlist(`t',2009,2011,2013) {
			
			
			use "Data/CEPAL/surveys/MEX/raw/MEX_`t'_raw.dta", clear
							
				sum _fep   
				scalar totalpop = r(sum)	

			local taxfile "$data/Database_taxfiles/Mexico`t'.dta" // define location of tax data (one file per year)

			use `taxfile', clear
			
			qui include "$codes/auxiliar/auxiliar_MEX.do"
			
			keep 	de_total_de_ing_acumulables de_isr_causado	income*
			
				cap drop income 
				gen income = .
				replace 	income = income_3  // "income" 
				rename 	de_isr_causado	tax
				*drop if income == 0
			
				gen eff_tax_rate = tax / income
				replace eff_tax_rate = 0 if eff_tax_rate == .
				replace eff_tax_rate = 0.3 if eff_tax_rate > 0.3
			
				sum eff_tax_rate, d
				
				tempvar weight ftile freq F fy cumfy L d_eq bckt_size cum_weight wy	wt
			
				
				// Estimate Gini and keep in memory
				cap drop weight poptot freq F
				gsort 		-income
				gen weight 	= 1
				gen poptot 	= totalpop
				gen freq 	= weight / poptot
				gen F 		= 1-sum(freq)
				
				// Classify obs in g-percentiles
				gsort -F
				sort F
				cap drop ftile
				egen ftile = cut(F), at(0.9(0.01)0.99 0.991(0.001)0.999 0.9991(0.0001)0.9999 0.99991(0.00001)0.99999 1)
				
				// Average tax rate
				gsort -F
				collapse eff_tax_rate [w=weight], by (ftile)
				
				
				// Interval thresholds		
				drop if ftile==.
				sort ftile
									
				// Year
				gen year = `t' in 1
					
				// Order and save	
				order year ftile eff_tax_rate
				keep year  ftile eff_tax_rate	
				
				set obs 127
				replace ftile = 0 if ftile == .
				sort ftile
				replace ftile = (_n-1)/100 if ftile == 0
				replace eff_tax_rate = 0 if eff_tax_rate == .
				
				gen eff_tax_rate_ipol = eff_tax_rate
				gen p_merge = round(ftile,.00001)
				save "$data/eff-tax-rate/MEX_effrates_`t'", replace 
				
			}			
			
				if !inlist(`t',2010,2012,2014) {
				
				
				//Import adult population data into tabulations
				use "Data/Population/PopulationLatAm.dta", clear
				mkmat year totalpop adultpop, matrix(_mat_sum)

				scalar totalpop2009=_mat_sum[795, 2]
				scalar totalpop2010=_mat_sum[796, 2]
				scalar totalpop2011=_mat_sum[797, 2]
				scalar totalpop2012=_mat_sum[798, 2]
				scalar totalpop2013=_mat_sum[799, 2]
				scalar totalpop2014=_mat_sum[800, 2]

				local taxfile "$data/Database_taxfiles/Mexico`t'.dta" // define location of tax data (one file per year)

				use `taxfile', clear
				qui include "$codes/auxiliar/auxiliar_MEX.do"
				keep 	de_total_de_ing_acumulables de_isr_causado	income*
				
				cap drop income 
				gen income = .
				replace 	income = income_3 // "income" 
				rename 	de_isr_causado	tax
				*drop if income == 0
			
				gen eff_tax_rate = tax / income
				replace eff_tax_rate = 0 if eff_tax_rate == .
				replace eff_tax_rate = 0.3 if eff_tax_rate > 0.3
			
				sum eff_tax_rate, d
				
				tempvar weight ftile freq F fy cumfy L d_eq bckt_size cum_weight wy	wt
			
				
				// Estimate Gini and keep in memory
				cap drop weight poptot freq F
				gsort -income
				gen weight 	= 1
				gen poptot = totalpop`t'
				gen freq 	= weight / poptot
				gen F 		= 1-sum(freq)
				
				// Classify obs in g-percentiles
				gsort -F
				sort F
				cap drop ftile
				egen ftile = cut(F), at(0.9(0.01)0.99 0.991(0.001)0.999 0.9991(0.0001)0.9999 0.99991(0.00001)0.99999 1)
				
				// Average tax rate
				gsort -F
				collapse eff_tax_rate [w=weight], by (ftile)
				
				
				// Interval thresholds		
				drop if ftile==.
				sort ftile
									
				// Year
				gen year = `t' in 1
					
				// Order and save	
				order year ftile eff_tax_rate
				keep year  ftile eff_tax_rate	
				
				set obs 127
				replace ftile = 0 if ftile == .
				sort ftile
				replace ftile = (_n-1)/100 if ftile == 0
				replace eff_tax_rate = 0 if eff_tax_rate == .

				gen eff_tax_rate_ipol = eff_tax_rate
				gen p_merge = round(ftile,.00001)
				save "$data/eff-tax-rate/MEX_effrates_`t'", replace 
				
			}
			
			//call graph parameters 
			global aux_part  ""graph_basics"" 
			do "code/Do-files/auxiliar/aux_general.do"
					
			* plot
			form p %15.1fc
			twoway (connected eff_tax_rate_ipol ftile),  ///
					ytitle("Effective tax rate") ///
					xtitle("x-tiles") ///
					yline(100, lpattern(dash) lcolor(black*0.5)) ///
					ylabel(0(0.05)0.3, $ylab_opts) ///
					xlabel(0(0.1)1, $xlab_opts) ///
					$graph_scheme			
					graph export "$results/MEX_`t'.pdf", replace
					
			form p %15.3fc
			twoway (connected eff_tax_rate_ipol ftile if p>=0.99), ///
					ytitle("Effective tax rate") ///
					xtitle("x-tiles") ///
					yline(100, lpattern(dash) lcolor(black*0.5)) ///
					ylabel(0(0.05)0.3, $ylab_opts) ///
					xlabel(0.99(0.001)1, $xlab_opts) ///
					$graph_scheme 
					graph export "$results/MEX_`t'_top1.pdf", replace

		
		
	}
*}
		
		
		
