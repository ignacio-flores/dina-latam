
//preliminary
global aux_part  ""preliminary"" 
quietly do "code/Stata/auxiliar/aux_general.do"

local lasty_col_tax = 2023
forvalues y = 2014/`lasty_col_tax' {
	
	*remember total population 
	di as result "`y' ", _continue
	qui use "intermediary_data/population/SurveyPop.dta", clear
	qui sum totpop_ie if country == "COL" & year == `y'
	local totalpop = r(sum)
	
	*open tax data 
	local n = `y' - 2007	
	global route ///
		"input_data/admin_data/COL/1_Cuantiles_Ingreso_Bruto_Naturales_2014-`lasty_col_tax'"
	global fil "`n'_Cuantiles_Ingreso_Bruto_Naturales_`y'_F-210"
	local sn 
	if `y' == 2014 {
		local cr A9:BY990
	}
	if `y' == 2015 local cr A9:BY992
	if `y' == 2016 local cr A9:BY991
	if `y' == 2017 local cr A9:CC995
	if `y' == 2018 local cr A9:CC996
	if `y' == 2019 local cr A9:CG997
	if `y' == 2019 local cr A9:CG997
	if `y' == 2020 local cr A9:DL998
	if `y' == 2021 local cr A9:DL998
	if `y' == 2022 {
		local cr B14:DM1002
		local sn "Ag cuantiles ingreso bruto"
	} 
	if `y' == 2023 {
		local cr B14:DM999
		local sn "Ag cuantiles ingreso bruto"
	} 
	di as result "`cr' ", _continue
	di as result "pop: " `totalpop'
	
	qui import excel "${route}/${fil}.xlsx", clear cellrange("`cr'") firstrow sheet("`sn'")
	
	*clean 
	qui drop if missing(Númerodecasos)
	qui drop if missing(Cuantil)
	qui rename Númerodecasos n
	if (`y' < 2017) qui rename Totalingresosrecibidosporcon pre_inc
	if (`y' >= 2017) qui rename Totalingresosbrutos1 pre_inc
	qui rename Impuestonetoderenta net_tax 
	qui keep n pre_inc net_tax
	
	*transform to pesos 
	format %20.0gc pre_inc
	format %20.0gc net_tax
	
	qui replace pre_inc = (pre_inc * 1000000) / n // promedio por bracket en millones de pesos
	qui gen pos_inc = pre_inc - (net_tax * 1000000) / n // promedio por bracket en millones de pesos
	qui gen eff_tax_rate = (pre_inc-pos_inc) / pre_inc
	
	foreach v in /*pre*/ pos {
		cap drop __*
	
		tempvar F freq wy weight poptot inc_avg topavg 
	
		qui gen `weight' = n
		qui set obs `=_N+1'
		
		qui replace `weight' = `totalpop' - `_N' - 1 if missing(n)
		qui replace pre_inc = . if missing(n)
	
		
		// Total average
		qui gen `poptot' = `totalpop'
		qui sum `v'_inc [w=`weight']
		local inc_tot = r(sum)
		qui gen `inc_avg' = `inc_tot'/`poptot' 
		gsort -`v'_inc
		qui	gen `freq' = `weight'/`poptot'
		qui	gen `F' = 1- sum(`freq')
		sort `v'_inc

		gsort -`F'
		sort `F'
		cap drop ftile
		qui egen ftile = cut(`F'), at(0(0.01)0.99 0.991(0.001)0.999 ///
			0.9991(0.0001)0.9999 0.99991(0.00001)0.99999 1)
			
		// Top average 
		gsort -`F'
		qui gen `wy' = `v'_inc*`weight'
		qui gen `topavg' = sum(`wy')/sum(`weight')
		sort `F'
		
		preserve
			qui replace `v' = `v' / 1000000
			qui drop if `v' < 0 | missing(n)
			// Interval thresholds
			qui collapse ///
				(min) poptot = `poptot' thr = `v'_inc topavg = `topavg'  ///
				(mean) bracketavg = `v'_inc average = `inc_avg' eff_tax_rate ///
				[w=`weight'], by (ftile)
			foreach vari in thr bracketavg topavg {
				format %20.0gc `vari'
				qui replace `vari' = `vari' * 1000000
			}	
			sort ftile 	
			qui replace ftile = round(ftile * 100000)
			qui gen p = ftile / 100000
			qui drop ftile
			sort thr
			qui drop if thr == 0 
			assert bracketavg > bracketavg[_n-1] if !missing(bracketavg[_n-1])

		
			qui drop if missing(bracketavg) 
			qui drop if bracketavg == thr
			qui drop if thr < 1000000
			
			qui gen year = `y' in 1 
			qui gen country = "COL" in 1 
			
			// Create directory if it doesnt exist 
			local dirpath "input_data/admin_data/COL/_clean"
			mata: st_numscalar("exists", direxists(st_local("dirpath")))
			if (scalar(exists) == 0) {
				mkdir "`dirpath'"
				display "Created directory: `dirpath'"
			}	
			
			qui export excel using ///
				"input_data/admin_data/COL/_clean/total-`v'-COL.xlsx", ///
				firstrow(			variables)  sheet("`y'", modify) 
		restore
		
	}

}


