
clear all

//0. General settings ----------------------------------------------------------

//get list of paths 
global aux_part " "preliminary" " 
qui do "code/Stata/auxiliar/aux_general.do"

//cell range
qui local cellr "E11:G64"

*prepare to treat different sectors 
global sna_sectors households gg row 

forvalues t = $first_y / $last_y {
	
	tempfile tf_all_`t' 
	foreach s in $sna_sectors {
		
		*define file name 
		if "`s'" == "households" local filename "CSI_3.`t'_SC_S.1402.xlsx"
		if "`s'" == "gg" local filename "CSI_3.`t'_SC_S.1301.xlsx"
		if "`s'" == "row" local filename "CSI_3.`t'_SC_S.2000.xlsx"
		
		clear 
		qui cap import excel ///
			"input_data/sna_country_data/URY/`filename'", clear 
		
		di as result "Country SNA URY-`t' (`s'): " _continue 
		if _rc == 0 {
			di as text "found"
			*clean
			local vars E F G  
			qui keep `vars'
			qui rename (`vars') (code code_long `s')
			qui drop if missing(code) | `s' == "0"
			tempfile tf_op_`s'_`t'
			qui gen n = _n 
			foreach op in pagar cobrar {
				if "`op'" == "pagar" local x "u"
				if "`op'" == "cobrar" local x "r"
				preserve 
					qui keep if strpos(code, "B") | strpos(code_long, "por `op'") 
					if "`op'" == "cobrar" qui drop if strpos(code_long, "por pagar") 
					if "`op'" == "pagar" qui drop if strpos(code_long, "por cobrar") 
					qui destring `s', replace 
					qui rename `s' `s'_`x' 
					if "`op'" == "cobrar" qui merge m:m code ///
						using `tf_op_`s'_`t'', nogen
					qui replace code_long ///
						= subinstr(code_long, " por `op'", "", .)
					sort n 
					qui save `tf_op_`s'_`t'', replace 
				restore 
			}
			qui use `tf_op_`s'_`t'', clear 
			if "`s'" != "households" qui merge m:m code using `tf_all_`t'', nogen
			sort n 
			drop n 
			qui save `tf_all_`t'', replace 
			
		}
		else {
			di as error "not found"
		}
	}
	
	if _N != 0 {
		qui export excel using "input_data/sna_country_data/URY/cei.xlsx", ///
			sheet("`t'", replace) firstrow(variables) 
	}
}



