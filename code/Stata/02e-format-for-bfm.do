do "code/Stata/auxiliar/dina_runtime_config.do"
capture confirm file "output/data_reports/admin_gpinter_countries.csv"
if _rc {
    di as error "Admin gpinter country handoff is missing; run 02d5-interpolate-admin-data."
    exit 601
}
quietly import delimited using "output/data_reports/admin_gpinter_countries.csv", clear varnames(1)
quietly levelsof country if status == "excluded", local(gpinter_excluded) clean
clear
local list_noquotes : subinstr global all_countries `"""' "" , all
foreach c in "SLV" "PER" "DOM" "URY" "BRA" "CHL" "COL" "ECU" {
	
	if strpos("`list_noquotes'", "`c'") > 0 {
		if strpos(" `gpinter_excluded' ", " `c' ") > 0 {
			di as result "Skipping Admin gpinter for `c' (explicit exclusion)."
			continue
		}
		
		*define locals for each country 
		local vars year country component average
		local component "postax"
		if inlist("`c'" , "BRA") local component "pretax"
		local short = substr("`component'", 1, 3)
		
		//CHECK IF GPINTER HAS BEEN APPLIED 
		if "`c'" != "URY" {
		    local dirpath "input_data/admin_data/`c'/gpinter_output/"
			mata: st_numscalar("exists", direxists(st_local("dirpath")))
			if (scalar(exists) == 0) {
				di as error "Directory not found input_data/admin_data/`c'/gpinter_output/"
				di as error "Run 02d5-interpolate-admin-data to generate the workbook."
				exit 1 
			}
		}
		
		*store worksheet names 
		if "`c'" != "URY" {
			capture confirm file "input_data/admin_data/`c'/gpinter_output/total-`short'-`c'.xlsx"
			if _rc {
				di as error "Admin gpinter output workbook missing for `c'."
				exit 601
			}
		}
		cap import excel using ///
			"input_data/admin_data/`c'/gpinter_output/total-`short'-`c'.xlsx", describe		
		if _rc == 0 {
			local nws_`c' = r(N_worksheet) 
			forvalues x = 1/`nws_`c'' {
				local nam = r(worksheet_`x')
				local nam2 =  regexm("`nam'", "([0-9]+)")
				if `nam2' {
					local num_part : display regexs(1)
					local sheet_`c'_`num_part' `nam'
					*di as result "sheet `c'-`num_part': `sheet_`c'_`num_part''"
				}
			}
		
		}
		
		*loop over years 
		forvalues t = $first_y/$last_y {
		    
			if ("`c'" == "SLV" & `t' == 2000) continue 
			
			clear
			*import data 
			if "`c'" == "URY" {
				cap import excel using ///
					"input_data/admin_data/`c'/gpinter_`c'_`t'.xlsx", firstrow
			}
			else{
				if "`sheet_`c'_`t''" != "" {
					qui import excel using ///
						"input_data/admin_data/`c'/gpinter_output/total-`short'-`c'.xlsx", /// 
						sheet("`sheet_`c'_`t''") firstrow clear	
				}	
			}
			*check if file exists...	
			qui cap assert _N == 0
			if _rc != 0 {		
				
				*get rid of thr 0
				if "`c'" == "URY" {
					cap confirm string var thr
					if _rc==0 {
						qui replace thr = subinstr(thr, ",", ".", .)
						di as result "`c' `t'"
						destring thr, replace 
					}
					qui drop if thr == 0 
					*collapse doubled thr
					quietly collapse (min) p ///
					(mean) bracketavg male female _* Miss* *inc ///
					 (firstnm) average totalpop (max) topavg, by(thr) 
				}
				else {
					foreach v in `vars' {
						cap replace `v' = `v'[1]
					}
					qui drop if thr == 0
					foreach v in `vars' {
						if "`v'" != "average" cap replace `v' = "" if _n != 1 
						else cap replace `v' = . if _n != 1 
					}
				}
				
				if "`c'" == "COL" {
					qui replace thr = round(thr) if thr < 1 
				}
				
				*check increasing vakues 
				qui gen n = _n / 100
				qui gen tester = thr - thr[_n-1] if p > .5
				assert tester >= -1e-12
				qui replace thr = thr + n if tester == 0 
				qui gen tester2 = thr - thr[_n-1] if p > .5
				assert tester2 > -1e-12 
				qui drop n tester tester2
				
				* URY is calculated directly from Admin microdata. Its workbooks are
				* already BFM-ready; rewriting them changes their source checksums.
				if "`c'" != "URY" {
					export excel using "input_data/admin_data/`c'/gpinter_`c'_`t'.xlsx", ///
						firstrow(variables) keepcellfmt replace
				}
			}
			else {
				di as error "input_data/admin_data/`c'/gpinter_output/total-`short'-`c'.xlsx, sheet(`component', `c', `t') not found"
			}
		}
	}	
}
