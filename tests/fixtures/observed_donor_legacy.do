// Frozen reference: the donor arithmetic used by 03d before stable capture.
// Kept only for isolated numerical equivalence tests; never a pipeline entry.
capture program drop legacy_observed_donor
program define legacy_observed_donor
    syntax, COUNTRY(string) YEAR(integer) SAVING(string)
    local c "`country'"
    local t = `year'
    local raw_weight "_fep"
    if "`c'" == "ARG" local raw_weight "n_fep"
    local cor_weight "_weight"
    local y "ind_pre_tot"
    if "`c'" == "ARG" local y "ind_pos_prwag"
    tempvar freq F
		//check consistency of raw / corrected weights
		foreach type2 in "raw" "cor" {
			qui sum ``type2'_weight', meanonly 
			local poptot_`type2' = r(sum)
		}
		local ratio = round(`poptot_cor' / `poptot_raw' * 100)
		cap assert `ratio' == 100
		if (_rc != 0) {
			*qui use "intermediary_data/microdata/raw/`c'/`c'_`t'_raw.dta", clear 
			*qui sum _fep, meanonly 
			*local actual_original_pop = r(sum)
			di as error "Corrected population from BFM is `ratio'% the original"
			di as error "original: `poptot_raw', corrected: `poptot_cor'"
			di as error "if you are using BFM with replace option on, only use the original survey-weight variable from the raw version"
			exit 1
		}
		
		//compute cumulative distribution 
		sort `y' _pid 
		quietly	gen `freq' = `raw_weight'/`poptot_raw'
		quietly	gen `F' = sum(`freq')	
			
		//classify obs in 127 g-percentiles
		cap drop ftile 
		qui egen ftile = cut(`F'), ///
			at(0(0.01)0.99 0.991(0.001)0.999 0.9991(0.0001)0.9999 ///
			0.99991(0.00001)0.99999 1)
		qui replace ftile = 0.99999 if missing(ftile)	
			
		//aggregate to fractile level	
		qui collapse (sum) `raw_weight' `cor_weight' ///
			[weight = `raw_weight'], by(ftile)	
		qui gen country = "`c'"
		qui gen year = `t'
		
		//define pseudo-theta 
		qui gen pseudo_theta = `raw_weight' / _weight
		qui label var pseudo_theta "ratio: raw over corrected weight" 
		
		//check all fractiles are present in pseudo thetas 	
		cap assert _N == 127
		if _rc != 0 {
			tempfile tf_`c'_`t'
			qui save `tf_`c'_`t'', replace 
			clear 
			qui set obs 127
			qui gen ftile = (_n - 1)/100 in 1/100
			qui replace ftile = (99 + (_n - 100)/10)/100 in 101/109
			qui replace ftile = (99.9 + (_n - 109)/100)/100 in 110/118
			qui replace ftile = (99.99 + (_n - 118)/1000)/100 in 119/127
			qui merge 1:1 ftile using `tf_`c'_`t'', nogen
		
			//interpolate using model  
			qui nl (pseudo_theta = {a = 1} * (1 - ftile)^-{b = 1})  
			local coeff_`c'_`theta' = _b[b:]
			local coeff_rd_`c'_`theta' = round(`coeff_`c'_`theta'', 0.01)
			local coeff_txt_`c'_`theta' "`coeff_rd_`c'_`theta'' (`theta')"
			qui predict predicted //if e(sample)
			
			*qui ipolate pseudo_theta ftile, gen(ipo) epolate
			qui replace pseudo_theta = predicted if ///
				missing(pseudo_theta)
			qui drop predicted
			qui replace country = country[1] if missing(country)
			qui replace year = year[1] if missing(year)
			assert _N == 127
		}
	

    rename (`raw_weight' `cor_weight') (raw_weight_sum corrected_weight_sum)
    keep ftile pseudo_theta raw_weight_sum corrected_weight_sum
    save "`saving'", replace
end
