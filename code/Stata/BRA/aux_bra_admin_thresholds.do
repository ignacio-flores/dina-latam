capture program drop bra_admin_thresholds
program define bra_admin_thresholds, rclass
	version 14
	syntax, Year(integer) [Require(string)]

	local minwage_path "input_data/admin_data/BRA/downloads/wiki_minwage.csv"
	local thresholds_path "input_data/admin_data/BRA/downloads/admin_thresholds.csv"

	foreach path in "`minwage_path'" "`thresholds_path'" {
		capture confirm file "`path'"
		if _rc {
			di as error "Missing Brazil auxiliary threshold input: `path'"
			exit 601
		}
	}

	local minwage = .
	local avg_ui = .
	local maxlimit_inss = .
	local exempt_dirpf = .

	preserve
		quietly import delimited using "`minwage_path'", varnames(1) clear
		capture confirm variable year
		if _rc {
			di as error "Brazil minimum-wage input must contain a year column: `minwage_path'"
			restore
			exit 459
		}
		capture confirm variable minwage
		if _rc {
			di as error "Brazil minimum-wage input must contain a minwage column: `minwage_path'"
			restore
			exit 459
		}
		capture confirm numeric variable year
		if _rc quietly destring year, replace ignore(" ,")
		capture confirm numeric variable minwage
		if _rc quietly destring minwage, replace ignore(" ,")
		quietly count if year == `year'
		if r(N) != 1 {
			di as error "Expected exactly one minimum-wage row for Brazil year `year' in `minwage_path'; found " r(N)
			restore
			exit 459
		}
		quietly summarize minwage if year == `year', meanonly
		local minwage = r(mean)
	restore

	preserve
		quietly import delimited using "`thresholds_path'", varnames(1) clear
		foreach var in year avg_ui maxlimit_inss exempt_dirpf {
			capture confirm variable `var'
			if _rc {
				di as error "Brazil admin-threshold input must contain column `var': `thresholds_path'"
				restore
				exit 459
			}
			capture confirm numeric variable `var'
			if _rc quietly destring `var', replace ignore(" ,")
		}
		quietly count if year == `year'
		if r(N) != 1 {
			di as error "Expected exactly one admin-threshold row for Brazil year `year' in `thresholds_path'; found " r(N)
			restore
			exit 459
		}
		foreach var in avg_ui maxlimit_inss exempt_dirpf {
			quietly summarize `var' if year == `year', meanonly
			local `var' = r(mean)
		}
	restore

	local require : lower local require
	local require : subinstr local require "," " ", all
	local require : subinstr local require "-" "_", all
	foreach var of local require {
		if !inlist("`var'", "minwage", "avg_ui", "maxlimit_inss", "exempt_dirpf") {
			di as error "Unknown Brazil threshold requirement: `var'"
			exit 198
		}
		if missing(``var'') {
			di as error "Missing required Brazil threshold `var' for year `year'"
			exit 459
		}
	}

	return scalar minwage = `minwage'
	return scalar avg_ui = `avg_ui'
	return scalar maxlimit_inss = `maxlimit_inss'
	return scalar exempt_dirpf = `exempt_dirpf'
end
