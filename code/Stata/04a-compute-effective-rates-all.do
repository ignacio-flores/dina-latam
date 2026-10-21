// Direct and runner execution share country selection and staged publication.
args dina_requested_countries
local dina_country_worker : environment DINA_COUNTRY_WORKER
if "`dina_country_worker'" != "04a-compute-effective-rates" {
    do "code/Stata/auxiliar/dina_country_task.do" "04a-compute-effective-rates" "`dina_requested_countries'"
    exit
}

////////////////////////////////////////////////////////////////////////////////
// Calculates effective tax rates for all country-years with data. 
////////////////////////////////////////////////////////////////////////////////

clear all
do "code/Stata/auxiliar/dina_runtime_config.do"
adopath ++ "code/Stata/auxiliar"
global codes "code/Stata/eff-tax-rates"

// Create directory if it doesnt exist 
local dirpath "${dina_output_root}/output/figures/eff_tax_rates"
mata: st_numscalar("exists", direxists(st_local("dirpath")))
if (scalar(exists) == 0) {
	mkdir "`dirpath'"
	display "Created directory: `dirpath'"
}

foreach c in "COL" "BRA" "MEX" {
	local selected_countries "${dina_run_countries}"
	local selected : list c in selected_countries
	if !`selected' continue
	di as result "running $codes/compute-eff-tax-rate-`c'-alt.do"
	global codes "code/Stata/eff-tax-rates"
	qui do "$codes/compute-eff-tax-rate-`c'-alt.do"
}


*empirical w/o social sec
foreach c in "CRI" /*"ARG"*/ "CHL" "SLV"  {
	local selected_countries "${dina_run_countries}"
	local selected : list c in selected_countries
	if !`selected' continue
	di as result "running $codes/compute-eff-tax-rate-`c'.do"
	global codes "code/Stata/eff-tax-rates"
	qui do "$codes/compute-eff-tax-rate-`c'.do"
}

*empirical w/ social sec
foreach c in /*"COL"*/ "ECU" "URY" {
	local selected_countries "${dina_run_countries}"
	local selected : list c in selected_countries
	if !`selected' continue
	di as result "running $codes/compute-eff-tax-ss-rate-`c'.do"
	global codes "code/Stata/eff-tax-rates"
	qui do "$codes/compute-eff-tax-ss-rate-`c'.do"
}

*theoretical and DOM branches
foreach c in PER DOM {
    local selected_countries "${dina_run_countries}"
    local selected : list c in selected_countries
    if !`selected' continue
    global codes "code/Stata/eff-tax-rates"
    if "`c'" == "PER" do "$codes/compute-eff-tax-rate-theo-PER.do"
    if "`c'" == "DOM" do "$codes/compute-eff-tax-rate-DOM-alt.do"
}
clear
