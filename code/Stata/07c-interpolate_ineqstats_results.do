* DINA country-run entry; the worker writes only to its temporary output folder.
args dina_requested_countries
local dina_worker : environment DINA_COUNTRY_WORKER
if "`dina_worker'" != "07c-interpolate-ineqstats" {
    do "code/Stata/auxiliar/dina_country_task.do" "07c-interpolate-ineqstats" "`dina_requested_countries'"
    exit
}
local dina_output_env : environment DINA_OUTPUT_ROOT
local dina_countries_env : environment DINA_RUN_COUNTRIES
if `"`dina_output_env'"' == "" | trim("`dina_countries_env'") == "" {
    di as error "Missing country-run output folder or selected countries."
    exit 198
}
global dina_output_root `"`dina_output_env'"'
global dina_run_countries "`dina_countries_env'"

do "code/Stata/auxiliar/dina_runtime_config.do"
di as txt "Using an R to interpolate ineqstats..."
rcall: source("code/R/07c_interpolate_ineqstats.R")
