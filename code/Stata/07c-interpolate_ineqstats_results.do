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
local rscript : environment DINA_RSCRIPT
if "`rscript'" == "" local rscript "Rscript"
tempfile dina_07c_ready
capture erase "`dina_07c_ready'"
local dina_07c_log "${dina_output_root}/07c-rscript.log"
di as txt "Interpolating ineqstats with R; R log: `dina_07c_log'"
// A fresh completion marker propagates R errors even when Stata's shell
// command does not return the child's status. All output remains staged.
shell "`rscript'" code/R/07c_run_interpolation.R "`dina_07c_ready'" > "`dina_07c_log'" 2>&1
capture noisily type "`dina_07c_log'"
capture confirm file "`dina_07c_ready'"
if _rc {
    di as error "07c R interpolation failed; see the R error above or `dina_07c_log'."
    di as error "Published interpolation results have not been replaced."
    exit 459
}
capture erase "`dina_07c_ready'"
