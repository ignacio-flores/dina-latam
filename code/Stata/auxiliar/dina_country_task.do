* A standalone invocation uses exactly the same scope and publisher as the CLI.
args dina_task dina_countries
local dina_codes = subinstr("`dina_countries'", ",", " ", .)
foreach dina_code in `dina_codes' {
    if !regexm("`dina_code'", "^[A-Za-z][A-Za-z][A-Za-z]$") {
        di as error "Countries must be comma-separated ISO3 codes."
        exit 198
    }
}
local dina_rscript : environment DINA_RSCRIPT
if "`dina_rscript'" == "" local dina_rscript "Rscript"
tempfile dina_done
shell "`dina_rscript'" code/R/country_task.R "`dina_task'" "`dina_codes'" "`dina_done'"
capture confirm file "`dina_done'"
if _rc {
    di as error "Country task failed. See the error and resume command above."
    exit 459
}
