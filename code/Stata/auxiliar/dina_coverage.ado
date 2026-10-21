program define dina_coverage
    version 13
    syntax, Country(string) Step(string) Status(string) [Year(integer -1) Unit(string)]
    if "${dina_output_root}" == "" exit
    if "`unit'" == "" local unit "all"
    if !inlist("`status'", "expected", "success", "failed", "excluded", "not_applicable") {
        di as error "Invalid coverage status: `status'"
        exit 198
    }
    local recorded_year "`year'"
    if `year' < 0 local recorded_year ""
    local path "${dina_output_root}/_coverage.csv"
    tempname handle
    capture confirm file "`path'"
    local exists = (_rc == 0)
    file open `handle' using "`path'", write append text
    if !`exists' file write `handle' "country,year,step,unit,status" _n
    file write `handle' "`country',`recorded_year',`step',`unit',`status'" _n
    file close `handle'
end
