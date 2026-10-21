program define dina_donor_snapshot
    version 13
    syntax varlist(numeric) using/
    tempname rows
    tempfile snapshot
    postfile `rows' str3 country int year str64 variable double value using `snapshot', replace
    quietly count
    local n = r(N)
    forvalues i = 1/`n' {
        foreach v of local varlist {
            post `rows' (country[`i']) (year[`i']) ("`v'") (`v'[`i'])
        }
    }
    postclose `rows'
    preserve
        quietly use `snapshot', clear
        sort country year variable
        format value %24.17g
        quietly export delimited using "`using'", replace
    restore
end
