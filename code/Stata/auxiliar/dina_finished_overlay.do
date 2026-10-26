// Called after the internal rectangularization and before donor calculations.
// Keeping this operation here prevents an early supplied year from creating
// extra historical observations for every other country.
args overlay
preserve
    quietly use "`overlay'", clear
    keep country year step
    duplicates drop
    gen byte finished_dina = 1
    tempfile replacement_cases
    quietly save `replacement_cases'
restore
quietly merge m:1 country year step using `replacement_cases', keep(master match) nogen
quietly drop if finished_dina == 1
quietly drop finished_dina
quietly append using "`overlay'"
quietly replace finished_dina = 0 if missing(finished_dina)
isid country year step p
