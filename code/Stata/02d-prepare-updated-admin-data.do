// Reviewed Admin PIT handoff
//
// Chile, Brazil and Colombia are cleaned during Admin Explore and accepted
// together by Admin Include. This task intentionally does not re-run their
// historical cleaners: doing so could choose a different publisher workbook
// or overwrite the reviewed interpolation-ready tables.
do "code/Stata/auxiliar/dina_runtime_config.do"

capture confirm file "${admin_pit_outputs_manifest}"
if _rc {
    di as err "Admin PIT outputs are not the reviewed included version; explore and include Admin."
    exit 601
}

di as txt "Using reviewed included Admin PIT outputs."
di as txt "02d is a validation-only handoff; no country cleaner is run here."
di as txt "Manifest: ${admin_pit_outputs_manifest}"
