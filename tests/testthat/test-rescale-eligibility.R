test_that("05b retains historical zero-component exclusions and fails genuinely missing inputs", {
  stata <- Sys.getenv("DINA_STATA_CMD", "/Applications/Stata/StataMP.app/Contents/MacOS/stata-mp")
  skip_if(!file.exists(stata), "Stata is not installed")
  root <- tempfile("dina-rescale-eligibility-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  for (path in c("code/Stata/auxiliar", "intermediary_data/microdata/bfm_norep_pre", "stage")) {
    dir.create(file.path(root, path), recursive = TRUE)
  }
  file.copy(file.path(repo_root_for_tests, "code/Stata/auxiliar/dina_coverage.ado"),
    file.path(root, "code/Stata/auxiliar/dina_coverage.ado"))

  # Exercise the production file/country dispatch, input checks and exclusions.
  # Replace only the expensive calculation body with a success marker.
  lines <- readLines(file.path(repo_root_for_tests, "code/Stata/05b-rescale-and-impute.do"))
  first <- which(startsWith(lines, "// 3. Rescale and impute corrected surveys"))
  calculation <- which(trimws(lines) == "//rescale them")
  excluded <- which(grepl('else if "`misvar_`c\'_`y\'\'" != "" {', lines, fixed = TRUE))
  expect_length(first, 1L)
  expect_length(calculation, 1L)
  expect_length(excluded, 1L)
  writeLines(c('local ext "_norep"',
    'foreach c in CRI MEX {', 'forvalues y = 2002/2006 {',
    'local TOT_B5g_wid_`c\'_`y\' "$fixture_total"', '}', '}',
    lines[first:(calculation - 1L)],
    'file open result using "${dina_output_root}/`c\'`y\'.txt", write replace',
    'file write result "calculation reached"', 'file close result',
    'dina_coverage, country("`c\'") year(`y\') step("rescale") status("success")',
    '}', lines[excluded:length(lines)]), file.path(root, "eligibility.do"))

  writeLines(c('set more off', 'adopath ++ "code/Stata/auxiliar"',
    'global dina_output_root "stage"', 'global dina_run_countries "CRI"',
    'global first_y 2002', 'global last_y 2006', 'global unit_list "ind"',
    'global exep_countries "BRA"', 'global ind_raw_norm "ind_pre_wag ind_pre_cap"',
    'global fixture_total 1000',
    'forvalues y = 2002/2006 {', 'clear', 'set obs 4',
    'gen id = _n', 'gen double ind_pre_wag = 100 * id',
    'gen double ind_pre_cap = 10 * id',
    'if inlist(`y\', 2002, 2003, 2005) replace ind_pre_cap = 0',
    'save "intermediary_data/microdata/bfm_norep_pre/CRI_`y\'_bfm_norep_pre.dta"', '}',
    'do "eligibility.do"',
    'forvalues y = 2002/2006 {',
    'use "intermediary_data/microdata/bfm_norep_pre/CRI_`y\'_bfm_norep_pre.dta", clear',
    'assert ind_pre_wag == 100 * id',
    'if inlist(`y\', 2002, 2003, 2005) assert ind_pre_cap == 0',
    'else assert ind_pre_cap == 10 * id', '}',
    '// This is the original generic rule, independent of country codes.',
    'use "intermediary_data/microdata/bfm_norep_pre/CRI_2002_bfm_norep_pre.dta", clear',
    'save "intermediary_data/microdata/bfm_norep_pre/MEX_2002_bfm_norep_pre.dta"',
    'global dina_run_countries "MEX"', 'global last_y 2002', 'do "eligibility.do"',
    '// Missing fields and entirely missing values are not zero-component exclusions.',
    'drop ind_pre_cap',
    'save "intermediary_data/microdata/bfm_norep_pre/MEX_2002_bfm_norep_pre.dta", replace',
    'capture noisily do "eligibility.do"', 'assert _rc == 459',
    'gen double ind_pre_cap = .',
    'save "intermediary_data/microdata/bfm_norep_pre/MEX_2002_bfm_norep_pre.dta", replace',
    'capture noisily do "eligibility.do"', 'assert _rc == 459',
    '// Both numeric-missing and absent prepared macro totals must fail.',
    'replace ind_pre_cap = 10 * id',
    'save "intermediary_data/microdata/bfm_norep_pre/MEX_2002_bfm_norep_pre.dta", replace',
    'foreach total in "." "" {', 'global fixture_total "`total\'"',
    'capture noisily do "eligibility.do"', 'assert _rc == 459', '}',
    'file open marker using "passed.txt", write replace', 'file write marker "passed"', 'file close marker'),
    file.path(root, "test.do"))
  processx::run(stata, c("-b", "do", "test.do"), wd = root,
    timeout = 60000, error_on_status = FALSE)
  expect_true(file.exists(file.path(root, "passed.txt")),
    info = paste(readLines(file.path(root, "test.log"), warn = FALSE), collapse = "\n"))
  coverage <- read.csv(file.path(root, "stage/_coverage.csv"))
  cri <- coverage[coverage$country == "CRI" & coverage$status != "expected", ]
  expect_equal(cri$year, 2002:2006)
  expect_identical(cri$status, c("excluded", "excluded", "success", "excluded", "success"))
  mex <- coverage[coverage$country == "MEX" & coverage$status != "expected", ]
  expect_identical(mex$status, c("excluded", rep("failed", 4L)))
  expect_setequal(list.files(file.path(root, "stage"), pattern = "[.]txt$"), c("CRI2004.txt", "CRI2006.txt"))
})
