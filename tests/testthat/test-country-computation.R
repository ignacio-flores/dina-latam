# Tiny Stata fixtures exercise the actual scripts without production inputs.
country_stata_fixture <- function(task, countries) {
  stata <- Sys.getenv("DINA_STATA_CMD", "/Applications/Stata/StataMP.app/Contents/MacOS/stata-mp")
  skip_if(!file.exists(stata), "Stata is not installed")
  root <- tempfile("dina-country-computation-")
  dir.create(root)
  dirs <- c("code/Stata/auxiliar", "intermediary_data/microdata/bfm_norep_pre",
            "stage/intermediary_data/microdata/bfm_norep_pre", "stage/output/figures/eff_tax_rates")
  for (path in dirs) dir.create(file.path(root, path), recursive = TRUE)
  for (helper in c("dina_coverage.ado", "dina_donor_snapshot.ado")) {
    file.copy(file.path(repo_root_for_tests, "code/Stata/auxiliar", helper), file.path(root, "code/Stata/auxiliar", helper))
  }
  config <- c('global all_countries "BRA CHL"', 'global first_y 2020', 'global last_y 2020',
    'global lang "en"', 'global bfm_replace "no"',
    'local scope : environment DINA_RUN_COUNTRIES', 'global dina_run_countries "`scope\'"',
    'local stage : environment DINA_OUTPUT_ROOT', 'global dina_output_root "`stage\'"',
    'adopath ++ "code/Stata/auxiliar"')
  for (name in c("aux_general.do", "dina_runtime_config.do")) writeLines(config, file.path(root, "code/Stata/auxiliar", name))
  list(root = root, stata = stata, env = c(DINA_COUNTRY_WORKER = task, DINA_RUN_COUNTRIES = countries,
    DINA_OUTPUT_ROOT = file.path(root, "stage")))
}

country_stata_execute <- function(fixture, lines) {
  writeLines(c("set more off", lines,
    'file open marker using "passed.txt", write replace', 'file write marker "passed"', "file close marker"),
    file.path(fixture$root, "test.do"))
  processx::run(fixture$stata, c("-b", "do", "test.do"), wd = fixture$root,
               env = c(Sys.getenv(), fixture$env), timeout = 60000, error_on_status = FALSE)
  expect_true(file.exists(file.path(fixture$root, "passed.txt")),
    info = paste(readLines(file.path(fixture$root, "test.log"), warn = FALSE), collapse = "\n"))
}

test_that("04c computes selected countries into stage and leaves all canonical inputs intact", {
  task <- "04c-create-raw-postax-units"
  fixture <- country_stata_fixture(task, "BRA")
  on.exit(unlink(fixture$root, recursive = TRUE))
  script <- file.path(repo_root_for_tests, "code/Stata", paste0(task, ".do"))
  country_stata_execute(fixture, c('clear', 'set obs 4', 'gen id_hogar = ceil(_n/2)',
    'gen married = 2', 'gen adults_house = 2', 'gen hh_size = 2',
    'foreach v in wag pen cap imp mix {', 'gen ind_pos_`v\' = _n', '}',
    'save "intermediary_data/microdata/bfm_norep_pre/BRA_2020_bfm_norep_pre.dta"',
    'save "intermediary_data/microdata/bfm_norep_pre/CHL_2020_bfm_norep_pre.dta"',
    sprintf('do "%s"', script),
    'use "stage/intermediary_data/microdata/bfm_norep_pre/BRA_2020_bfm_norep_pre.dta", clear',
    'assert esn_pos_wag == 1.5 in 1/2', 'assert esn_pos_wag == 3.5 in 3/4',
    'assert pch_pos_wag == esn_pos_wag',
    'capture confirm file "stage/intermediary_data/microdata/bfm_norep_pre/CHL_2020_bfm_norep_pre.dta"',
    'assert _rc == 601', 'foreach c in BRA CHL {',
    'use "intermediary_data/microdata/bfm_norep_pre/`c\'_2020_bfm_norep_pre.dta", clear',
    'capture confirm variable esn_pos_wag', 'assert _rc == 111', 'assert ind_pos_wag == _n', '}'))
  coverage <- read.csv(file.path(fixture$root, "stage/_coverage.csv"))
  expect_identical(coverage$country, c("BRA", "BRA"))
  expect_identical(coverage$status, c("expected", "success"))
})

test_that("04a scopes nested and formerly unconditional country branches", {
  task <- "04a-compute-effective-rates-all"
  fixture <- country_stata_fixture("04a-compute-effective-rates", "BRA DOM")
  on.exit(unlink(fixture$root, recursive = TRUE))
  child_dir <- file.path(fixture$root, "code/Stata/eff-tax-rates")
  dir.create(child_dir)
  branches <- c(COL = "compute-eff-tax-rate-COL-alt", BRA = "compute-eff-tax-rate-BRA-alt",
    MEX = "compute-eff-tax-rate-MEX-alt", CRI = "compute-eff-tax-rate-CRI", CHL = "compute-eff-tax-rate-CHL",
    SLV = "compute-eff-tax-rate-SLV", ECU = "compute-eff-tax-ss-rate-ECU", URY = "compute-eff-tax-ss-rate-URY",
    PER = "compute-eff-tax-rate-theo-PER", DOM = "compute-eff-tax-rate-DOM-alt")
  for (country in names(branches)) writeLines(c('clear all',
    sprintf('file open result using "${dina_output_root}/%s.txt", write replace', country),
    sprintf('file write result "%s"', country), 'file close result'), file.path(child_dir, paste0(branches[[country]], ".do")))
  country_stata_execute(fixture, sprintf('do "%s"', file.path(repo_root_for_tests, "code/Stata", paste0(task, ".do"))))
  expect_setequal(list.files(file.path(fixture$root, "stage"), pattern = "txt$"), c("BRA.txt", "DOM.txt"))
})

test_that("SLV rate coverage excludes only the intentionally omitted source years", {
  fixture <- country_stata_fixture("04a-compute-effective-rates", "SLV")
  on.exit(unlink(fixture$root, recursive = TRUE))
  lines <- readLines(file.path(repo_root_for_tests, "code/Stata/eff-tax-rates/compute-eff-tax-rate-SLV.do"))
  # Run the actual eligibility/import prefix against an empty input directory.
  # Limit only the loop years so the test never reaches rate calculations.
  first_calculation <- which(grepl("rename TOTALRENTASGRAVADAS", lines, fixed = TRUE))[[1L]]
  prefix <- lines[seq_len(first_calculation - 1L)]
  loop <- which(grepl("^forvalues x = 2000/2017 \\{", prefix))
  expect_length(loop, 1L)
  excluded <- prefix
  excluded[loop] <- "foreach x in 2008 2011 {"
  writeLines(c(excluded, "}"), file.path(fixture$root, "excluded.do"))
  eligible <- prefix
  eligible[loop] <- "foreach x in 2007 {"
  writeLines(c(eligible, "}"), file.path(fixture$root, "eligible.do"))
  country_stata_execute(fixture, c('do "code/Stata/auxiliar/aux_general.do"',
    'do "excluded.do"', 'capture noisily do "eligible.do"',
    'local import_rc = _rc', 'assert `import_rc\' == 601'))
  coverage <- read.csv(file.path(fixture$root, "stage/_coverage.csv"))
  expect_identical(coverage$country, rep("SLV", 5L))
  expect_equal(coverage$year, c(2008, 2008, 2011, 2011, 2007))
  expect_identical(coverage$status, c("expected", "excluded", "expected", "excluded", "expected"))
  log <- readLines(file.path(fixture$root, "test.log"), warn = FALSE)
  expect_true(any(grepl("Tabulaciones_SLV.xls not found", log, fixed = TRUE)))
})

test_that("04b reruns BRA and CRI annual and fallback rates from unchanged pretax incomes", {
  fixture <- country_stata_fixture("04b-build-pretax-surveys", "BRA CRI")
  on.exit(unlink(fixture$root, recursive = TRUE))
  file.copy(file.path(repo_root_for_tests, "code/Stata/auxiliar/aux_pretax.do"),
    file.path(fixture$root, "code/Stata/auxiliar/aux_pretax.do"))
  cat('\nglobal last_y 2021\n', file = file.path(fixture$root, "code/Stata/auxiliar/aux_general.do"), append = TRUE)
  dirs <- c("intermediary_data/microdata/bfm_norep_pos", "stage/intermediary_data/microdata/bfm_norep_pos")
  for (country in c("BRA", "CRI")) {
    dirs <- c(dirs, file.path("input_data/admin_data", country, "eff-tax-rate"),
      file.path("stage/input_data/admin_data", country, "eff-tax-rate"))
  }
  for (path in dirs) dir.create(file.path(fixture$root, path), recursive = TRUE)
  script <- file.path(repo_root_for_tests, "code/Stata/04b-build-pretax-surveys.do")
  country_stata_execute(fixture, c(
    'clear', 'set obs 10000', 'gen p_merge = _n - 1', 'gen double eff_tax_rate_ipol = .2',
    'foreach c in BRA CRI {',
    'save "input_data/admin_data/`c\'/eff-tax-rate/`c\'_effrates_2020.dta"', '}',
    'clear', 'set obs 4', 'gen id = _n', 'gen _fep = 1',
    'foreach v in wag fwag prfwag mix cap oth ben imp kap mir tot totnb totesn totesb pen {',
    'gen double pre_`v\'_svy = 100 * id', 'gen double ind_pre_`v\' = 200 * id', '}',
    '// Simulate derived fields retained by a previous 04b publication.',
    'gen double eff_tax_rate_ipol = .9', 'gen double eff_tax_rate_ipol_syn = .8',
    'gen double eff_ss_rate_ipol = .7', 'gen double eff_ss_rate_ipol_syn = .6',
    'foreach c in BRA CRI {', 'foreach year in 2020 2021 {',
    'save "intermediary_data/microdata/bfm_norep_pre/`c\'_`year\'_bfm_norep_pre.dta"', '}', '}',
    sprintf('do "%s"', script),
    'foreach c in BRA CRI {', 'foreach year in 2020 2021 {',
    'use "stage/intermediary_data/microdata/bfm_norep_pre/`c\'_`year\'_bfm_norep_pre.dta", clear',
    'assert _N == 4', 'assert abs(eff_tax_rate_ipol - .2) < 1e-7',
    'foreach v in wag fwag prfwag mix cap oth ben imp kap mir tot totnb totesn totesb pen {',
    'assert pre_`v\'_svy == 100 * id', 'assert ind_pre_`v\' == 200 * id',
    'assert abs(pos_`v\'_svy - .8 * pre_`v\'_svy) < .0001',
    'assert abs(ind_pos_`v\' - .8 * ind_pre_`v\') < .0001', '}',
    'save "first_`c\'_`year\'.dta"',
    '// Simulate publication so the second invocation reads the enriched file.',
    'save "intermediary_data/microdata/bfm_norep_pre/`c\'_`year\'_bfm_norep_pre.dta", replace', '}', '}',
    sprintf('do "%s"', script),
    'foreach c in BRA CRI {', 'foreach year in 2020 2021 {',
    'use "stage/intermediary_data/microdata/bfm_norep_pre/`c\'_`year\'_bfm_norep_pre.dta", clear',
    'sort id', 'cf _all using "first_`c\'_`year\'.dta", all',
    'use "stage/intermediary_data/microdata/bfm_norep_pos/`c\'_`year\'_bfm_norep_pos.dta", clear',
    'sort id', 'cf _all using "first_`c\'_`year\'.dta", all', '}', '}'))
  coverage <- read.csv(file.path(fixture$root, "stage/_coverage.csv"))
  expect_equal(sum(coverage$status == "success" & coverage$step == "pretax_survey"), 8L)
})

test_that("04b preserves the first accepted top-tail sample on a repeated run", {
  fixture <- country_stata_fixture("04b-build-pretax-surveys", "CRI")
  on.exit(unlink(fixture$root, recursive = TRUE))
  file.copy(file.path(repo_root_for_tests, "code/Stata/auxiliar/aux_pretax.do"),
    file.path(fixture$root, "code/Stata/auxiliar/aux_pretax.do"))
  for (path in c("stage/intermediary_data/microdata/bfm_norep_pos",
    "input_data/admin_data/CRI/eff-tax-rate", "stage/input_data/admin_data/CRI/eff-tax-rate")) {
    dir.create(file.path(fixture$root, path), recursive = TRUE)
  }
  script <- file.path(repo_root_for_tests, "code/Stata/04b-build-pretax-surveys.do")
  country_stata_execute(fixture, c(
    'clear', 'set obs 10000', 'gen p_merge = _n - 1', 'gen double eff_tax_rate_ipol = .2',
    'save "input_data/admin_data/CRI/eff-tax-rate/CRI_effrates_2020.dta"',
    'clear', 'set obs 100000', 'gen id = _n', 'gen _fep = 1',
    'foreach v in wag fwag prfwag mix cap oth ben imp kap mir tot totnb totesn totesb pen {',
    'gen double pre_`v\'_svy = id', 'gen double ind_pre_`v\' = id', '}',
    'save "intermediary_data/microdata/bfm_norep_pre/CRI_2020_bfm_norep_pre.dta"',
    '// Establish the unchanged first-run scientific sample with the real helper.',
    'global prefix "pre"', 'global section ""fractiles""',
    'do "code/Stata/auxiliar/aux_pretax.do"', 'assert _N < 100000',
    'save "original_fractile_sample.dta"',
    sprintf('do "%s"', script),
    'use "stage/intermediary_data/microdata/bfm_norep_pre/CRI_2020_bfm_norep_pre.dta", clear',
    'sort id', 'cf id _fep pre_* ind_pre_* p_merge fractiles using "original_fractile_sample.dta", all',
    'save "first.dta"',
    'save "intermediary_data/microdata/bfm_norep_pre/CRI_2020_bfm_norep_pre.dta", replace',
    sprintf('do "%s"', script),
    'use "stage/intermediary_data/microdata/bfm_norep_pre/CRI_2020_bfm_norep_pre.dta", clear',
    'sort id', 'cf _all using "first.dta", all',
    '// Invalid stored grids cannot be accepted as a reusable previous ranking.',
    'replace p_merge = . in 1',
    'save "intermediary_data/microdata/bfm_norep_pre/CRI_2020_bfm_norep_pre.dta", replace',
    sprintf('capture noisily do "%s"', script), 'local invalid_rc = _rc', 'assert `invalid_rc\' == 459'))
})

test_that("donor snapshots retain the complete table and coverage survives clear programs", {
  fixture <- country_stata_fixture("fixture", "BRA")
  on.exit(unlink(fixture$root, recursive = TRUE))
  country_stata_execute(fixture, c('do "code/Stata/auxiliar/aux_general.do"',
    'clear', 'input str3 country year tax', '"BRA" 2020 .25', '"CHL" 2020 .75', 'end',
    'dina_donor_snapshot tax using "${dina_output_root}/_donors_taxes.csv"',
    'assert _N == 2', 'assert tax == .75 in 2',
    'dina_coverage, country("BRA") year(2020) step("fixture") status("expected")',
    'clear programs', 'dina_coverage, country("BRA") year(2020) step("fixture") status("success")'))
  donors <- read.csv(file.path(fixture$root, "stage/_donors_taxes.csv"))
  expect_identical(donors$country, c("BRA", "CHL"))
  expect_equal(donors$value, c(.25, .75))
  expect_identical(read.csv(file.path(fixture$root, "stage/_coverage.csv"))$status, c("expected", "success"))
})

test_that("03a records an intentional empty contribution when no overlap is eligible", {
  fixture <- country_stata_fixture("03a-run-bfm", "BRA")
  on.exit(unlink(fixture$root, recursive = TRUE))
  for (path in c("output/bfm_summary/bfm_norep_pre", "output/bfm_summary/bfm_norep_pos", "output/figures/MP")) {
    dir.create(file.path(fixture$root, "stage", path), recursive = TRUE, showWarnings = FALSE)
  }
  cat('\nglobal countries_bfm_02a "BRA CHL"\n', file = file.path(fixture$root, "code/Stata/auxiliar/aux_general.do"), append = TRUE)
  country_stata_execute(fixture, sprintf('do "%s"', file.path(repo_root_for_tests, "code/Stata/03a-run-bfm.do")))
  coverage <- read.csv(file.path(fixture$root, "stage/_coverage.csv"))
  expect_identical(coverage$country, "BRA")
  expect_identical(coverage$status, "not_applicable")
  source(file.path(repo_root_for_tests, "code/R/country_outputs.R"), local = TRUE)
  expect_silent(country_output_coverage(file.path(fixture$root, "stage")))
})

test_that("05b snapshots actual prepared scalars for unselected recipient countries", {
  fixture <- country_stata_fixture("fixture", "BRA")
  on.exit(unlink(fixture$root, recursive = TRUE))
  lines <- readLines(file.path(repo_root_for_tests, "code/Stata/05b-rescale-and-impute.do"))
  first <- which(startsWith(lines, "// Snapshot exactly"))
  last <- which(startsWith(lines, "// 3. Rescale")) - 1L
  writeLines(c('do "code/Stata/auxiliar/aux_general.do"', 'local donor_snapshot "1"',
    'global scal_list "pre_wag"', 'local ni_wid_BRA_2020 100', 'local ni_wid_CHL_2020 200',
    'local scaling_pre_wag_CHL_2020 2', lines[first:last]), file.path(fixture$root, "snapshot.do"))
  country_stata_execute(fixture, 'do "snapshot.do"')
  donors <- read.csv(file.path(fixture$root, "stage/_donors.csv"))
  expect_equal(donors$value[donors$country == "BRA" & donors$variable == "ni_wid"], 100)
  expect_equal(donors$value[donors$country == "CHL" & donors$variable == "ni_wid"], 200)
  expect_equal(donors$value[donors$country == "CHL" & donors$variable == "scaling_pre_wag"], 2)
})
