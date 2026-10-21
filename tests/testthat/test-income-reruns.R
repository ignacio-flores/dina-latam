income_rerun_fixture <- function() {
  stata <- Sys.getenv("DINA_STATA_CMD", "/Applications/Stata/StataMP.app/Contents/MacOS/stata-mp")
  skip_if(!file.exists(stata), "Stata is not installed")
  root <- tempfile("dina-income-rerun-")
  dir.create(root)
  list(root = root, stata = stata)
}

income_rerun_execute <- function(fixture, lines, env = character()) {
  writeLines(c("set more off", lines,
    'file open marker using "passed.txt", write replace', 'file write marker "passed"', "file close marker"),
    file.path(fixture$root, "test.do"))
  processx::run(fixture$stata, c("-b", "do", "test.do"), wd = fixture$root,
    env = c(Sys.getenv(), env), timeout = 60000, error_on_status = FALSE)
  expect_true(file.exists(file.path(fixture$root, "passed.txt")),
    info = paste(readLines(file.path(fixture$root, "test.log"), warn = FALSE), collapse = "\n"))
}

test_that("05c refreshes macro columns and repeats the positive-gap calculation safely", {
  fixture <- income_rerun_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE))
  for (path in c("code/Stata/auxiliar", "intermediary_data/national_accounts",
    "intermediary_data/microdata/bfm_norep_pre", "stage/intermediary_data/microdata/bfm_norep_pre")) {
    dir.create(file.path(fixture$root, path), recursive = TRUE)
  }
  file.copy(file.path(repo_root_for_tests, "code/Stata/auxiliar/dina_coverage.ado"),
    file.path(fixture$root, "code/Stata/auxiliar/dina_coverage.ado"))
  writeLines(c('global dina_run_countries "BRA"', 'global first_y 2020', 'global last_y 2020',
    'global bfm_replace "no"', 'global lang "en"', 'global unit_list "ind"',
    'global ind_upr_norm "ind_pre_fac_sca"', 'local stage : environment DINA_OUTPUT_ROOT',
    'global dina_output_root "`stage\'"', 'adopath ++ "code/Stata/auxiliar"'),
    file.path(fixture$root, "code/Stata/auxiliar/aux_general.do"))
  script <- file.path(repo_root_for_tests, "code/Stata/05c-impute-pretax.do")
  income_rerun_execute(fixture, c(
    'clear', 'set obs 2', 'gen country = "BRA"', 'gen year = 2019 + _n',
    'gen double tax_indg_oecd_gni = .1', 'gen double TOT_B5g_wid = 1000',
    'foreach v in bpi_corp_gg prop_inc_net_gg gdp_wid GG_D61_R {', 'gen double `v\' = 10', '}',
    'save "intermediary_data/national_accounts/UNDATA-WID-OECD-Merged.dta"',
    'clear', 'set obs 4', 'gen id = _n', 'gen double ind_pre_fac_sca = id * 10',
    'gen _weight = 1', 'gen double original_income = id * 17',
    'save "intermediary_data/microdata/bfm_norep_pre/BRA_2020_bfm_norep_pre.dta"',
    sprintf('do "%s"', script),
    'use "stage/intermediary_data/microdata/bfm_norep_pre/BRA_2020_bfm_norep_pre.dta", clear',
    'assert tax_indg_lcu == 100', 'assert abs(ind_pre_lef - id * 80) < .0001',
    'assert original_income == id * 17', 'sort id', 'save "first.dta"',
    'save "intermediary_data/microdata/bfm_norep_pre/BRA_2020_bfm_norep_pre.dta", replace',
    sprintf('do "%s"', script),
    'use "stage/intermediary_data/microdata/bfm_norep_pre/BRA_2020_bfm_norep_pre.dta", clear',
    'sort id', 'cf _all using "first.dta", all',
    '// A changed source must replace the old macro values in the enriched survey.',
    'gen double TOT_B5g_wid = 1000', 'gen double tax_indg_oecd_gni = .9',
    'gen double ind_shtotaux = 999',
    'save "intermediary_data/microdata/bfm_norep_pre/BRA_2020_bfm_norep_pre.dta", replace',
    'use "intermediary_data/national_accounts/UNDATA-WID-OECD-Merged.dta", clear',
    'replace TOT_B5g_wid = 2000',
    'save "intermediary_data/national_accounts/UNDATA-WID-OECD-Merged.dta", replace',
    sprintf('do "%s"', script),
    'use "stage/intermediary_data/microdata/bfm_norep_pre/BRA_2020_bfm_norep_pre.dta", clear',
    'assert tax_indg_lcu == 200',
    'assert abs(ind_pre_lef - id * 170) < .0001',
    'assert ind_pre_fac_sca == id * 10', 'assert original_income == id * 17'),
    c(DINA_COUNTRY_WORKER = "05c-impute-pretax", DINA_OUTPUT_ROOT = file.path(fixture$root, "stage")))
})

test_that("05d refreshes only the prepared CEQ and macro columns on enriched inputs", {
  fixture <- income_rerun_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE))
  lines <- readLines(file.path(repo_root_for_tests, "code/Stata/05d-impute-posttax-disp-nat.do"))
  preparation <- function(table) {
    at <- which(startsWith(trimws(lines), paste0("local dina_", table, "_fields ")))
    expect_length(at, 1L)
    lines[c(at - 1L, at)]
  }
  merge_block <- function(table, merge) {
    anchor <- if (table == "spending") "local dina_spending_refresh_fields : list " else
      paste0("foreach v of local dina_", table, "_fields {")
    first <- which(grepl(anchor, lines, fixed = TRUE))
    last <- which(grepl(merge, lines, fixed = TRUE))
    expect_length(first, 1L)
    expect_length(last, 1L)
    lines[first:last]
  }
  ceq_merge <- merge_block("ceq", "merge m:1 country ftile using `tf_ceq'")
  tax_merge <- merge_block("tax", "merge m:1 country year using `tf_sna_tax'")
  spending_merge <- merge_block("spending", "merge m:1 country year using `tf_sna_spending'")
  income_rerun_execute(fixture, c('tempfile tf_ceq tf_sna_tax tf_sna_spending',
    'clear', 'set obs 1', 'gen country = "BRA"', 'gen ftile = 50000',
    'gen double sh_indg_ceq = .25', 'gen double sh_health_ceq = .5',
    'gen ctry_yr_ceq = "BRA_2010"', preparation("ceq"), 'save `tf_ceq\'',
    'clear', 'set obs 1', 'gen country = "BRA"', 'gen year = 2020',
    'gen double tax_indg_oecd_gni = .1', 'gen double TOT_B5g_wid = 1000',
    'gen extsna_tax_indg_oecd_gni = 0', preparation("tax"), 'save `tf_sna_tax\'',
    'clear', 'set obs 1', 'gen country = "BRA"', 'gen year = 2020',
    '// Unequal overlapping values exercise the original first-merge precedence.',
    'gen double hea_gni = .2', 'gen double TOT_B5g_wid = 2000',
    preparation("spending"), 'save `tf_sna_spending\'',
    'foreach enriched in 0 1 {', 'clear', 'set obs 1', 'gen country = "BRA"',
    'gen year = 2020', 'gen ftile = 50000', 'gen double ind_pre_tot = 123',
    'gen double original_income = 17', 'gen double sh_unrelated = 7',
    'if `enriched\' {',
    'foreach v in sh_indg_ceq sh_health_ceq tax_indg_oecd_gni TOT_B5g_wid hea_gni extsna_tax_indg_oecd_gni {',
    'gen double `v\' = 999', '}', 'gen ctry_yr_ceq = "old"', '}',
    ceq_merge, tax_merge, spending_merge,
    'assert sh_indg_ceq == .25', 'assert sh_health_ceq == .5',
    'assert ctry_yr_ceq == "BRA_2010"', 'assert tax_indg_oecd_gni == .1',
    'assert TOT_B5g_wid == 1000', 'assert hea_gni == .2',
    'assert extsna_tax_indg_oecd_gni == 0', 'assert ind_pre_tot == 123',
    'assert original_income == 17', 'assert sh_unrelated == 7',
    'if !`enriched\' save "fresh.dta"', 'else cf _all using "fresh.dta", all', '}'))
})
