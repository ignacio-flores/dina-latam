test_that("observed donor capture matches legacy arithmetic and survives overwritten microdata", {
  stata <- Sys.getenv("DINA_STATA_CMD", unset = "/Applications/Stata/StataMP.app/Contents/MacOS/stata-mp")
  skip_if(!file.exists(stata), "Stata is not installed")
  work <- tempfile("dina-stable-donor-test-")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE))
  theta <- data.frame(p = c(.9, .99, .9999), antitonic = c(1.1, .3, .05), mpoint = c(.9, NA, NA))
  openxlsx::write.xlsx(list(ARG2007 = theta, CRI2010 = theta), file.path(work, "merging.xlsx"))
  helper <- file.path(repo_root_for_tests, "code/Stata/auxiliar/dina_observed_donor.do")
  legacy <- file.path(repo_root_for_tests, "tests/fixtures/observed_donor_legacy.do")
  lines <- readLines(file.path(repo_root_for_tests, "code/Stata/03d-prepare-theta-extrapolation.do"))
  start <- which(grepl("^// Load immutable observed summaries", lines))
  end <- which(grepl("^// 2. Graph Pseudo-thetas", lines)) - 1L
  consumer <- lines[start:end]
  policy_start <- which(grepl("^// Explicit scientific choices", lines))
  policy_end <- which(grepl("^// 3. Generate weight-adjusters", lines)) - 1L
  policy_loader <- lines[policy_start:policy_end]
  target_start <- which(grepl('local target_year "', lines, fixed = TRUE))
  target_end <- which(seq_along(lines) > target_start & grepl("//report existence of files", lines, fixed = TRUE))[[1L]] - 1L
  target_selection <- lines[target_start:target_end]
  dir.create(file.path(work, "config"))
  file.copy(file.path(repo_root_for_tests, "config/theta_extrapolation_policy.csv"), file.path(work, "config"))
  script <- c("set more off", "set type float", 'global bfm_replace "no"',
    sprintf('global dina_output_root "%s"', work),
    'global y_postax_private_wage "ind_pos_prwag"', 'global y_pretax_tot "ind_pre_tot"',
    sprintf('do "%s"', helper), sprintf('do "%s"', legacy),
    "foreach c in ARG CRI {", 'local t = cond("`c\'" == "ARG", 2007, 2010)',
    "clear", "set obs 5000", "generate long _pid = mod(_n, 5) + 1",
    "generate n_fep = 1 + mod(_n, 3)", "generate _fep = 1 + mod(_n, 5)",
    'local raw = cond("`c\'" == "ARG", "n_fep", "_fep")',
    "generate ind_pos_prwag = floor(_n/25)", "generate ind_pre_tot = 201 - floor(_n/25)",
    "generate _weight = `raw' * (1 + .15 * sin(_n/170))",
    "quietly summarize `raw', meanonly", "local total = r(sum)",
    "quietly summarize _weight, meanonly", "replace _weight = _weight * `total'/r(sum)",
    'save "micro_`c\'.dta", replace',
    # Legacy income/_pid ties are randomized by Stata. Fix the test seed in
    # both calls to compare the arithmetic, without changing production sorts.
    "set sortseed 123456",
    'legacy_observed_donor, country("`c\'") year(`t\') saving("expected_`c\'.dta")',
    'use "micro_`c\'.dta", clear',
    "clear", "set obs 1", "generate statistic = 42",
    "set sortseed 123456",
    'dina_observed_donor, country("`c\'") year(`t\') mpfile("merging.xlsx") corfile("micro_`c\'.dta") stage("03b")',
    "assert _N == 1 & statistic == 42",
    'use "output/bfm_summary/observed_donors/`c\'`t\'.dta", clear',
    "assert _N == 127", "assert source_method == \"observed_bfm_noreplace\"",
    "assert abs(unobs_cutoff - .9999) < 1e-7", "assert inter == 1 if missing(raw_weight_sum)",
    "assert abs(corrected_population/raw_population - 1) < 1e-7",
    "rename pseudo_theta captured", 'merge 1:1 ftile using "expected_`c\'.dta", assert(match) nogen',
    "assert captured == pseudo_theta", "}",
    "program define dina_coverage", "version 16.1", "end",
    "program define consume_stable_donors", "tempfile tf", "args review_root bad_panel",
    'global overlap_countries "ARG CRI"', 'global ARG_overlap_years "2007"', 'global CRI_overlap_years "2010"',
    "local trust_ARG_2007 = .95", "local trust_CRI_2010 = .95",
    "local panel_mp_ARG_2007 = .9", "local panel_mp_CRI_2010 = .9",
    'if "`bad_panel\'" != "" local panel_mp_ARG_2007 = .91', consumer, "end",
    "consume_stable_donors", 'save "consumed.dta", replace',
    'capture noisily consume_stable_donors "absent-review-root"',
    "local strict_rc = _rc", "assert `strict_rc' == 601",
    'capture noisily consume_stable_donors "" "bad_panel"',
    "local panel_rc = _rc", "assert `panel_rc' == 459",
    'use "micro_ARG.dta", clear', "replace _weight = n_fep * 20", 'save "micro_ARG.dta", replace',
    "consume_stable_donors", 'cf _all using "consumed.dta"',
    'erase "output/bfm_summary/observed_donors/ARG2007.dta"',
    "capture noisily consume_stable_donors", "local missing_rc = _rc", "assert `missing_rc' == 601",
    'global all_countries "ARG CRI COL"', policy_loader,
    'local observed_ARG "2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015"',
    'local observed_CRI "2010 2011 2012 2013 2014 2015 2016 2017 2018"',
    'foreach c in ARG CRI {', "forvalues t = 2000/2024 {", target_selection,
    'if "`c\'" == "ARG" & `t\' <= 2014 assert "`register_`c\'_`t\'\'" == "no"',
    'if "`c\'" == "ARG" & `t\' == 2015 assert "`register_`c\'_`t\'\'" == "yes"',
    'if "`c\'" == "CRI" & `t\' <= 2017 assert "`register_`c\'_`t\'\'" == "no"',
    'if "`c\'" == "CRI" & `t\' == 2018 assert "`register_`c\'_`t\'\'" == "yes"',
    'if `t\' >= 2019 assert "`register_`c\'_`t\'\'" == "no"', "}", "}",
    'file open marker using "passed.txt", write replace', 'file write marker "passed"', "file close marker")
  writeLines(script, file.path(work, "test.do"))
  processx::run(stata, c("-b", "do", "test.do"), wd = work, timeout = 60000, error_on_status = FALSE)
  expect_true(file.exists(file.path(work, "passed.txt")),
    info = paste(tail(readLines(file.path(work, "test.log"), warn = FALSE), 100L), collapse = "\n"))
})

test_that("explicit extrapolation policy contains the approved 22 observed cases only", {
  policy <- read.csv(file.path(repo_root_for_tests, "config/theta_extrapolation_policy.csv"))
  expect_equal(nrow(policy), 22L)
  expect_false(anyDuplicated(paste(policy$country, policy$year)) > 0L)
  expect_setequal(policy$year[policy$country == "ARG"], 2000:2014)
  expect_setequal(policy$year[policy$country == "CRI"], 2010:2016)
  expect_true(all(policy$method == "extrapolated"))
})
