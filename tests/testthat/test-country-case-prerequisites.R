case_prerequisite_fixture <- function() {
  root <- tempfile("dina-case-inventory-"); dir.create(root)
  env <- new.env(parent = globalenv())
  old <- getwd(); setwd(repo_root_for_tests); on.exit(setwd(old))
  sys.source(file.path(repo_root_for_tests, "code/R/country_run.R"), envir = env)
  config <- list(countries = c("CRI", "ECU", "MEX"), years = list(first = 2020L, last = 2024L), run = list(steps = "natinc", debug = FALSE))
  path <- file.path(root, "intermediary_data/weight_adjusters/index.xlsx")
  dir.create(dirname(path), recursive = TRUE)
  openxlsx::write.xlsx(list(country_years_03d = data.frame(country_year = c("CRI_2022", "ECU_2023", "MEX_2024")),
    country_years_03e = data.frame(country_year = "CRI_2022", extrap = 0, fail = 1)), path)
  list(root = root, env = env, config = config)
}

test_that("explicit missing and failed corrected cases are scoped and recognize earlier producers", {
  f <- case_prerequisite_fixture(); on.exit(unlink(f$root, recursive = TRUE))
  task <- list(id = "06a-run-ineqstats")
  result <- f$env$country_run_case_prerequisites(task, f$root, f$config, "CRI")
  expect_length(result$issues, 1L)
  expect_match(result$issues, "CRI_2022 has a failed 03e")
  expect_false(any(grepl("ECU|MEX", result$issues)))
  path <- file.path(f$root, "intermediary_data/microdata/bfm_norep_pre/CRI_2022_bfm_norep_pre.dta")
  dir.create(dirname(path), recursive = TRUE); file.create(path)
  expect_length(f$env$country_run_case_prerequisites(task, f$root, f$config, "CRI")$issues, 1L)
  expected <- f$env$country_run_case_prerequisites(task, f$root, f$config, c("ECU", "MEX"))
  expect_length(expected$issues, 2L)
  planned <- f$env$country_run_case_prerequisites(task, f$root, f$config, c("ECU", "MEX"),
    c("03e-extrapolate-bfm-correction", "04b-build-pretax-surveys", "04c-create-raw-postax-units"))
  expect_length(planned$issues, 0L)
  expect_length(planned$messages, 2L)
  partial <- f$env$country_run_case_prerequisites(task, f$root, f$config, "CRI", "04b-build-pretax-surveys")
  expect_length(partial$issues, 1L)
  raw <- f$config; raw$run$steps <- "raw"
  expect_length(f$env$country_run_case_prerequisites(task, f$root, raw, "CRI")$issues, 0L)
})

test_that("approved historical observations and current MP indices both establish expected cases", {
  f <- case_prerequisite_fixture(); on.exit(unlink(f$root, recursive = TRUE))
  path <- file.path(f$root, "output/bfm_summary/all_thetas.xlsx")
  dir.create(dirname(path), recursive = TRUE)
  openxlsx::write.xlsx(list(panel = data.frame(country = c("MEX", "MEX"), year = 2021, p = c(.9, .95))), path)
  path <- file.path(f$root, "output/bfm_summary/bfm_norep_pos/merging_points.xlsx")
  dir.create(dirname(path), recursive = TRUE)
  openxlsx::write.xlsx(list(country_years_2stage = data.frame(country_year = c("MEX_2020", ""))), path)
  cases <- f$env$country_run_index_cases(f$root)
  expect_equal(sum(cases$country == "MEX" & cases$year == 2021L), 1L)
  result <- f$env$country_run_case_prerequisites(list(id = "05b-rescale-and-impute"), f$root, f$config, "MEX")
  expect_length(result$issues, 3L)
  expect_true(any(grepl("MEX_2020.*--from 03b", result$issues)))
  # Raw files not listed in any authoritative index do not create invented failures.
  dir.create(file.path(f$root, "intermediary_data/microdata/raw/MEX"), recursive = TRUE)
  file.create(file.path(f$root, "intermediary_data/microdata/raw/MEX/MEX_2022_raw.dta"))
  expect_false(any(grepl("MEX_2022", f$env$country_run_case_prerequisites(list(id = "06a-run-ineqstats"), f$root, f$config, "MEX")$issues)))
})

test_that("shared macro warnings and planned statistics preserve requested scope", {
  f <- case_prerequisite_fixture(); on.exit(unlink(f$root, recursive = TRUE))
  macro <- f$env$country_run_case_prerequisites(list(id = "05a-compare-sna-totals"), f$root, f$config,
    c("ECU", "MEX"), c("03e-extrapolate-bfm-correction", "04b-build-pretax-surveys"))
  expect_length(macro$issues, 0L)
  expect_true(any(grepl("outside.*CRI_2022", macro$messages)))
  stats <- f$env$country_run_case_prerequisites(list(id = "07a-export-synthetic-microfiles"), f$root, f$config,
    "CRI", "06a-run-ineqstats")
  expect_length(stats$issues, 0L)
  expect_true(any(grepl("produced by 06a", stats$messages)))
})

test_that("an accepted observed case supersedes an obsolete failed extrapolation entry", {
  f <- case_prerequisite_fixture(); on.exit(unlink(f$root, recursive = TRUE))
  panel <- file.path(f$root, "output/bfm_summary/all_thetas.xlsx")
  dir.create(dirname(panel), recursive = TRUE)
  openxlsx::write.xlsx(list(panel = data.frame(country = "CRI", year = 2022L, p = .9)), panel)
  openxlsx::write.xlsx(list(country_years_03d = data.frame(country_year = "MEX_2024"),
    country_years_03e = data.frame(country_year = "CRI_2022", extrap = 0, fail = 1)),
    file.path(f$root, "intermediary_data/weight_adjusters/index.xlsx"))
  path <- file.path(f$root, "intermediary_data/microdata/bfm_norep_pre/CRI_2022_bfm_norep_pre.dta")
  dir.create(dirname(path), recursive = TRUE); file.create(path)
  result <- f$env$country_run_case_prerequisites(list(id = "06a-run-ineqstats"), f$root, f$config, "CRI")
  expect_length(result$issues, 0L)
})
