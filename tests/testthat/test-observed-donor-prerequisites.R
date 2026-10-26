test_that("03d requires independent donors only for selected eligible observed cases", {
  root <- tempfile("dina-donor-inputs-"); dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  env <- new.env(parent = globalenv())
  old <- getwd(); setwd(repo_root_for_tests); on.exit(setwd(old), add = TRUE)
  sys.source(file.path(repo_root_for_tests, "code/R/country_run.R"), env)
  dir.create(file.path(root, "config"))
  file.copy(file.path(repo_root_for_tests, "config/pipeline.yml"), file.path(root, "config/pipeline.yml"))
  base <- file.path(root, "output/bfm_summary"); dir.create(base, recursive = TRUE)
  openxlsx::write.xlsx(list(panel = data.frame(country = c("ARG", "ARG", "CRI", "CHL", "ECU"),
    year = c(2007L, 2012L, 2010L, 2000L, 2011L))), file.path(base, "all_thetas.xlsx"))
  config <- list(countries = c("ARG", "CRI", "CHL", "ECU"), years = list(first = 2000L, last = 2024L))
  task <- list(id = "03d-prepare-theta-extrapolation")
  check <- function(scope, planned = character()) env$country_run_case_prerequisites(task, root, config, scope, planned)
  result <- check("ARG")
  expect_length(result$issues, 1L)
  expect_match(result$issues, "ARG_2007, ARG_2012")
  expect_match(result$issues, "03b-run-bfm-2stage --countries ARG")
  expect_false(any(grepl("CRI_2010", result$issues)))
  planned <- check("ARG", "03b-run-bfm-2stage")
  expect_length(planned$issues, 0L)
  expect_match(planned$messages, "checked again before 03d")
  expect_length(check("ARG", "03a-run-bfm")$issues, 1L)
  # Existing donor exclusions do not create invented reconstruction work.
  expect_length(check(c("CHL", "ECU"))$issues, 0L)
  donors <- file.path(base, "observed_donors"); dir.create(donors)
  file.create(file.path(donors, c("ARG2007.dta", "ARG2012.dta")))
  expect_length(check("ARG")$issues, 0L)
  expect_length(check("CRI")$issues, 1L)
})

test_that("03d declares stable donor products instead of mutable corrected microdata", {
  tasks <- yaml::read_yaml(file.path(repo_root_for_tests, "config/pipeline.yml"))$tasks
  task <- Filter(function(x) startsWith(x$id, "03d-"), tasks)[[1L]]
  expect_true("output/bfm_summary/observed_donors" %in% unlist(task$inputs))
  expect_false(any(grepl("microdata/bfm", unlist(task$inputs))))
  for (prefix in c("03a-", "03b-")) {
    producer <- Filter(function(x) startsWith(x$id, prefix), tasks)[[1L]]
    expect_true("output/bfm_summary/observed_donors" %in% unlist(producer$outputs))
  }
})
