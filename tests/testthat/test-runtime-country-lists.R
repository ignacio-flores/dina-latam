test_that("generated quoted country universes validate complete and boundary selections in Stata", {
  stata <- Sys.getenv("DINA_STATA_CMD", unset = "/Applications/Stata/StataMP.app/Contents/MacOS/stata-mp")
  skip_if(!file.exists(stata), "Stata unavailable")
  root <- tempfile("dina-country-lists-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  env <- new.env(parent = globalenv())
  sys.source(file.path(repo_root_for_tests, "code/R/cli/dina_lib.R"), envir = env)
  config <- yaml::read_yaml(file.path(repo_root_for_tests, "config/dina.yml"))
  config$countries <- c("COL", "ARG", "PER", "URY", "CRI", "ECU", "CHL", "BRA", "SLV", "MEX", "DOM")
  generated <- file.path(root, "generated.do")
  env$dina_render_config_do(config, generated, identity = list(scope = "fixture", fingerprint = "fixture",
    baseline_fingerprint = "fixture", admin_trust_regions = "fixture.csv"))
  file.copy(file.path(repo_root_for_tests, "code/Stata/auxiliar/dina_runtime_config.do"), file.path(root, "runtime.do"))
  writeLines(c("clear all", 'global dina_run_countries "OLD"', 'do "runtime.do"',
    'file open scope using "scope.txt", write text replace',
    'file write scope `"$all_countries"\' _n "$dina_run_countries" _n', "file close scope"),
    file.path(root, "scope.do"))
  writeLines(c('capture noisily do "scope.do"', "local rc = _rc", 'file open result using "rc.txt", write text replace',
    'file write result "`rc\'"', "file close result"), file.path(root, "worker.do"))
  run <- function(selected, worker = "fixture") {
    unlink(file.path(root, c("rc.txt", "scope.txt")))
    processx::run(stata, c("-b", "do", "worker.do"), wd = root, timeout = 30000,
      env = c(DINA_CONFIG_DO = generated, DINA_COUNTRY_WORKER = worker,
        DINA_RUN_COUNTRIES = selected, DINA_OUTPUT_ROOT = root), error_on_status = FALSE)
    as.integer(readLines(file.path(root, "rc.txt"), warn = FALSE))
  }
  universe <- paste(config$countries, collapse = " ")
  for (selected in c(universe, "COL", "DOM", "COL DOM", "ECU MEX")) {
    expect_equal(run(selected), 0L)
    actual <- readLines(file.path(root, "scope.txt"), warn = FALSE)
    expect_identical(actual[[2L]], selected)
    expect_identical(strsplit(trimws(gsub('"', "", actual[[1L]])), " +")[[1L]], config$countries)
  }
  expect_equal(run("ZZZ"), 198L)
  expect_false(file.exists(file.path(root, "scope.txt")))
  expect_equal(run("", worker = ""), 0L)
  expect_identical(readLines(file.path(root, "scope.txt"), warn = FALSE)[[2L]], universe)
})
