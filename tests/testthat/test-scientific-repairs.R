test_that("PAVA handles singleton and fully monotone blocks and preserves previous valid fits", {
  stata <- Sys.getenv("DINA_STATA_CMD", unset = "/Applications/Stata/StataMP.app/Contents/MacOS/stata-mp")
  skip_if(!file.exists(stata), "Stata is not installed")
  work <- tempfile("dina-pava-test-"); dir.create(work)
  on.exit(unlink(work, recursive = TRUE))
  files <- c("bfmcorr_experiments.ado", "bfmcorr.ado", "experiments_bfm.ado")
  functions <- lapply(files, function(file) {
    lines <- readLines(file.path(repo_root_for_tests, "code/Stata/ado/bfm_stata_ado", file))
    start <- which(grepl("^real vector isotonic_pava\\(", lines))
    end <- which(seq_along(lines) > start & lines == "}")[[1L]]
    lines[start:end]
  })
  expect_identical(functions[[1]], functions[[2]])
  expect_identical(functions[[1]], functions[[3]])
  legacy <- gsub("isotonic_pava", "legacy_pava", functions[[1]], fixed = TRUE)
  legacy <- gsub("S = J(n + 1, 1, .)", "S = J(n, 1, .)", legacy, fixed = TRUE)
  script <- c("set more off", "mata:", functions[[1]], legacy,
    "w = (1\\1\\1)", "a = (3\\2\\1)", "y = isotonic_pava(w, a, 1)",
    "assert(all(y :== (3\\2\\1)))", "w = 2", "a = 7", "y = isotonic_pava(w, a, 1)", "assert(y == 7)",
    "w = (1\\2\\3)", "a = (3\\1\\2)", "y = isotonic_pava(w, a, 1)",
    "assert(max(abs(y :- (3\\1.6\\1.6))) < 1e-14)",
    "w = (1\\2\\3)", "a = (3\\1\\2)", "old = legacy_pava(w, a, 1)", "assert(all(y :== old))",
    "w = (1\\2\\3)", "a = (1\\2\\3)", "y = isotonic_pava(w, a, 0)", "assert(all(y :== (1\\2\\3)))", "end",
    'file open marker using "passed.txt", write replace', 'file write marker "passed"', "file close marker")
  writeLines(script, file.path(work, "test.do"))
  processx::run(stata, c("-b", "do", "test.do"), wd = work, timeout = 60000, error_on_status = FALSE)
  expect_true(file.exists(file.path(work, "passed.txt")), info = paste(readLines(file.path(work, "test.log"), warn = FALSE), collapse = "\n"))
})

test_that("trust CSV imports preserve the historical Excel boundary precision", {
  stata <- Sys.getenv("DINA_STATA_CMD", unset = "/Applications/Stata/StataMP.app/Contents/MacOS/stata-mp")
  skip_if(!file.exists(stata), "Stata is not installed")
  work <- tempfile("dina-trust-precision-test-"); dir.create(work)
  on.exit(unlink(work, recursive = TRUE))
  writeLines(c("country,year,trust,trust_wages", "MEX,2010,0.98,0.82"), file.path(work, "trust.csv"))
  openxlsx::write.xlsx(data.frame(country = "MEX", year = 2010, trust = .98, trust_wages = .82),
    file.path(work, "trust.xlsx"))
  files <- c("03a-run-bfm.do", "03b-run-bfm-2stage.do", "03d-prepare-theta-extrapolation.do")
  imports <- lapply(files, function(file) {
    lines <- readLines(file.path(repo_root_for_tests, "code/Stata", file))
    found <- lines[grepl('import delimited using "${admin_trust_regions}"', lines, fixed = TRUE)]
    expect_equal(length(found), 1L, info = file)
    found
  })
  script <- c("set more off", "set type float", 'global admin_trust_regions "trust.csv"',
    'import excel "trust.xlsx", firstrow clear',
    "scalar historical_wages = trust_wages[1]", "scalar historical_income = trust[1]",
    "assert historical_wages == .82", "assert historical_income == .98",
    'import delimited using "${admin_trust_regions}", clear varnames(1)',
    "scalar imprecise_wages = trust_wages[1]", "assert imprecise_wages == float(.82)",
    "clear", "set obs 2", "generate double p = cond(_n == 1, float(.82), .83)",
    "count if p < historical_wages", "assert r(N) == 1",
    "count if p < imprecise_wages", "assert r(N) == 0")
  for (import in imports) {
    script <- c(script, import,
      "assert trust_wages[1] == historical_wages", "assert trust[1] == historical_income",
      "scalar imported_wages = trust_wages[1]", "clear", "set obs 2",
      "generate double p = cond(_n == 1, float(.82), .83)",
      "drop if p < imported_wages", "assert _N == 1", "assert p[1] == .83")
  }
  script <- c(script, 'file open marker using "passed.txt", write replace',
    'file write marker "passed"', "file close marker")
  writeLines(script, file.path(work, "test.do"))
  processx::run(stata, c("-b", "do", "test.do"), wd = work, timeout = 60000, error_on_status = FALSE)
  expect_true(file.exists(file.path(work, "passed.txt")),
    info = paste(readLines(file.path(work, "test.log"), warn = FALSE), collapse = "\n"))
})

test_that("03e keeps the intended99.7 fractile despite Stata float storage", {
  stata <- Sys.getenv("DINA_STATA_CMD", unset = "/Applications/Stata/StataMP.app/Contents/MacOS/stata-mp")
  skip_if(!file.exists(stata), "Stata is not installed")
  work <- tempfile("dina-merging-grid-test-"); dir.create(work)
  on.exit(unlink(work, recursive = TRUE))
  lines <- readLines(file.path(repo_root_for_tests, "code/Stata/03e-extrapolate-bfm-correction.do"))
  start <- which(grepl("local dina_mp_requested =", lines, fixed = TRUE))
  end <- which(seq_along(lines) > start & grepl("qui sum p ", lines, fixed = TRUE) & !grepl(" if ", lines, fixed = TRUE))[[1L]] - 1L
  matching <- lines[start:end]
  header <- c('local c "CRI"', "local t 2003", "local mp_CRI_2003 = .997")
  script <- c("set more off", "clear", "set obs 3", "generate float p = .996 + (_n-1)*.001", header,
    "count if p >= `mp_CRI_2003'", "assert r(N) == 1", matching,
    "assert abs(`mp_CRI_2003' - .997) < .0000001", "assert `mp_CRI_2003' == p[2]",
    "count if p >= `mp_CRI_2003'", "assert r(N) == 2",
    "replace p = float(.997) in 1", "program define ambiguous", header, matching, "end",
    "capture noisily ambiguous", "local match_rc = _rc", "assert `match_rc' == 459",
    'file open marker using "passed.txt", write replace', 'file write marker "passed"', "file close marker")
  writeLines(script, file.path(work, "test.do"))
  processx::run(stata, c("-b", "do", "test.do"), wd = work, timeout = 60000, error_on_status = FALSE)
  expect_true(file.exists(file.path(work, "passed.txt")), info = paste(readLines(file.path(work, "test.log"), warn = FALSE), collapse = "\n"))
})
