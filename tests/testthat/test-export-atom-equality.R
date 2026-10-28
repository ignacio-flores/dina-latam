test_that("Stata consistency handoffs smooth positive masses and preserve donor contributions", {
  stata <- "/Applications/Stata/StataMP.app/Contents/MacOS/stata-mp"
  skip_if_not(file.exists(stata), "Stata is required")
  skip_if_not_installed("haven"); skip_if_not_installed("processx")
  root <- tempfile("consistency-stata-"); dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  stopifnot(file.symlink(file.path(repo_root_for_tests, "code"), file.path(root, "code")))
  grid <- c(seq(0, .99, .01), seq(.991, .999, .001), seq(.9991, .9999, .0001), seq(.99991, .99999, .00001))
  mass <- diff(c(grid, 1))
  make <- function(country, plateaus = FALSE) {
    a <- 1 + grid
    if (plateaus) {
      a[grid < .1] <- 0
      a[grid >= .49 & grid < .52] <- 1.5
      a[grid >= .999] <- 2
    }
    a <- a / sum(a * mass)
    t <- c(0, head(a, -1))
    data.frame(country, year = 2024L, step = "nat", p = round(grid * 1e5),
      bracketavg_norm = a, bracketavg = a, bracketpop = mass * 1e5,
      thr = t, topavg = rev(cumsum(rev(a * mass))) / (1-grid),
      s = a * mass, topsh = rev(cumsum(rev(a * mass))),
      pub_source = if (country == "AAA") "supplied-fixture" else "",
      data_points = "[2024]", extrap_points = "", interp_points = "")
  }
  input <- rbind(make("AAA", TRUE), make("BBB"))
  haven::write_dta(input, file.path(root, "input.dta"))
  # AAA's source still participates in donors even when omitted from this
  # deliberately restricted publication scope.
  scope <- data.frame(country = c("BBB", "REG"), year = 2024L, step = "nat", consistency_publish = 1L)
  dir.create(file.path(root, "work"))
  haven::write_dta(scope, file.path(root, "work/publication_scope.dta"))
  code <- readLines(file.path(repo_root_for_tests, "code/Stata/07d-export-results-to-wid.do"))
  block <- function(start, end) {
    a <- which(grepl(start, code, fixed = TRUE))[[1L]]
    b <- which(grepl(end, code, fixed = TRUE) & seq_along(code) > a)[[1L]]
    code[a:(b-1L)]
  }
  commands <- c("clear all", "set more off", "set type double", 'use "input.dta", clear',
    'local consistency_work "work"', 'tempfile consistency_input consistency_output',
    block("// Consistency preparation of sources", "// End consistency source preparation."),
    'save "prepared.dta", replace', "preserve",
    "collapse (mean) bracketavg_norm thr (firstnm) bracketpop, by(year step p)",
    'gen str3 country = "REG"', 'save "regional.dta", replace', "restore",
    'append using "regional.dta"',
    block("// Consistency preparation of assembled", "// End consistency assembled preparation."),
    'save "assembled.dta", replace',
    'drop if consistency_publish == 0', 'save "publication.dta", replace',
    'file open done using "passed.txt", write replace', 'file write done "passed"', "file close done")
  writeLines(commands, file.path(root, "check.do"))
  processx::run(stata, c("-b", "do", "check.do"), wd = root, timeout = 90000, error_on_status = FALSE)
  expect_true(file.exists(file.path(root, "passed.txt")),
    info = paste(tail(readLines(file.path(root, "check.log"), warn = FALSE), 45), collapse = "\n"))
  if (!file.exists(file.path(root, "passed.txt"))) return(invisible(NULL))
  prepared <- as.data.frame(haven::read_dta(file.path(root, "prepared.dta")))
  assembled <- as.data.frame(haven::read_dta(file.path(root, "assembled.dta")))
  published <- as.data.frame(haven::read_dta(file.path(root, "publication.dta")))
  order_rows <- function(d) d[order(d$country, d$p), ]
  prepared <- order_rows(prepared); assembled <- order_rows(assembled)
  unchanged <- prepared$country == "BBB"
  expect_equal(prepared$bracketavg_norm[unchanged], input$bracketavg_norm[input$country == "BBB"], tolerance = 0)
  z <- prepared[prepared$country == "AAA", ]
  expect_true(all(z$bracketavg_norm[z$p < 10000] == 0))
  expect_true(all(diff(z$bracketavg_norm[z$bracketavg_norm > 0]) > 0))
  expect_true(all(diff(z$thr[z$thr > 0]) > 0))
  expect_equal(sum(z$bracketavg_norm * mass), 1, tolerance = 1e-14)
  expect_true(all(z$pub_source == "supplied-fixture"))
  source_again <- assembled[assembled$country != "REG", ]
  expect_identical(source_again$bracketavg_norm, prepared$bracketavg_norm)
  expect_identical(source_again$thr, prepared$thr)
  expected_regional <- (prepared$bracketavg_norm[prepared$country == "AAA"] + prepared$bracketavg_norm[prepared$country == "BBB"]) / 2
  expect_equal(assembled$bracketavg_norm[assembled$country == "REG"], expected_regional, tolerance = 0)
  expect_setequal(published$country, c("BBB", "REG"))
  expect_true(all(is.finite(assembled$s)))
  expect_true(all(is.finite(assembled$topavg)))
})

test_that("Stata records the donor pools actually used for regional review", {
  stata <- "/Applications/Stata/StataMP.app/Contents/MacOS/stata-mp"
  skip_if_not(file.exists(stata), "Stata is required")
  skip_if_not_installed("processx")
  root <- tempfile("regional-map-"); dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  code <- readLines(file.path(repo_root_for_tests, "code/Stata/07d-export-results-to-wid.do"))
  a <- which(grepl('local dina_expected ""', code, fixed = TRUE))
  b <- which(grepl('levelsof country if', code, fixed = TRUE))
  pool <- code[a:(b-1L)]
  a <- which(grepl('tempname dina_donor_map', code, fixed = TRUE))
  b <- which(seq_along(code) > a & trimws(code) == "preserve")[[1L]]
  writer <- code[a:(b-1L)]
  commands <- c("clear all", "set more off", 'global all_countries "ARG CRI ECU URY"',
    'local steps "nat pod"', 'foreach s in `steps\' {', 'foreach x in all CUB {',
    pool, "}", "}", 'local missing_ctries "BOL CUB"',
    paste0('local consistency_work "', root, '"'), writer, "}", "}",
    "file close `dina_donor_map'")
  writeLines(commands, file.path(root, "map.do"))
  processx::run(stata, c("-b", "do", "map.do"), wd = root, timeout = 60000, error_on_status = FALSE)
  path <- file.path(root, "regional_donors.csv")
  expect_true(file.exists(path))
  x <- read.csv(path)
  expect_equal(nrow(x), 12L)
  expect_setequal(x$step, c("nat", "pod"))
  expect_setequal(x$donor, c("ARG", "ECU", "URY"))
  expect_false(anyDuplicated(x) > 0)
})
