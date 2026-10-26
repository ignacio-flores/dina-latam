gpinter_precision_test_engine <- function() {
  env <- new.env(parent = globalenv())
  suppressPackageStartupMessages(library(dplyr))
  source(file.path(repo_root_for_tests, "code/R/gpinter_precision.R"), local = env)
  source(file.path(repo_root_for_tests, "code/R/gpinter_thresholds.R"), local = env)
  source(file.path(repo_root_for_tests, "code/R/gpinter_tail_repair.R"), local = env)
  source(file.path(repo_root_for_tests, "code/R/gpinter_atoms.R"), local = env)
  # Load pure definitions only: no runner entry, production reads, or writes.
  definitions <- c("thr_digits", "avg_digits", "tol", "p_grid", "collapse_and_clean",
                   "sanitize_eps", "strict_margin", "enforce_avg_strictly_inside", "fit_and_tab")
  for (expr in parse(file.path(repo_root_for_tests, "code/R/07c_interpolate_ineqstats.R"))) {
    if (is.call(expr) && identical(expr[[1L]], as.name("<-")) &&
        is.symbol(expr[[2L]]) && as.character(expr[[2L]]) %in% definitions) eval(expr, env)
  }
  env$prepare <- function(d) env$gpinter_precision_prepare(d, env$tol, env$strict_margin,
    env$collapse_and_clean, env$enforce_avg_strictly_inside)
  env
}

gpinter_precision_point_tail <- function(offset = 0) {
  # PER2003's generated tail: identical incomes, with Stata-float means just
  # below the double-precision threshold. The lower bin is an aggregate fixture.
  avg <- c(10000, 122000, 1536133.25, 1536133.25)
  p <- c(0, .99, .999, .9999)
  data.frame(p = p, thr = c(0, 100000, rep(1536133.27363 + offset, 2)),
             avg = avg, average = weighted.mean(avg, diff(c(p, 1))))
}

test_that("PER2003 rounding repairs preserve income and the first tied tail anchor", {
  e <- gpinter_precision_test_engine()
  d <- gpinter_precision_point_tail()
  prepared <- e$prepare(d)
  expect_equal(prepared$data$p, c(0, .99, .999))
  expect_equal(prepared$data$avg, c(d$avg[1:2], d$avg[3]))
  expect_equal(sum(prepared$data$avg * diff(c(prepared$data$p, 1))),
               sum(d$avg * diff(c(d$p, 1))))
  expect_equal(nrow(prepared$adjustments), 2L)
  expect_true(all(prepared$adjustments$threshold_after < prepared$adjustments$threshold_before))
  expect_true(all(prepared$adjustments$rule == "storage_precision_threshold_margin"))
  fitted <- e$fit_and_tab(d)
  expected <- sum(d$avg[d$p >= .999] * diff(c(d$p, 1))[d$p >= .999]) / d$average[1]
  expect_true(abs(fitted$topsh[fitted$p == .999] - expected) < 1e-8)
})

test_that("rounding perturbations do not erase the tail boundary", {
  e <- gpinter_precision_test_engine()
  shares <- vapply(c(-.04, -.02, 0, .02, .04), function(offset) {
    d <- gpinter_precision_point_tail(offset)
    p <- e$prepare(d)
    expect_true(.999 %in% p$data$p)
    f <- e$fit_and_tab(d)
    f$topsh[f$p == .999]
  }, 0)
  expect_true(max(shares) - min(shares) < 1e-8)
})

test_that("valid groups retain the existing cleaner and fit", {
  e <- gpinter_precision_test_engine()
  d <- data.frame(p = c(0, .5, .9, .99), thr = c(0, 1, 5, 10),
                  avg = c(.5, 2, 8, 20), average = 1)
  legacy <- e$enforce_avg_strictly_inside(e$collapse_and_clean(d))
  legacy$average <- weighted.mean(legacy$avg, diff(c(legacy$p, 1)))
  prepared <- e$prepare(d)
  expect_equal(prepared$data, legacy)
  expect_equal(nrow(prepared$adjustments), 0L)
})

test_that("substantive and infeasible repairs fail with group diagnostics", {
  e <- gpinter_precision_test_engine()
  d <- gpinter_precision_point_tail()
  d$thr[3:4] <- d$thr[3:4] + 100
  expect_error(e$prepare(d), "Substantive mean/threshold inconsistency")
  d <- data.frame(p = c(0, .5), thr = c(.002, 1), avg = c(.001, 2), average = 1)
  expect_error(e$prepare(d), "No feasible precision threshold")
  d <- gpinter_precision_point_tail()
  d$thr[2] <- d$avg[3] - .001
  # The unchanged legacy cleaner would absorb the repaired boundary here.
  error <- tryCatch(e$prepare(d), error = identity)
  expect_true(inherits(error, "gpinter_precision_error"))
  expect_true(!is.null(error$precision_adjustments))
})

test_that("fit validation rejects an altered repaired tail", {
  e <- gpinter_precision_test_engine()
  prepared <- e$prepare(gpinter_precision_point_tail())
  bad <- list(fractile = c(0, .999), top_share = c(1, .5))
  expect_error(e$gpinter_precision_validate_fit(bad, prepared), "did not preserve the repaired tail anchor")
})

test_that("a repaired threshold cannot silently change the preceding bracket income", {
  e <- gpinter_precision_test_engine()
  d <- data.frame(p = c(0, .5, .9), thr = c(0, 20, 40),
                  avg = c(19.995, 20, 60), average = 23.9975)
  # Lowering the middle threshold to 19.99 would make the legacy sanitizer
  # lower the preceding mean to 19.985. Its tail-anchor check alone misses this.
  error <- tryCatch(e$prepare(d), error = identity)
  expect_true(inherits(error, "gpinter_precision_error"))
  expect_match(conditionMessage(error), "change total income")
  expect_equal(nrow(error$precision_adjustments), 1L)
  expect_equal(d$avg, c(19.995, 20, 60))
})

test_that("sparse reversed PER fixtures require the complete grid for approved tail repair", {
  e <- gpinter_precision_test_engine()
  # Actual generated p90 pairs after the unchanged top-tail rescaling. These
  # are thousands of currency units apart, not floating-point discrepancies.
  for (pair in list(c(26990.835236, 23383.7653, 25929.44333),
                   c(50690.2362152, 40659.026222, 49057.25221))) {
    d <- data.frame(p = c(0, .89, .9, .99), thr = c(0, 20000, pair[1], 1e6),
                    avg = c(10000, pair[3], pair[2], 2e6), average = 1)
    # Threshold reconstruction alone still cannot repair reversed means.
    expect_error(e$gpinter_threshold_prepare(d, e$tol, e$strict_margin,
      e$collapse_and_clean, e$enforce_avg_strictly_inside), "[Dd]ecreasing bracket means")
    # The integration fallback is now authorized, but these sparse fixtures
    # omit p99.9/p99.99 and cannot establish its protected tail shares.
    expect_error(e$fit_and_tab(d), "requires the existing p90, p99, p99.9 and p99.99 fitting grid")
  }
})

test_that("a failed selected group writes diagnostics and no publishable CSV", {
  e <- gpinter_precision_test_engine()
  suppressPackageStartupMessages(library(tidyr))
  suppressPackageStartupMessages(library(purrr))
  stage <- tempfile("gpinter-precision-failure-")
  dir.create(file.path(stage, "output/data_reports"), recursive = TRUE)
  dir.create(file.path(stage, "output/ineqstats"), recursive = TRUE)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  published <- file.path(stage, "previous-publication.csv")
  writeLines("previous complete publication", published)
  p <- e$p_grid
  fixture <- data.frame(country = "PER", year = 2003L, step = "nat", unit = "esn",
    p = p, average = 50, thr = 100 * p, avg = 50 * (p + c(p[-1L], 1)),
    topavg = 50 * (1 + p), topsh = 1 - p^2)
  fixture$avg[fixture$p == .5] <- .1
  e$output_root <- stage
  e$selected_countries <- "PER"
  e$fixture <- fixture
  script <- parse(file.path(repo_root_for_tests, "code/R/07c_interpolate_ineqstats.R"))
  failure <- tryCatch({
    for (expr in script) {
      if (is.call(expr) && identical(expr[[1L]], as.name("country_run_r_entry"))) next
      if (is.call(expr) && identical(expr[[1L]], as.name("source"))) next
      if (is.call(expr) && identical(expr[[1L]], as.name("<-")) && is.symbol(expr[[2L]])) {
        name <- as.character(expr[[2L]])
        if (name %in% c("output_root", "selected_countries")) next
        if (name == "df") { e$df <- fixture; next }
      }
      eval(expr, e)
    }
    NULL
  }, error = identity)
  expect_match(conditionMessage(failure), "PER-2003-nat-esn")
  expect_match(conditionMessage(failure), "published CSV has not changed")
  expect_equal(readLines(published), "previous complete publication")
  expect_false(file.exists(file.path(stage, "output/ineqstats/_gpinter_all_topcorrected.csv")))
  coverage <- read.csv(file.path(stage, "_coverage.csv"))
  expect_equal(coverage$status, "failed")
  expect_match(coverage$reason, "[Dd]ecreas|[Rr]evers")
  expect_true(file.exists(file.path(stage, "output/data_reports/gpinter_precision_adjustments.csv")))
  expect_true(file.exists(file.path(stage, "output/data_reports/gpinter_threshold_reconstruction.csv")))
})

test_that("07c reconstructs selected thresholds and records the method without fitting other countries", {
  e <- gpinter_precision_test_engine()
  suppressPackageStartupMessages(library(tidyr))
  suppressPackageStartupMessages(library(purrr))
  stage <- tempfile("gpinter-threshold-selection-")
  dir.create(file.path(stage, "output/data_reports"), recursive = TRUE)
  dir.create(file.path(stage, "output/ineqstats"), recursive = TRUE)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  p <- e$p_grid
  selected <- data.frame(country = "PER", year = 2003L, step = "nat", unit = "esn",
    p = p, average = 50, thr = 100 * p, avg = 50 * (p + c(p[-1L], 1)),
    topavg = 50 * (1 + p), topsh = 1 - p^2)
  selected$thr[selected$p == .5] <- 1000
  unselected <- selected
  unselected$country <- "CHL"
  unselected$avg[unselected$p == .5] <- .1 # Would fail if fitted.
  e$output_root <- stage
  e$selected_countries <- "PER"
  e$fixture <- rbind(selected, unselected)
  for (expr in parse(file.path(repo_root_for_tests, "code/R/07c_interpolate_ineqstats.R"))) {
    if (is.call(expr) && identical(expr[[1L]], as.name("country_run_r_entry"))) next
    if (is.call(expr) && identical(expr[[1L]], as.name("source"))) next
    if (is.call(expr) && identical(expr[[1L]], as.name("<-")) && is.symbol(expr[[2L]])) {
      name <- as.character(expr[[2L]])
      if (name %in% c("output_root", "selected_countries")) next
      if (name == "df") { e$df <- e$fixture; next }
    }
    eval(expr, e)
  }
  coverage <- read.csv(file.path(stage, "_coverage.csv"))
  expect_equal(coverage$country, "PER")
  expect_equal(coverage$status, "success")
  methods <- read.csv(file.path(stage, "output/data_reports/gpinter_fitting_methods.csv"))
  expect_equal(methods$fit_method, "shares_fit")
  report <- read.csv(file.path(stage, "output/data_reports/gpinter_threshold_reconstruction.csv"))
  expect_true(nrow(report) > 0L)
  expect_true(all(report$country == "PER"))
  expect_true(any(report$p == .5 & report$threshold_before == 1000 & report$threshold_after < 100))
  result <- read.csv(file.path(stage, "output/ineqstats/_gpinter_all_topcorrected.csv"))
  expect_equal(nrow(result), 127L)
  expect_true(all(result$country == "PER"))
})
