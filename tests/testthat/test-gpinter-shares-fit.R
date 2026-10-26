gpinter_shares_test_engine <- function() {
  env <- new.env(parent = globalenv())
  source(file.path(repo_root_for_tests, "code/R/gpinter_precision.R"), local = env)
  source(file.path(repo_root_for_tests, "code/R/gpinter_thresholds.R"), local = env)
  env
}

test_that("explicit zero masses preserve the quantile curve and tiny first-positive income", {
  e <- gpinter_shares_test_engine()
  # Seven-bin numerical regression: the first positive bin is tiny relative
  # to total income, making differences of cumulative shares lose precision.
  d <- data.frame(p = c(0, .05, .15, .16, .5, .9, .99),
    avg = c(0, 0, .7382501231, 10000, 100000, 200000, 1000000), thr = 0)
  before <- d
  dist <- e$gpinter_threshold_fit_shares(d)
  expect_equal(d, before)
  expect_true(any(grepl("explicit Dirac mass", attr(dist, "dina_fit_notes"))))
  zero_quantiles <- gpinter::fitted_quantile(dist, c(0, .01, .049, .05, .1, .149))
  expect_true(all(abs(zero_quantiles) < 1e-12))
  actual_income <- stats::integrate(function(p) gpinter::fitted_quantile(dist, p),
    lower = .15, upper = .16, rel.tol = 1e-10, abs.tol = 1e-12)$value
  expected_income <- .01 * d$avg[3]
  expect_true(abs(actual_income - expected_income) < 1e-11)
  tab <- e$gpinter_threshold_tabulate(dist, d$p, d)
  expect_equal(tab$bracket_average[1:2], c(0, 0))
  expect_true(abs(tab$bracket_average[3] - actual_income / .01) < 1e-10)
  expect_true(e$gpinter_threshold_validate_fit(dist, tab,
    list(method = "shares_fit", original = d)))
  # An interior export-grid cut must also carry the correct local income.
  split_grid <- sort(c(d$p, .155))
  split <- e$gpinter_threshold_tabulate(dist, split_grid, d)
  first <- which(split$fractile >= .15 & split$fractile < .16)
  split_income <- sum(split$bracket_average[first] * diff(c(split$fractile, 1))[first])
  expect_true(abs(split_income - expected_income) < 1e-11)
})

test_that("shares fitting enforces a nonnegative lower boundary without changing means", {
  e <- gpinter_shares_test_engine()
  # This shape produces a negative spline-estimated first threshold in the
  # installed GPinter version even with first_threshold=0.
  p <- c(seq(0, .89, .01), .9, .99, .999, .9999)
  avg <- c(seq(100, 49057.25221, length.out = 90), 69386.765321, 196229.00884,
    392458.01768, 1962290.0884)
  d <- data.frame(p = p, avg = avg, thr = 0)
  dist <- e$gpinter_threshold_fit_shares(d)
  expect_true(gpinter::fitted_quantile(dist, 0) >= -1e-10)
  tab <- e$gpinter_threshold_tabulate(dist, p, d)
  expect_true(max(abs(tab$bracket_average - avg) / pmax(1, abs(avg))) < 1e-8)
  expect_true(e$gpinter_threshold_validate_fit(dist, tab,
    list(method = "shares_fit", original = d)))
})

test_that("the shares wrapper cannot infer an interior zero-income mass", {
  e <- gpinter_shares_test_engine()
  d <- data.frame(p = c(0, .5, .9), avg = c(1, 0, 100), thr = 0)
  expect_error(e$gpinter_threshold_fit_shares(d), "leading population mass")
})

test_that("tiny first-positive income includes a narrow endpoint histogram segment", {
  e <- gpinter_shares_test_engine()
  # A single adaptive quadrature call misses nearly half the first bin's
  # income here: GPinter inserts a knot close to p=.02. The tabulator must
  # integrate each interpolation segment rather than trust that error estimate.
  d <- data.frame(p = c(0, .005, .01, .02, .5, .9, .99),
    avg = c(0, 0, .01725414107, 10000, 100000, 200000, 1000000), thr = 0)
  dist <- e$gpinter_threshold_fit_shares(d)
  expected_income <- .01 * d$avg[3L]
  tab <- e$gpinter_threshold_tabulate(dist, d$p, d)
  expect_true(abs(tab$bracket_average[3L] * .01 - expected_income) < 1e-11)
  expect_true(e$gpinter_threshold_validate_fit(dist, tab,
    list(method = "shares_fit", original = d)))
  split <- e$gpinter_threshold_tabulate(dist, sort(c(d$p, .015)), d)
  keep <- split$fractile >= .01 & split$fractile < .02
  income <- sum(split$bracket_average[keep] * diff(c(split$fractile, 1))[keep])
  expect_true(abs(income - expected_income) < 1e-11)
})
