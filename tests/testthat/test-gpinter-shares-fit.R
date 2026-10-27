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

test_that("a tiny histogram mean uses its original anchor instead of cumulative subtraction", {
  e <- gpinter_shares_test_engine()
  # Actual CHL2024 pod anchors after the already approved broad-tail repair.
  # The tiny first positive bin has a mean near 2.49 against an overall mean
  # near 17 million. GPinter's cumulative subtraction loses its last digits.
  d <- read.csv(file.path(repo_root_for_tests,
    "tests/fixtures/chl_2024_pod_gpinter_anchors.csv"))
  before <- d
  repair <- e$gpinter_threshold_histogram_mean
  e$gpinter_threshold_histogram_mean <- function(dist, fitting) dist
  raw <- e$gpinter_threshold_fit_shares(d)
  raw_tab <- e$gpinter_threshold_tabulate(raw,d$p,d)
  expect_error(e$gpinter_threshold_validate_fit(raw,raw_tab,
    list(method="shares_fit",original=d)),"did not preserve original bracket means")
  e$gpinter_threshold_histogram_mean <- repair
  dist <- e$gpinter_threshold_fit_shares(d)
  tab <- e$gpinter_threshold_tabulate(dist,d$p,d)
  expect_equal(d,before,tolerance=0)
  expect_true(e$gpinter_threshold_validate_fit(dist,tab,
    list(method="shares_fit",original=d)))
  first <- which(d$avg > 0)[1L]
  expect_true(abs(tab$bracket_average[first]-d$avg[first]) < 1e-9)
  expect_equal(dist$pk[c(1,3)],raw$pk[c(1,3)],tolerance=0)
  expect_equal(dist$qk[c(1,3)],raw$qk[c(1,3)],tolerance=0)
  expect_equal(dist$pk[-2],raw$pk[-2],tolerance=0)
  expect_equal(dist$qk[-2],raw$qk[-2],tolerance=0)
  expect_equal(dist$average,raw$average,tolerance=0)
  expect_equal(dist$mu_top,raw$mu_top,tolerance=0)
  expect_equal(dist$sigma_top,raw$sigma_top,tolerance=0)
  expect_equal(dist$xi_top,raw$xi_top,tolerance=0)
  expect_true(any(grepl("cancellation in cumulative-income subtraction",attr(dist,"dina_fit_notes"))))
  grid <- sort(unique(c(d$p,.035,dist$pk[2])))
  split <- e$gpinter_threshold_tabulate(dist,grid,d)
  expect_true(e$gpinter_threshold_validate_fit(dist,split,
    list(method="shares_fit",original=d)))
  selected <- grid>=d$p[first] & c(grid[-1],1)<=d$p[first+1]
  expect_true(abs(sum(split$bracket_average[selected]*diff(c(grid,1))[selected])-
    (d$p[first+1]-d$p[first])*d$avg[first]) < 1e-10)
  fitting <- d[seq.int(first,nrow(d)),]
  substantive <- fitting; substantive$avg[1] <- substantive$avg[1] * 2
  expect_error(repair(raw,substantive),"beyond arithmetic precision")
  negative <- fitting; negative$avg[1] <- -1
  expect_error(repair(raw,negative),"beyond arithmetic precision")
  infeasible <- raw; infeasible$qk[1] <- fitting$avg[1]
  expect_error(repair(infeasible,fitting),"infeasible")
  expect_identical(repair(dist,fitting),dist)
})
