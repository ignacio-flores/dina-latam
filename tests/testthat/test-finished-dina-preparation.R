source(file.path(repo_root_for_tests, "code", "R", "source-diagnostics", "finished_dina_explorer.R"))
source(file.path(repo_root_for_tests, "code", "R", "source-diagnostics", "finished_dina_preparer.R"))

finished_preparation_fixture <- function(
    means = c(1, 3, 8, 20), p = c(0, 0.5, 0.9, 0.99),
    masses = c(0.49, 0.41, 0.09, 0.01), thresholds = c(0, 2, 5, 15)) {
  average <- sum(means * masses)
  shares <- means * masses / average
  data.frame(
    percentile = paste0("p", p * 100, "p", c(tail(p, -1L), 1) * 100),
    step = "nat", unit = "esn", country = "AAA", year = 2022,
    gini = 0.45, average = average, p = p, thr = thresholds, avg = means,
    bckt_sh = shares, topavg = rev(cumsum(rev(means * masses))) / rev(cumsum(rev(masses))),
    topsh = rev(cumsum(rev(shares))), stringsAsFactors = FALSE
  )
}

finished_preparation_spec <- function() {
  list(countries = "AAA", years = 2022, steps = "nat", units = "esn",
    definitions = list(population_age = "fixture", currency_price_basis = "fixture",
      bracket_semantics = "weighted source bins"))
}

testthat::test_that("preparation recovers weighted populations and preserves source income", {
  testthat::skip_if_not_installed("gpinter")
  x <- finished_preparation_fixture()
  original <- x
  calls <- list()
  result <- finished_dina_prepare(x, finished_preparation_spec(),
    progress = function(i, n, label) calls[[length(calls) + 1L]] <<- list(i, n, label))
  testthat::expect_identical(x, original)
  testthat::expect_equal(nrow(result$data), 127)
  testthat::expect_identical(result$data$p, finished_dina_export_grid())
  testthat::expect_true(all(result$data$gini == x$gini[[1L]]))
  testthat::expect_equal(unique(result$data$average), x$average[[1L]], tolerance = 1e-12)
  testthat::expect_true(all(result$validation$checks$status == "pass"))
  testthat::expect_lt(result$summary$max_mean_preservation_relative_error, 1e-6)
  testthat::expect_identical(calls, list(list(1L, 1L, "AAA 2022 nat/esn")))
  mass <- subset(result$changes, field == "population_mass" & rule == "inferred_population")
  testthat::expect_equal(mass$after[mass$percentile == "p0p50"], 0.49)
  share <- subset(result$impact, statistic == "bottom50")
  testthat::expect_setequal(share$basis, c("nominal_label_to_exact_population", "same_population_boundary"))
  testthat::expect_equal(share$original_population_boundary, rep(0.49, 2))
  testthat::expect_equal(share$before[share$basis == "same_population_boundary"],
    share$after[share$basis == "same_population_boundary"], tolerance = 1e-6)
  testthat::expect_true(any(is.na(subset(result$impact, statistic == "top0_1")$before)))
})

testthat::test_that("precision adjustments are bounded and independent top means are checked", {
  testthat::skip_if_not_installed("gpinter")
  x <- finished_preparation_fixture()
  x$average <- x$average * (1 + 4e-7)
  result <- finished_dina_prepare(x, finished_preparation_spec())
  testthat::expect_equal(unique(result$data$average), unique(x$average) / (1 + 4e-7),
    tolerance = 1e-12)
  testthat::expect_equal(result$summary$population_normalization, 4e-7, tolerance = 1e-12)
  testthat::expect_true(any(result$changes$rule == "precision_normalization"))
  x$average <- x$average * (1 + 2e-6)
  testthat::expect_error(finished_dina_prepare(x, finished_preparation_spec()),
    "AAA 2022 nat/esn.*normalization exceeds")
  x <- finished_preparation_fixture()
  x$topavg[[3L]] <- x$topavg[[3L]] * 1.01
  testthat::expect_error(finished_dina_prepare(x, finished_preparation_spec()),
    "AAA 2022 nat/esn p90p99.*cumulative top mean")
  x <- finished_preparation_fixture()
  x$avg[[1L]] <- 0
  testthat::expect_error(finished_dina_prepare(x, finished_preparation_spec()), "Nonpositive bracket means")
})

testthat::test_that("threshold repairs preserve means and distinguish numerical margins", {
  testthat::skip_if_not_installed("gpinter")
  x <- finished_preparation_fixture()
  x$thr[[2L]] <- 1
  x$thr[[3L]] <- 9
  result <- finished_dina_prepare(x, finished_preparation_spec())
  repairs <- subset(result$changes, rule == "threshold_repair")
  margins <- subset(result$changes, rule == "threshold_numerical_margin")
  testthat::expect_equal(repairs$percentile, "p90p99")
  testthat::expect_equal(repairs$after, 8 - 8e-7, tolerance = 1e-12)
  testthat::expect_equal(margins$percentile, "p50p90")
  testthat::expect_equal(margins$after, 1 + 3e-7, tolerance = 1e-12)
  testthat::expect_equal(result$summary$threshold_repairs, 1)
  testthat::expect_equal(result$summary$threshold_nudges, 1)
  testthat::expect_false(any(result$changes$rule == "tiny_mean_pooling"))
  testthat::expect_equal(result$data$thr[[1L]], 0)
})

testthat::test_that("only approved tiny adjacent pooling is allowed", {
  testthat::skip_if_not_installed("gpinter")
  p <- c(0, 0.5, 0.9, 0.91, 0.99)
  x <- finished_preparation_fixture(
    means = c(1, 3, 3 * (1 - 8e-6), 8, 20), p = p,
    masses = diff(c(p, 1)), thresholds = c(0, 2, 3, 5, 15))
  result <- finished_dina_prepare(x, finished_preparation_spec())
  pooled <- subset(result$changes, rule == "tiny_mean_pooling" & field == "avg")
  testthat::expect_equal(nrow(pooled), 2)
  testthat::expect_lte(max(abs(pooled$relative_percent)), 0.001)
  testthat::expect_equal(result$summary$pooled_bins, 2)
  testthat::expect_equal(result$summary$fitted_brackets, 4)
  testthat::expect_equal(result$summary$prepared_brackets, 127)
  testthat::expect_equal(unique(result$data$average), x$average[[1L]], tolerance = 1e-12)
  testthat::expect_equal(sum(diff(c(p, 1))[2:3] * pooled$after),
    sum(diff(c(p, 1))[2:3] * pooled$before), tolerance = 1e-12)
  bad <- finished_preparation_fixture(
    means = c(1, 3, 2.9, 8, 20), p = p,
    masses = diff(c(p, 1)), thresholds = c(0, 2, 3, 5, 15))
  testthat::expect_error(finished_dina_prepare(bad, finished_preparation_spec()),
    "above the approved 0.001% limit", fixed = TRUE)
})

testthat::test_that("complete declared coverage and source structure are required", {
  testthat::skip_if_not_installed("gpinter")
  x <- finished_preparation_fixture()
  spec <- finished_preparation_spec()
  spec$years <- c(2021, 2022)
  testthat::expect_error(finished_dina_prepare(x, spec), "Expected distribution is absent")
  testthat::expect_error(finished_dina_prepare(rbind(x, x[1, ]), finished_preparation_spec()),
    "Duplicate case/percentile key")
  x <- finished_preparation_fixture()
  x$percentile[[2L]] <- "p51p90"
  x$p[[2L]] <- 0.51
  testthat::expect_error(finished_dina_prepare(x, finished_preparation_spec()), "Gap before this interval")
})
