source(file.path(repo_root_for_tests, "code", "R", "source-diagnostics", "finished_dina_explorer.R"))

finished_dina_fixture <- function(year = 2022, country = "AAA") {
  data.frame(
    percentile = c("p0p50", "p50p100"), step = "nat", unit = "esn",
    country = country, year = year, gini = 0.125, average = 2,
    p = c(0, 0.5), thr = c(0, 2), avg = c(1, 3),
    bckt_sh = c(0.25, 0.75), topavg = c(2, 3), topsh = c(1, 0.75)
  )
}

finished_dina_spec <- function() {
  list(countries = "AAA", years = 2022, steps = "nat", units = "esn",
       definitions = list(population_age = "all ages", currency_price_basis = "fixture units",
                          bracket_semantics = "population intervals with income means"))
}

testthat::test_that("coherent supplied distributions pass independently of export grid", {
  data <- finished_dina_fixture()
  original <- data
  result <- finished_dina_validate(data, finished_dina_spec())
  testthat::expect_identical(data, original)
  testthat::expect_false(any(result$checks$status %in% c("fail", "review")))
  testthat::expect_identical(result$checks$status[result$checks$check_id == "export_grid"], "warning")
  testthat::expect_identical(result$checks$status[result$checks$check_id == "percentile_coverage"], "pass")
  testthat::expect_true(result$coverage$publication)
  testthat::expect_equal(result$coverage$rows, 2)
  testthat::expect_equal(result$coverage$first_year, 2022)
  testthat::expect_true(all(c("country", "year", "step", "unit", "percentile", "observed", "expected", "difference") %in% names(result$issues)))
})

testthat::test_that("schema and nonfinite errors are explicit and do not throw", {
  data <- finished_dina_fixture()
  data$avg <- NULL
  result <- finished_dina_validate(data)
  testthat::expect_true(any(result$issues$check_id == "schema" & result$issues$field == "avg"))
  data <- finished_dina_fixture()
  data$avg[[1L]] <- Inf
  result <- finished_dina_validate(data)
  issue <- subset(result$issues, check_id == "finite_values")
  testthat::expect_equal(issue$row, 1)
  testthat::expect_equal(issue$percentile, "p0p50")
  testthat::expect_equal(issue$status, "fail")
  data$avg <- c("one", "three")
  testthat::expect_true(any(finished_dina_validate(data)$issues$check_id == "schema"))
  data <- finished_dina_fixture()
  data$country[[1L]] <- NA_character_
  data$year[[2L]] <- NA_real_
  testthat::expect_true(any(finished_dina_validate(data)$issues$check_id == "key_values"))
})

testthat::test_that("duplicates and missing declared cases are structural failures", {
  data <- rbind(finished_dina_fixture(), finished_dina_fixture()[1L, ])
  result <- finished_dina_validate(data, finished_dina_spec())
  testthat::expect_equal(sum(result$issues$check_id == "unique_keys"), 2)
  spec <- finished_dina_spec()
  spec$years <- list(first = 2021, last = 2022)
  spec$units <- c("esn", "ind")
  result <- finished_dina_validate(finished_dina_fixture(), spec)
  absent <- subset(result$issues, check_id == "expected_coverage")
  testthat::expect_equal(nrow(absent), 3)
  testthat::expect_true(all(absent$status == "fail"))
  testthat::expect_true(all(absent$country == "AAA"))
  testthat::expect_equal(sort(unique(absent$year)), c(2021, 2022))
})

testthat::test_that("real interval coverage distinguishes merged brackets from gaps", {
  data <- finished_dina_fixture()
  data$percentile[[2L]] <- "p60p100"
  data$p[[2L]] <- 0.6
  result <- finished_dina_validate(data, finished_dina_spec())
  gap <- subset(result$issues, check_id == "percentile_coverage")
  testthat::expect_equal(gap$row, 2)
  testthat::expect_equal(gap$difference, 0.1)
  data$percentile[[2L]] <- "p40p100"
  data$p[[2L]] <- 0.4
  result <- finished_dina_validate(data, finished_dina_spec())
  testthat::expect_true(any(grepl("Overlap", result$issues$detail)))
  data$percentile[[2L]] <- "not-a-percentile"
  testthat::expect_true(any(finished_dina_validate(data)$issues$check_id == "percentile_intervals"))
})

testthat::test_that("storage precision is tolerated but substantive p misalignment fails", {
  data <- finished_dina_fixture()
  data$p[[2L]] <- data$p[[2L]] + 2.95e-8
  result <- finished_dina_validate(data, finished_dina_spec())
  testthat::expect_identical(result$checks$status[result$checks$check_id == "percentile_alignment"], "pass")
  data$p[[2L]] <- 0.51
  testthat::expect_true(any(finished_dina_validate(data)$issues$check_id == "percentile_alignment"))
})

testthat::test_that("scientific checks report precise unmodified values for review", {
  data <- finished_dina_fixture()
  data$gini <- 1.1
  data$thr <- c(2, 1)
  data$avg <- c(3, 1)
  data$bckt_sh[[1L]] <- 0.4
  data$topsh[[1L]] <- 0.8
  data$topavg[[1L]] <- 9
  result <- finished_dina_validate(data, finished_dina_spec())
  for (id in c("gini_bounds", "threshold_order", "bracket_average_order", "bracket_bounds",
               "share_total", "share_identity", "top_share_identity", "top_average_identity")) {
    testthat::expect_true(any(result$issues$check_id == id & result$issues$status == "review"), info = id)
  }
  share <- subset(result$issues, check_id == "share_identity" & row == 1)
  testthat::expect_equal(as.numeric(share$observed), 0.4)
  testthat::expect_equal(as.numeric(share$expected), 0.75)
  testthat::expect_equal(share$difference, -0.35)
  testthat::expect_equal(share$percentile, "p0p50")
  data <- finished_dina_fixture()
  data$average[[2L]] <- 3
  data$gini[[2L]] <- 0.2
  result <- finished_dina_validate(data, finished_dina_spec())
  testthat::expect_equal(sum(result$issues$check_id == "group_constants"), 2)
  testthat::expect_true(any(result$issues$check_id == "overall_mean_identity") || any(result$issues$check_id == "group_constants"))
})

testthat::test_that("all source concepts and units are checked while publication is labelled", {
  data <- do.call(rbind, lapply(c("nat", "pon"), function(step) {
    do.call(rbind, lapply(c("esn", "ind"), function(unit) {
      x <- finished_dina_fixture()
      x$step <- step
      x$unit <- unit
      x
    }))
  }))
  data$topsh[data$step == "pon" & data$unit == "ind"] <- 0
  result <- finished_dina_validate(data)
  testthat::expect_equal(nrow(result$coverage), 4)
  testthat::expect_equal(sum(result$coverage$publication), 1)
  testthat::expect_true(any(result$issues$check_id == "top_share_identity" & result$issues$step == "pon" & result$issues$unit == "ind"))
  testthat::expect_equal(sum(result$issues$check_id == "metadata"), 3)
  spec <- finished_dina_spec()
  spec$definitions$currency_price_basis <- "unconfirmed; unconfirmed"
  testthat::expect_true(any(finished_dina_validate(data, spec)$issues$field == "currency_price_basis"))
})

testthat::test_that("standard export grid is compatible without fitting or internal inputs", {
  p <- finished_dina_export_grid()
  upper <- c(tail(p, -1), 1)
  mean <- 1.5
  avg <- 1 + (p + upper) / 2
  shares <- avg * (upper - p) / mean
  data <- data.frame(
    percentile = paste0("p", format(p * 100, trim = TRUE, scientific = FALSE),
                        "p", format(upper * 100, trim = TRUE, scientific = FALSE)),
    step = "nat", unit = "esn", country = "AAA", year = 2022, gini = 1 / 9,
    average = mean, p = p, thr = 1 + p, avg = avg, bckt_sh = shares,
    topavg = rev(cumsum(rev(avg * (upper - p)))) / (1 - p),
    topsh = rev(cumsum(rev(shares)))
  )
  result <- finished_dina_validate(data, finished_dina_spec())
  testthat::expect_true(all(result$checks$status == "pass"))
  testthat::expect_equal(nrow(result$issues), 0)
  testthat::expect_equal(result$coverage$rows, 127)
})
