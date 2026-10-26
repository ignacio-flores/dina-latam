gpinter_tail_test_engine <- function() {
  env <- new.env(parent = globalenv())
  source(file.path(repo_root_for_tests, "code/R/gpinter_tail_repair.R"), local = env)
  source(file.path(repo_root_for_tests, "code/R/gpinter_precision.R"), local = env)
  source(file.path(repo_root_for_tests, "code/R/gpinter_thresholds.R"), local = env)
  env
}

gpinter_tail_test_grid <- function() {
  p <- c(seq(0, .99, .01), .999, .9999)
  avg <- c(seq_len(100), 200, 400)
  data.frame(p = p, avg = avg, thr = c(0, (head(avg, -1) + tail(avg, -1)) / 2),
    average = sum(diff(c(p, 1)) * avg))
}

gpinter_tail_test_fit <- function(e, repaired) {
  dist <- gpinter::shares_fit(repaired$data$p, bracketavg = repaired$data$avg,
    average = repaired$overall)
  tab <- gpinter::generate_tabulation(dist, fractiles = repaired$original$p,
    threshold = TRUE, bracketavg = TRUE, bracketshare = TRUE, topavg = TRUE, topshare = TRUE)
  expect_true(e$gpinter_tail_validate_fit(dist, tab, repaired))
  list(dist = dist, tab = tab)
}

test_that("ordered tail inputs are unchanged by the fallback", {
  e <- gpinter_tail_test_engine()
  d <- gpinter_tail_test_grid()
  r <- e$gpinter_tail_repair(d)
  expect_false(r$applied)
  expect_equal(r$data, d)
  expect_equal(r$original, d)
  expect_equal(nrow(r$adjustments), 0L)
})

test_that("p90--p99 consolidation preserves bottom bins and all broad tail incomes", {
  e <- gpinter_tail_test_engine()
  d <- gpinter_tail_test_grid()
  d$avg[d$p == .9] <- 88
  original <- d
  r <- e$gpinter_tail_repair(d)
  expect_true(r$applied)
  expect_equal(d, original)
  expect_equal(r$original, original)
  expect_equal(r$data$avg[r$data$p < .9], d$avg[d$p < .9])
  expect_false(any(r$data$p > .9 & r$data$p < .99))
  expect_true(abs(r$data$avg[r$data$p == .9] - mean(d$avg[d$p >= .9 & d$p < .99])) < 1e-10)
  removed <- r$adjustments[r$adjustments$field == "population_boundary", ]
  expect_equal(nrow(removed), 8L)
  expect_equal(removed$p, d$p[d$p > .9 & d$p < .99])
  expect_true(all(is.na(removed$after)))
  expect_true(all(grepl("preserve total p90", removed$reason)))
  expected <- vapply(c(.9, .99, .999, .9999), function(p) {
    sum(diff(c(d$p, 1))[d$p >= p] * d$avg[d$p >= p]) / sum(diff(c(d$p, 1)) * d$avg)
  }, numeric(1))
  expect_true(max(abs(r$targets$top_share - expected)) < 1e-12)
  fitted <- gpinter_tail_test_fit(e, r)
  expect_true(max(abs(fitted$tab$bracket_average[d$p < .9] - d$avg[d$p < .9])) < 1e-7)
})

test_that("only an infeasible top-0.01 share moves and its numerical margin is separate", {
  e <- gpinter_tail_test_engine()
  for (side in c("lower", "upper")) {
    d <- gpinter_tail_test_grid()
    if (side == "lower") d$avg[d$p == .9999] <- 150
    if (side == "upper") d$avg[d$p == .999] <- 80
    before <- d
    r <- e$gpinter_tail_repair(d)
    expect_equal(r$original, before)
    expect_equal(r$data$p, d$p)
    expect_equal(r$data$avg[r$data$p < .999], d$avg[d$p < .999])
    expect_true(all(diff(r$data$avg) > 0))
    expect_equal(r$adjustments$rule, c("tail_top001_feasibility", "tail_top001_interior_margin"))
    changes <- r$adjustments
    bound <- if (side == "lower") changes$lower_bound[1] else changes$upper_bound[1]
    expect_equal(changes$after[1], bound)
    expect_equal(changes$before[2], bound)
    expect_true(abs(changes$difference_pp[2]) < 1e-8)
    expect_true(all(abs(changes$difference_pp - 100 * (changes$after - changes$before)) < 1e-14))
    income <- diff(c(d$p, 1)) * d$avg
    old_shares <- vapply(c(.9, .99, .999), function(p) sum(income[d$p >= p]) / sum(income), numeric(1))
    expect_true(max(abs(r$targets$top_share[1:3] - old_shares)) < 1e-12)
    gpinter_tail_test_fit(e, r)
  }
})

test_that("feasible top-0.01 targets remain unchanged during p90 repair", {
  e <- gpinter_tail_test_engine()
  d <- gpinter_tail_test_grid()
  d$avg[d$p == .9] <- 88
  r <- e$gpinter_tail_repair(d)
  expect_equal(r$data$avg[r$data$p >= .99], d$avg[d$p >= .99])
  expect_false(any(r$adjustments$field == "top_share"))
})

test_that("the observed PER2006 and PER2020 reversal sizes can be repaired without lower-bin changes", {
  e <- gpinter_tail_test_engine()
  # The p89/p90 pairs are the actual cases also recorded in the precision tests;
  # the other bins are synthetic ordered anchors, not purported country results.
  for (pair in list(c(25929.44333, 23383.7653), c(49057.25221, 40659.026222))) {
    d <- gpinter_tail_test_grid()
    d$avg[d$p < .9] <- seq(pair[1] / 90, pair[1], length.out = sum(d$p < .9))
    d$avg[d$p >= .9 & d$p < .99] <- seq(pair[2], 2 * pair[1], length.out = 9)
    d$avg[d$p >= .99] <- pair[1] * c(4, 8, 40)
    r <- e$gpinter_tail_repair(d)
    expect_equal(r$data$avg[r$data$p < .9], d$avg[d$p < .9])
    expect_true(all(diff(r$data$avg) > 0))
    gpinter_tail_test_fit(e, r)
  }
})

test_that("incompatible protected anchors and unexpected reversals fail explicitly", {
  e <- gpinter_tail_test_engine()
  d <- gpinter_tail_test_grid()
  d$avg[d$p >= .9 & d$p < .99] <- seq(70, 78)
  expect_error(e$gpinter_tail_repair(d), "another protected income anchor")
  d <- gpinter_tail_test_grid()
  d$avg[30] <- d$avg[29] - 1
  expect_error(e$gpinter_tail_repair(d), "another protected income anchor")
  d <- gpinter_tail_test_grid()
  d$avg[d$p >= .999] <- c(50, 60)
  expect_error(e$gpinter_tail_repair(d), "without changing the protected top-1% or top-0.1%")
  d <- gpinter_tail_test_grid()
  d$avg[5] <- NA_real_
  expect_error(e$gpinter_tail_repair(d), "finite nonnegative bracket means")
})

test_that("the fallback does not consolidate equal-income atoms", {
  e <- gpinter_tail_test_engine()
  d <- gpinter_tail_test_grid()
  d$avg[1:2] <- 0
  r <- e$gpinter_tail_repair(d)
  expect_equal(r$data$p, d$p)
  expect_equal(r$data$avg, d$avg)
  expect_false(r$applied)
  # Arithmetic noise belongs to exact-atom representation, never to a new
  # scientific consolidation or a top-share target correction.
  d <- gpinter_tail_test_grid()
  d$avg[d$p == .9] <- d$avg[d$p == .89] - 1e-12
  r <- e$gpinter_tail_repair(d)
  expect_equal(r$data$p, d$p)
  expect_equal(r$data$avg, d$avg)
  expect_false(r$applied)
  # An exact terminal equality has a feasible inward direction in this case.
  d$avg[d$p == .9999] <- d$avg[d$p == .999]
  r <- e$gpinter_tail_repair(d)
  expect_equal(r$adjustments$rule, "tail_top001_interior_margin")
  expect_true(abs(r$adjustments$difference_pp) < 1e-8)
  expect_equal(r$data$avg[r$data$p < .999], d$avg[d$p < .999])
  # A terminal atom with no strictly feasible interval cannot be smoothed by
  # changing a protected broad share.
  d$avg[d$p >= .99] <- 100
  expect_error(e$gpinter_tail_repair(d), "No strictly feasible")
})

test_that("fit validation rejects changed retained anchors", {
  e <- gpinter_tail_test_engine()
  d <- gpinter_tail_test_grid()
  d$avg[d$p == .9] <- 88
  r <- e$gpinter_tail_repair(d)
  fitted <- gpinter_tail_test_fit(e, r)
  altered <- r
  altered$anchors$avg[10] <- altered$anchors$avg[10] + 1
  expect_error(e$gpinter_tail_validate_fit(fitted$dist, fitted$tab, altered), "retained bracket-income anchors")
  altered <- r
  altered$targets$top_share[2] <- altered$targets$top_share[2] + .01
  expect_error(e$gpinter_tail_validate_fit(fitted$dist, fitted$tab, altered), "protected broad tail shares")
  expect_true(e$gpinter_tail_validate_tab(fitted$tab, r))
  missing <- fitted$tab
  missing <- lapply(missing, function(x) x[-which(fitted$tab$fractile == .99)])
  expect_error(e$gpinter_tail_validate_tab(missing, r), "missing a retained population boundary")
  bad <- fitted$tab
  bad$bracket_average[10] <- bad$bracket_average[10] + 1
  expect_error(e$gpinter_tail_validate_tab(bad, r), "retained bracket-income anchors")
})
