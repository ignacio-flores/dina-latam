source(file.path(repo_root_for_tests, "code", "R", "source-diagnostics", "finished_dina_publication.R"))

publication_fixture <- function(country = "TST", year = 2022, shape = 1, scale = 100) {
  p <- c(seq(0, 0.99, 0.01), seq(0.991, 0.999, 0.001),
    seq(0.9991, 0.9999, 0.0001), seq(0.99991, 0.99999, 0.00001))
  p <- round(p * 1e5) / 1e5
  w <- diff(c(p, 1))
  avg <- seq(0.2, 15, length.out = length(p)) ^ shape
  thr <- c(0, (head(avg, -1) + tail(avg, -1)) / 2)
  mean <- sum(w * avg)
  data.frame(country = country, year = year, step = "nat", unit = "esn", p = p,
    avg = scale * avg, thr = scale * thr, average = scale * mean,
    bckt_sh = w * avg / mean, topsh = rev(cumsum(rev(w * avg / mean))),
    topavg = scale * rev(cumsum(rev(w * avg))) / (1 - p),
    gini = 0.45, fitted_gini = 0.451)
}

publication_spec <- function(countries = "TST", first = 2022, last = 2022) {
  list(countries = countries, years = list(first = first, last = last),
    publication = list(step = "nat", unit = "esn"), source_id = "generic-finished",
    citation = "Fixture author", doi = "fixture", version = "v1")
}

publication_run <- function(prepared = publication_fixture(), internal = rbind(
    publication_fixture(year = 2022), publication_fixture(year = 2023, shape = 1.05)),
    specification = publication_spec(), adjustment = "none", first_year = 2000,
    last_year = 2023, export_last_year = 2024) {
  finished_dina_publication_build(prepared, internal, specification,
    first_year, last_year, export_last_year, adjustment)
}

testthat::test_that("sparse observed history and fitted Gini retain their provenance through continuation", {
  years <- c(1976, 1990, 1992, 2015)
  prepared <- do.call(rbind, lapply(years, function(y) publication_fixture(year = y)))
  prepared$gini <- prepared$fitted_gini
  spec <- publication_spec(); spec$years <- years; spec$gini_kind <- "gpinter_fitted"
  spec$publication_policy <- list(continuation = "four_group_relative_changes", carry_forward = TRUE, include_history = TRUE)
  internal <- do.call(rbind, lapply(2015:2024, function(y) publication_fixture(year = y, shape = 1 + (y - 2015) * .002)))
  result <- publication_run(prepared, internal, spec, adjustment = "group_join", last_year = 2024, export_last_year = 2025)
  testthat::expect_true(result$valid)
  testthat::expect_equal(sort(unique(result$data$year)), c(years, 2016:2025))
  testthat::expect_false(any(result$data$year %in% c(1991, 1994)))
  supplied <- result$data[result$data$origin == "supplied", ]
  testthat::expect_true(all(is.na(supplied$supplied_gini)))
  testthat::expect_true(all(supplied$gini_method == "gpinter_fitted"))
  testthat::expect_true(all(supplied$gini == prepared$fitted_gini))
  testthat::expect_equal(result$data$avg[result$data$year == 2025], result$data$avg[result$data$year == 2024])
  testthat::expect_true(all(result$data$origin[result$data$year == 2025] == "carry_forward"))
  prepared$fitted_gini <- NA_real_
  invalid <- publication_run(prepared, internal, spec, adjustment = "group_join", last_year = 2024, export_last_year = 2025)
  testthat::expect_false(invalid$valid)
  testthat::expect_true(any(invalid$checks$check_id == "source_gini_provenance" & invalid$checks$status == "fail"))
})

testthat::test_that("source years and normalized shape are retained with transparent carry-forward", {
  source <- rbind(publication_fixture(year = 1999), publication_fixture(year = 2000))
  original <- source
  result <- publication_run(source, internal = data.frame(),
    specification = publication_spec(first = 1999, last = 2000),
    last_year = 2000, export_last_year = 2001)
  testthat::expect_true(result$valid)
  testthat::expect_identical(source, original)
  testthat::expect_equal(sort(unique(result$data$year)), 1999:2001)
  testthat::expect_equal(as.integer(table(result$data$year)), rep(127, 3))
  supplied <- subset(result$data, year == 1999)
  raw <- subset(source, year == 1999)
  testthat::expect_equal(supplied$avg, raw$avg / raw$average, tolerance = 1e-13)
  testthat::expect_equal(supplied$thr, raw$thr / raw$average, tolerance = 1e-13)
  testthat::expect_equal(supplied$bckt_sh, raw$bckt_sh, tolerance = 1e-13)
  testthat::expect_equal(unique(supplied$gini), 0.45)
  testthat::expect_equal(unique(supplied$fitted_gini), 0.451)
  testthat::expect_equal(unique(supplied$gini_method), "supplied")
  carry <- subset(result$data, year == 2001)
  testthat::expect_equal(carry$avg, subset(result$data, year == 2000)$avg)
  testthat::expect_true(all(carry$origin == "carry_forward" & carry$carried_from == 2000))
  testthat::expect_equal(unique(carry$gini_method), "supplied")
})

testthat::test_that("continuation implements four-group proportional growth and threshold factors exactly", {
  source <- publication_fixture(scale = 11)
  anchor <- publication_fixture(shape = 1, scale = 8)
  current <- publication_fixture(year = 2023, shape = 1.05, scale = 200)
  result <- publication_run(source, rbind(anchor, current))
  testthat::expect_true(result$valid)
  w <- diff(c(source$p, 1))
  group <- findInterval(source$p, c(0, .5, .9, .99, 1))
  internal_growth <- as.numeric(rowsum(w * current$avg / current$average, group)) /
    as.numeric(rowsum(w * anchor$avg / anchor$average, group))
  growth <- internal_growth[group]
  avg <- source$avg / source$average * growth
  norm <- sum(avg * w)
  continued <- subset(result$data, year == 2023)
  testthat::expect_equal(continued$avg, avg / norm, tolerance = 1e-13)
  testthat::expect_equal(continued$thr, source$thr / source$average * growth / norm, tolerance = 1e-13)
  testthat::expect_equal(sum(w * continued$avg), 1, tolerance = 1e-13)
  testthat::expect_equal(continued$bottomsh + continued$topsh, rep(1, 127), tolerance = 1e-13)
  testthat::expect_true(all(is.na(continued$supplied_gini)))
  testthat::expect_true(all(is.na(continued$fitted_gini)))
  testthat::expect_true(all(continued$gini_method == "grouped_lorenz_trapezoid"))
  carry <- subset(result$data, year == 2024)
  testthat::expect_equal(carry$avg, continued$avg)
  testthat::expect_equal(carry$thr, continued$thr)
  testthat::expect_true(all(result$impact$difference < 1e-12))
})

testthat::test_that("local join correction preserves every four-group income and the untouched tail", {
  source <- publication_fixture(shape = 0.1)
  anchor <- publication_fixture()
  current <- publication_fixture(year = 2023)
  # Both internal distributions stay ordered. Their small, uneven growth
  # nevertheless reverses the flatter supplied distribution at the join.
  current$avg[current$p < .5] <- current$avg[current$p < .5] * 1.01
  current$average <- sum(diff(c(current$p, 1)) * current$avg)
  raw <- publication_run(source, rbind(anchor, current), adjustment = "none")
  testthat::expect_false(raw$valid)
  testthat::expect_equal(nrow(raw$data), 127 * 3)
  testthat::expect_true(any(raw$problems$check_id == "mean_ordering"))
  corrected <- publication_run(source, rbind(anchor, current), adjustment = "group_join")
  testthat::expect_true(corrected$valid)
  testthat::expect_true(corrected$review_required)
  a <- subset(raw$data, year == 2023)
  b <- subset(corrected$data, year == 2023)
  w <- diff(c(a$p, 1))
  testthat::expect_equal(sum(w * a$avg), sum(w * b$avg), tolerance = 1e-13)
  testthat::expect_true(all(diff(b$avg) >= -1e-13))
  testthat::expect_true(any(diff(b$avg) == 0))
  testthat::expect_true(all(b$thr <= b$avg + 1e-13))
  testthat::expect_true(all(tail(b$thr, -1) >= head(b$avg, -1) - 1e-13))
  testthat::expect_equal(subset(raw$data, year == 2022), subset(corrected$data, year == 2022))
  testthat::expect_true(any(corrected$changes$rule == "review_group_join_means"))
  testthat::expect_true(any(corrected$changes$rule == "review_threshold_repair"))
  mean_impact <- subset(corrected$impact, year == 2023 & statistic == "average")
  testthat::expect_equal(mean_impact$difference, 0, tolerance = 1e-13)
  testthat::expect_equal(nrow(corrected$groups), 8)
  testthat::expect_true(all(corrected$groups$status == "pass"))
  testthat::expect_lt(max(abs(corrected$groups$income_difference)), 1e-12)
  testthat::expect_equal(b$avg[b$p >= .9], a$avg[a$p >= .9], tolerance = 0)
  for (cut in c(.5, .9, .99)) {
    testthat::expect_equal(sum(b$bckt_sh[b$p < cut]), sum(a$bckt_sh[a$p < cut]), tolerance = 1e-12)
  }
  testthat::expect_true(any(grepl("window", corrected$changes$reason[corrected$changes$rule == "review_group_join_means"])))
})

testthat::test_that("missing cases and invalid denominators fail with precise diagnostics", {
  missing <- publication_run(internal = publication_fixture())
  testthat::expect_false(missing$valid)
  testthat::expect_true(any(missing$problems$year == 2023 & missing$problems$check_id == "internal_current_values", na.rm = TRUE))
  testthat::expect_true(any(missing$problems$check_id == "complete_coverage"))
  testthat::expect_false(any(missing$data$year > 2022))
  anchor <- publication_fixture(); anchor$average <- 0
  zero <- publication_run(internal = rbind(anchor, publication_fixture(year = 2023)))
  testthat::expect_false(zero$valid)
  testthat::expect_true(any(zero$problems$check_id == "internal_anchor_values"))
  wrong_grid <- publication_fixture(); wrong_grid$p[[20]] <- wrong_grid$p[[19]]
  duplicate <- publication_run(prepared = wrong_grid)
  testthat::expect_false(duplicate$valid)
  testthat::expect_true(any(duplicate$problems$check_id == "supplied_grid"))
  wrong_share <- publication_fixture(); wrong_share$bckt_sh[[2]] <- 0.1
  identity <- publication_run(prepared = wrong_share)
  testthat::expect_false(identity$valid)
  testthat::expect_true(any(identity$problems$check_id == "supplied_bckt_sh_identity"))
  bad_config <- publication_run(export_last_year = 2022)
  testthat::expect_false(bad_config$valid)
  testthat::expect_equal(bad_config$problems$check_id, "configuration")
})

testthat::test_that("internal zero-income bins are retained while group denominators remain valid", {
  anchor <- publication_fixture()
  current <- publication_fixture(year = 2023)
  # Mirror a legitimate zero-income bottom tail without changing any source
  # bins or substituting invented positive income for the internal zeros.
  for (name in c("anchor", "current")) {
    x <- get(name)
    x$avg[x$p < .18] <- 0
    x$thr[x$p <= .18] <- 0
    x$average <- sum(diff(c(x$p, 1)) * x$avg)
    assign(name, x)
  }
  internal <- rbind(anchor, current)
  result <- publication_run(internal = internal, adjustment = "group_join")
  testthat::expect_true(result$valid)
  testthat::expect_equal(subset(result$data, year == 2023)$avg,
    subset(result$data, year == 2022)$avg, tolerance = 1e-12)
  testthat::expect_true(all(result$groups$status == "pass"))
  testthat::expect_true(any(grepl("18 zero-income brackets retained", result$checks$detail)))
  testthat::expect_equal(internal$avg[internal$p < .18], rep(0, 36))

  # Independent nominal rescaling of the internal years cannot change the
  # normalized continuation; WID monetary scaling belongs to final export.
  scaled <- internal
  factor <- ifelse(scaled$year == 2022, 23, .007)
  for (field in c("avg", "thr", "average")) scaled[[field]] <- scaled[[field]] * factor
  again <- publication_run(internal = scaled, adjustment = "group_join")
  testthat::expect_true(again$valid)
  testthat::expect_equal(again$data$avg, result$data$avg, tolerance = 1e-12)
  testthat::expect_equal(again$data$thr, result$data$thr, tolerance = 1e-12)

  anchor$avg[anchor$p < .5] <- 0
  anchor$thr[anchor$p <= .5] <- 0
  anchor$average <- sum(diff(c(anchor$p, 1)) * anchor$avg)
  invalid <- publication_run(internal = rbind(anchor, current), adjustment = "group_join")
  testthat::expect_false(invalid$valid)
  testthat::expect_true(any(invalid$problems$check_id == "group_growth_factors"))

  negative <- internal; negative$avg[[1]] <- -1
  rejected <- publication_run(internal = negative, adjustment = "group_join")
  testthat::expect_false(rejected$valid)
  testthat::expect_true(any(rejected$problems$check_id == "internal_anchor_values"))
})

testthat::test_that("country selection and input order do not alter generic computation", {
  source <- rbind(publication_fixture("TST"), publication_fixture("ABC", shape = 1.1))
  internal <- rbind(publication_fixture("TST"), publication_fixture("TST", 2023),
    publication_fixture("ABC"), publication_fixture("ABC", 2023))
  all <- publication_run(source, internal, publication_spec(c("TST", "ABC")))
  testthat::expect_true(all$valid)
  one <- publication_run(source, internal, publication_spec("ABC"))
  testthat::expect_true(one$valid)
  expected <- subset(all$data, country == "ABC"); rownames(expected) <- NULL
  testthat::expect_equal(one$data, expected)
  shuffled <- publication_run(source[rev(seq_len(nrow(source))), ],
    internal[rev(seq_len(nrow(internal))), ], publication_spec(c("TST", "ABC")))
  testthat::expect_equal(shuffled$data, all$data)
  float <- source
  float$p <- as.numeric(readBin(writeBin(as.numeric(float$p), raw(), size = 4), "numeric", n = nrow(float), size = 4))
  stored <- publication_run(float, internal, publication_spec(c("TST", "ABC")))
  testthat::expect_true(stored$valid)
  testthat::expect_equal(stored$data, all$data)
  extra_unit <- source; extra_unit$unit <- "ind"; extra_unit$avg <- NA_real_
  filtered <- publication_run(rbind(source, extra_unit), internal, publication_spec(c("TST", "ABC")))
  testthat::expect_equal(filtered$data, all$data)
})


testthat::test_that("local constrained projection is bounded, deterministic and handles equal plateaus", {
  fit <- finished_dina_group_join(c(1, 3, 2, 4), rep(.25, 4), c(1, 1, 2, 2))
  testthat::expect_equal(fit$avg, c(1.5, 2.5, 2.5, 3.5), tolerance = 1e-12)
  testthat::expect_equal(fit$windows$first, 1)
  testthat::expect_equal(fit$windows$last, 4)
  again <- finished_dina_group_join(c(1, 3, 2, 4), rep(.25, 4), c(1, 1, 2, 2))
  testthat::expect_identical(fit, again)
  flat <- finished_dina_group_join(c(1, 3, 1, 3), rep(.25, 4), c(1, 1, 2, 2))
  testthat::expect_equal(flat$avg, rep(2, 4), tolerance = 1e-12)
  testthat::expect_error(finished_dina_group_join(c(3, 4, 1, 2), rep(.25, 4), c(1, 1, 2, 2)), "Group target means are reversed")
  testthat::expect_error(finished_dina_group_join(c(2, 1, 3, 4), rep(.25, 4), c(1, 1, 2, 2)), "inside a scientific group")
  testthat::expect_error(finished_dina_group_join(c(1, 3, 2, 4), rep(.25, 4), c(1, 1, 2, 2), max_iterations = 1L), "did not converge")
  unchanged <- finished_dina_group_join(c(1, 2, 2, 3), rep(.25, 4), c(1, 1, 2, 2))
  testthat::expect_equal(unchanged$avg, c(1, 2, 2, 3), tolerance = 0)
  testthat::expect_equal(nrow(unchanged$windows), 0)
})

testthat::test_that("within-group internal reshuffling does not change a group continuation", {
  source <- publication_fixture()
  anchor <- publication_fixture()
  current <- publication_fixture(year = 2023)
  # Population widths are equal here; transfer income between two bins without
  # changing any of the four internal group totals.
  current$avg[20:21] <- current$avg[20:21] + c(0.1, -0.1)
  current$average <- sum(diff(c(current$p, 1)) * current$avg)
  result <- publication_run(source, rbind(anchor, current), adjustment = "group_join")
  testthat::expect_true(result$valid)
  selected <- subset(result$data, year == 2023)
  testthat::expect_equal(selected$avg, subset(result$data, year == 2022)$avg, tolerance = 1e-12)
  testthat::expect_equal(result$groups$growth_factor, rep(1, 8), tolerance = 1e-12)
  testthat::expect_equal(result$groups$population_mass, rep(c(.5, .4, .09, .01), 2), tolerance = 1e-12)
  testthat::expect_false(any(result$changes$rule == "review_group_join_means"))
  testthat::expect_error(publication_run(adjustment = "isotonic"), "obsolete isotonic")
})
