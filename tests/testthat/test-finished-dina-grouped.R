source(file.path(repo_root_for_tests, "code/R/source-diagnostics/finished_dina_grouped.R"))
source(file.path(repo_root_for_tests, "code/R/source-diagnostics/finished_dina_explorer.R"))

grouped_source_fixture <- function() {
  list(id = "bra-grouped", family = "dina", country = "BRA", method = "manual",
    inbox = "input_data/_new/dina/BRA.xlsx", canonical = "input_data/dina/BRA.xlsx",
    prepared = "input_data/dina/BRA_coarse.dta", integration = "coarse_external",
    finished_series = list(format = "wid_grouped_xlsx", version = "2026",
      first_year = 2000L, last_year = 2001L, prepared_step = "nat", prepared_unit = "esn",
      harmonization = list(share_normalization = "proportional"),
      percentiles = c("p0p50", "p50p90", "p90p100", "p99p100", "p99.9p100"),
      series = list(list(sheet = "BR-sptinc992j", code = "sptinc992j", measure = "share", unit = "fraction"),
        list(sheet = "BR-aptinc992j", code = "aptinc992j", measure = "average", unit = "2016 prices")),
      citation = "unconfirmed", doi = "", currency = "unconfirmed", population = "unconfirmed",
      price_basis = "2016 prices"))
}

grouped_rows_fixture <- function() {
  source <- grouped_source_fixture()
  do.call(rbind, lapply(source$finished_series$series, function(series) {
    do.call(rbind, lapply(2000:2001, function(year) {
      percentile <- source$finished_series$percentiles
      lower <- c(0, .5, .9, .99, .999); upper <- c(.5, .9, 1, 1, 1)
      share <- c(.15, .35, .5, .2, .08)
      value <- if (series$measure == "share") share else share / (upper - lower) * 100
      data.frame(row_type = "supplied", country = "BRA", year = year, step = "ptinc", unit = series$unit,
        wid_code = series$code, measure = series$measure, percentile = percentile, p_lower = lower,
        p_upper = upper, value = value, data_quality = 4, deflator = if (series$measure == "average") 95 else NA_real_,
        source_sheet = series$sheet, source_row = year - 1998L, source_column = percentile,
        requires_harmonization = 1L, publication_ready = 0L, stringsAsFactors = FALSE)
    }))
  }))
}

grouped_include_fixture <- function(root) {
  source_cli_for_tests()
  skip_if_not_installed("haven"); skip_if_not_installed("digest")
  skip_if_not_installed("gpinter")
  dir.create(file.path(root, "input_data/_new/dina"), recursive = TRUE)
  dir.create(file.path(root, "input_data/dina"), recursive = TRUE)
  for (file in c("finished_dina_explorer.R", "finished_dina_preparer.R", "finished_dina_grouped.R")) {
    file.copy(file.path(repo_root_for_tests, "code/R/source-diagnostics", file),
      file.path(root, "code/R/source-diagnostics", file))
  }
  source <- list(id = "aaa-finished", family = "dina", country = "AAA", method = "manual",
    inbox = "input_data/_new/dina/AAA.dta", canonical = "input_data/dina/AAA.dta",
    prepared = "input_data/dina/AAA_prepared.dta", integration = "prepared_external",
    finished_series = list(version = "v1", first_year = 2022L, last_year = 2022L,
      steps = "nat", units = "esn", publication_step = "nat", publication_unit = "esn",
      citation = "Fixture researchers", doi = "https://doi.org/fixture"))
  rows <- data.frame(country = "AAA", year = 2022, step = "nat", unit = "esn", gini = .25,
    percentile = c("p0p50", "p50p100"), p = c(0, .5), thr = c(0, 100),
    avg = c(50, 150), average = 100, bckt_sh = c(.25, .75), topsh = c(1, .75), topavg = c(100, 150))
  haven::write_dta(rows, file.path(root, source$inbox))
  engine <- dina_finished_include_engine(root); engine$gpinter_version <- "fixture"; engine$calls <- 0L
  engine$finished_dina_prepare <- function(data, specification) {
    engine$calls <- engine$calls + 1L
    list(data = data, validation = engine$finished_dina_validate(data, specification),
      summary = data.frame(country = "AAA", year = 2022, step = "nat", unit = "esn",
        prepared_brackets = 2L, threshold_repairs = 0, threshold_nudges = 0, pooled_bins = 0,
        boundary_changes = 0, max_mean_preservation_relative_error = 0), recipe = list(id = "fixture"))
  }
  list(sources = list(source), engine = engine)
}

grouped_scientific_hashes <- function(root) {
  files <- sort(list.files(file.path(root, "input_data"), full.names = TRUE, recursive = TRUE))
  setNames(lapply(files, dina_hash_file), files)
}

test_that("coarse preparation subtracts nested shares and preserves supplied monetary observations", {
  source <- grouped_source_fixture(); rows <- grouped_rows_fixture()
  prepared <- finished_dina_grouped_coarse(rows, source)
  original <- prepared$data[prepared$data$row_type == "supplied", names(rows)]
  expect_equal(original, rows)
  b <- prepared$data[prepared$data$row_type == "bracket" & prepared$data$year == 2000, ]
  expect_equal(b$percentile, c("p0p50", "p50p90", "p90p99", "p99p99.9", "p99.9p100"))
  expect_equal(b$bracket_share, c(.15, .35, .3, .12, .08))
  expect_equal(b$avg_norm, b$bracket_share / (b$p_upper - b$p_lower))
  expect_equal(b$subtract_percentile, c("", "", "p99p100", "p99.9p100", ""))
  expect_true(all(prepared$data$requires_harmonization))
  expect_false(any(prepared$data$publication_ready))
  expect_equal(prepared$summary$population_covered, c(1, 1))
  expect_equal(prepared$summary$uncovered_ranges, c("", ""))
  expect_false(any(prepared$validation$checks$status %in% c("review", "fail")))
  skip_if_not_installed("haven")
  path <- tempfile(fileext = ".dta")
  haven::write_dta(prepared$data, path, version = 14)
  serialized <- haven::read_dta(path)
  expect_true(isTRUE(all.equal(as.data.frame(serialized), prepared$data, check.attributes = FALSE)))
  expect_false(any(finished_dina_grouped_validate(serialized, source)$checks$status == "fail"))
  serialized$avg_norm[serialized$row_type == "bracket"][1L] <- 999
  expect_true(any(finished_dina_grouped_validate(serialized, source)$checks$status == "fail"))
})

test_that("coarse data retain zero, missing years, uncovered population and inconsistent totals", {
  rows <- grouped_rows_fixture(); source <- grouped_source_fixture()
  rows$value[rows$year == 2001] <- NA_real_
  rows$value[rows$measure == "share" & rows$percentile == "p0p50" & rows$year == 2000] <- 0
  rows$value[rows$measure == "share" & rows$percentile == "p50p90" & rows$year == 2000] <- NA_real_
  p <- finished_dina_grouped_coarse(rows, source)
  b <- p$data[p$data$row_type == "bracket", ]
  expect_equal(b$bracket_share[b$percentile == "p0p50"], 0)
  expect_equal(nrow(b), 4L)
  expect_equal(p$summary$population_covered, c(.6, 0))
  expect_equal(p$summary$uncovered_ranges, c("50–90", "0–100"))
  expect_equal(p$summary$eligible_for_gpinter, c(0L, 0L))
  expect_true(any(p$validation$checks$check_id == "grouped_coverage" & p$validation$checks$status == "warning"))
  rows <- grouped_rows_fixture()
  rows$value[rows$measure == "share" & rows$percentile == "p0p50"] <- .16
  p <- finished_dina_grouped_coarse(rows, source)
  expect_equal(p$summary$share_total, c(1.01, 1.01))
  expect_equal(p$data$bracket_share[p$data$row_type == "bracket" & p$data$percentile == "p0p50"], c(.16, .16))
  expect_true(any(p$validation$checks$check_id == "share_total" & p$validation$checks$status == "review"))
  expect_true(any(p$validation$checks$check_id == "grouped_mean_identity" & p$validation$checks$status == "review"))
})

test_that("grouped structural failures block preparation and scientific discrepancies remain explicit", {
  rows <- grouped_rows_fixture(); source <- grouped_source_fixture()
  expect_error(finished_dina_grouped_coarse(rbind(rows, rows[1, ]), source), "structural")
  rows$year[1] <- 1999
  expect_error(finished_dina_grouped_coarse(rows, source), "structural")
  rows$year[1] <- NA_real_
  expect_error(finished_dina_grouped_coarse(rows, source), "structural")
  rows <- grouped_rows_fixture(); rows$value[1] <- Inf
  expect_error(finished_dina_grouped_coarse(rows, source), "structural")
  rows <- grouped_rows_fixture()
  rows$value[rows$measure == "share" & rows$percentile == "p99p100"] <- .6
  p <- finished_dina_grouped_coarse(rows, source)
  expect_true(any(p$validation$checks$check_id == "top_share_order" & p$validation$checks$status == "review"))
  expect_true(any(p$data$bracket_share < 0, na.rm = TRUE))
  skip_if_not_installed("readxl")
  expect_error(finished_dina_grouped_read(readxl::readxl_example("datasets.xlsx"), source), "sheets")
})

test_that("supplied BRA workbook reads and validates without full-distribution requirements", {
  source_cli_for_tests()
  source <- Filter(function(s) s$id == "bra-morgan-souza-2026", dina_source_registry(repo_root_for_tests, family = "dina"))[[1]]
  path <- file.path(repo_root_for_tests, source$inbox)
  skip_if_not(file.exists(path), "BRA workbook is a local incoming source")
  rows <- finished_dina_grouped_read(path, source)
  p <- finished_dina_grouped_coarse(rows, source)
  expect_equal(nrow(rows), 1260L)
  expect_equal(sum(is.na(rows$value)), 360L)
  expect_equal(nrow(p$data[p$data$row_type == "bracket", ]), 450L)
  expect_false(any(p$validation$checks$status == "fail"))
  expect_false(any(p$validation$checks$check_id %in% c("gini_bounds", "export_grid", "threshold_order")))
  top1 <- p$validation$coverage[p$validation$coverage$wid_code == "sptinc992j" & p$validation$coverage$percentile == "p99p100", ]
  expect_equal(top1$n_years, 82L)
  expect_match(top1$internal_gaps, "1929,1930,1931,1932,1961,1962,1991,1994", fixed = TRUE)
  eligible <- p$summary$year[p$summary$eligible_for_gpinter == 1]
  expect_equal(eligible, setdiff(1976:2015, c(1991, 1994)))
  expect_true(all(p$summary$eligible_for_gpinter[p$summary$year < 1976] == 0))
  expect_equal(p$summary$population_scope[p$summary$year == 1926], "top_tail_only")
  expect_equal(p$summary$population_scope[p$summary$year == 1968], "top_tail_only")
  expect_true(all(p$data$interpolate_missing_years == 0 & p$data$extrapolate_years == 0))
  p$data$eligible_for_gpinter[p$data$year == 1926] <- 1L
  invalid <- finished_dina_grouped_validate(p$data, source)
  expect_true(any(invalid$checks$check_id == "schema" & invalid$checks$status == "fail"))
})

test_that("grouped policy never infers a full distribution from a tail or fills missing years", {
  source <- grouped_source_fixture(); rows <- grouped_rows_fixture()
  source$finished_series$harmonization <- list(first_full_year = 2001L)
  p <- finished_dina_grouped_coarse(rows, source)
  expect_equal(p$summary$eligible_for_gpinter, c(0L, 1L))
  rows$value[rows$percentile %in% c("p0p50", "p50p90")] <- NA_real_
  p <- finished_dina_grouped_coarse(rows, source)
  expect_equal(p$summary$population_scope, c("top_tail_only", "top_tail_only"))
  expect_true(all(p$summary$eligible_for_gpinter == 0L))
  expect_true(all(p$data$p_lower[p$data$row_type == "bracket"] >= .9))
  source$finished_series$harmonization$complete_partial_population <- TRUE
  expect_error(finished_dina_grouped_coarse(rows, source), "not implemented")
  source$finished_series$harmonization$complete_partial_population <- FALSE
  source$finished_series$harmonization$extrapolate_years <- TRUE
  expect_error(finished_dina_grouped_coarse(rows, source), "not implemented")
})

test_that("BRA GPinter preparation keeps full years and preserves normalized source shares", {
  skip_if_not_installed("gpinter")
  source <- grouped_source_fixture(); rows <- grouped_rows_fixture()
  rows$value[rows$measure == "share" & rows$percentile == "p0p50"] <- .16
  p <- finished_dina_grouped_prepare(rows, source)
  expect_equal(nrow(p$data), 254L)
  expect_true(all(table(p$data$year) == 127L))
  expect_true(all(p$data$average == 1))
  expect_true(all(p$data$requires_harmonization == 0))
  expect_false(any(p$validation$checks$status %in% c("fail", "review")))
  expect_true(all(p$summary$max_source_share_error < 1e-12))
  normalization <- p$changes[p$changes$rule == "proportional_share_normalization", ]
  expect_equal(normalization$after, normalization$before / 1.01)
  for (year in 2000:2001) {
    d <- p$data[p$data$year == year, ]
    expect_equal(sum(d$bckt_sh), 1)
    expect_equal(sum(d$avg * c(diff(d$p), 1 - tail(d$p, 1))), 1)
    expect_equal(sum(d$bckt_sh[d$p < .5]), .16 / 1.01)
  }
  skip_if_not_installed("haven")
  path <- tempfile(fileext = ".dta"); haven::write_dta(p$data, path, version = 14)
  saved <- as.data.frame(haven::read_dta(path))
  expect_false(any(finished_dina_grouped_validate_prepared(saved, source, rows)$checks$status == "fail"))
  saved$year[saved$year == 2000] <- 1999
  expect_true(any(finished_dina_grouped_validate_prepared(saved, source, rows)$checks$status == "fail"))
  rows$value[rows$year == 2000 & rows$percentile %in% c("p0p50", "p50p90")] <- NA_real_
  p <- finished_dina_grouped_prepare(rows, source)
  expect_equal(unique(p$data$year), 2001)
  expect_equal(p$coverage$included, c(0L, 1L))
  expect_equal(nrow(p$data), 127L)
  rows$value[rows$year == 2001] <- NA_real_
  expect_error(finished_dina_grouped_prepare(rows, source), "No observed full-population")
  rows <- grouped_rows_fixture()
  rows$value[rows$measure == "share" & rows$percentile == "p0p50"] <- 1.2
  expect_error(finished_dina_grouped_prepare(rows, source), "Invalid or nonnested")
})

test_that("mixed full and coarse inclusion remains reversible and coarse sources cannot publish", {
  source_cli_for_tests()
  source <- Filter(function(s) s$id == "bra-morgan-souza-2026", dina_source_registry(repo_root_for_tests, family = "dina"))[[1]]
  path <- file.path(repo_root_for_tests, source$inbox)
  skip_if_not(file.exists(path), "BRA workbook is a local incoming source")
  root <- mini_repo(); fixture <- grouped_include_fixture(root)
  file.copy(path, file.path(root, source$inbox))
  dina_write_yaml(list(sources = c(fixture$sources, list(source))), file.path(root, "config/sources.yml"))
  before <- grouped_scientific_hashes(root)
  capture.output(record <- dina_finished_explore(root))
  expect_equal(record$status, "needs_review")
  expect_equal(nrow(dina_review_table(record, "values")), 1260L)
  capture.output(view <- dina_finished_show_table(record, "values", country = "BRA", filters = list(year = 2015, "wid-code" = "sptinc992j", percentile = "p99p100")))
  expect_equal(nrow(view), 1L)
  expect_equal(view$percentile, "p99p100")
  # The original source format may legitimately have no DOI.
  registry <- dina_read_yaml(file.path(root, "config/sources.yml"))
  registry$sources[[1]]$finished_series$doi <- ""
  dina_write_yaml(registry, file.path(root, "config/sources.yml"))
  capture.output(record <- dina_finished_explore(root))
  expect_equal(record$status, "needs_review")
  capture.output(candidate <- dina_finished_include_prepare(root, record, engine = fixture$engine))
  expect_equal(candidate$status, "ready")
  coarse_coverage <- dina_finished_candidate_table(candidate, "coverage")
  expect_equal(nrow(coarse_coverage[coarse_coverage$country == "BRA", ]), 90L)
  expect_equal(fixture$engine$calls, 1L)
  # Source hashes were captured before the deliberate registry change; input files are unchanged.
  expect_equal(grouped_scientific_hashes(root), before)
  capture.output(dina_finished_include_print(record, candidate, root))
  record <- dina_review_read(root, "dina")
  capture.output(accepted <- dina_finished_include_publish(candidate, root, record,
    dina_finished_include_sources(root), fixture$engine))
  expect_equal(dina_hash_file(path), dina_hash_file(file.path(root, source$canonical)))
  coarse <- haven::read_dta(file.path(root, source$prepared))
  expect_true(all(coarse$requires_harmonization == 0))
  expect_false(any(coarse$publication_ready == 1))
  expect_equal(sort(unique(coarse$year)), setdiff(1976:2015, c(1991, 1994)))
  expect_true(all(table(coarse$year) == 127L))
  expect_length(dina_finished_publication_sources(root), 2L)
  expect_equal(dina_finished_publication_sources(root, source$id)[[1L]]$publication_years,
    setdiff(1976:2015, c(1991, 1994)))
  capture.output(dina_finished_include_restore(root, accepted$confirmation))
  expect_equal(grouped_scientific_hashes(root), before)
})

test_that("public grouped-only inclusion fits full-distribution years and tracks GPinter", {
  source_cli_for_tests()
  source <- Filter(function(s) s$id == "bra-morgan-souza-2026", dina_source_registry(repo_root_for_tests, family = "dina"))[[1]]
  path <- file.path(repo_root_for_tests, source$inbox)
  skip_if_not(file.exists(path), "BRA workbook is a local incoming source")
  root <- mini_repo(); grouped_include_fixture(root)
  # Retain only the coarse source and its real workbook in this disposable repo.
  unlink(file.path(root, "input_data/_new/dina/AAA.dta"))
  file.copy(path, file.path(root, source$inbox))
  dina_write_yaml(list(sources = list(source)), file.path(root, "config/sources.yml"))
  before <- grouped_scientific_hashes(root)
  explored <- run_dina_cli(c("sources", "explore", "dina"), root)
  expect_equal(explored$status, 0L, info = explored$output)
  included <- run_dina_cli(c("sources", "include", "dina", "--dry-run"), root)
  expect_equal(included$status, 0L, info = included$output)
  expect_match(included$output, "GPinter", fixed = TRUE)
  record <- dina_review_read(root, "dina")
  candidate <- dina_finished_include_candidate(root, record)
  expect_equal(candidate$status, "ready")
  expect_equal(candidate$identity$gpinter_version, as.character(utils::packageVersion("gpinter")))
  expect_equal(grouped_scientific_hashes(root), before)
  coverage <- run_dina_cli(c("sources", "table", "dina", "prepared-coverage", "--year", "2015"), root)
  expect_equal(coverage$status, 0L, info = coverage$output)
  expect_match(coverage$output, "share total", ignore.case = TRUE)
  impact <- run_dina_cli(c("sources", "table", "dina", "impact", "--year", "2015"), root)
  expect_equal(impact$status, 0L, info = impact$output)
  expect_match(impact$output, "source_normalization", fixed = TRUE)
  recipe <- file.path(root, "code/R/source-diagnostics/finished_dina_grouped.R")
  cat("\n# changed recipe\n", file = recipe, append = TRUE)
  expect_error(dina_finished_include_verify_candidate(candidate, record, root), "recipe.*changed")
  expect_error(dina_finished_include_verify_review(root, record, dina_finished_include_sources(root)), "changed")
})
