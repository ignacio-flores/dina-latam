source(file.path(repo_root_for_tests, "code", "R", "source-diagnostics", "wid_include.R"))

wid_fixture_data <- function(years = 1990:1992, offset = 0, countries = c("Argentina", "Mexico")) {
  regions <- c(Argentina = "South America", Mexico = "Central America")
  grid <- expand.grid(
    country = countries,
    year = years,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  grid <- grid[order(match(grid$country, countries), grid$year), , drop = FALSE]
  country_index <- match(grid$country, countries)
  grid$region <- unname(regions[grid$country])
  grid$totalpop <- 1000000 + 10000 * country_index + 100 * (grid$year - min(years)) + offset
  grid$adultpop <- 600000 + 8000 * country_index + 80 * (grid$year - min(years)) + offset
  grid[, c("country", "region", "year", "totalpop", "adultpop")]
}

wid_fixture_write_dta <- function(root, rel, data) {
  path <- file.path(root, rel)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  haven::write_dta(data, path)
  path
}

wid_fixture_root <- function(incoming = "valid", current_offset = 0, incoming_offset = 100) {
  skip_if_not_installed("haven")
  skip_if_not_installed("yaml")
  root <- tempfile("wid-fixture-")
  dir.create(root, recursive = TRUE)
  if (requireNamespace("withr", quietly = TRUE)) {
    withr::defer(unlink(root, recursive = TRUE), envir = parent.frame())
  }
  dir.create(file.path(root, "config"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(root, "code", "R", "source-diagnostics"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(root, "code", "manual-downloaders"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(root, "input_data"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(root, "output"), recursive = TRUE, showWarnings = FALSE)
  file.copy(
    file.path(repo_root_for_tests, "code", "R", "source-diagnostics", "wid_include.R"),
    file.path(root, "code", "R", "source-diagnostics", "wid_include.R"),
    overwrite = TRUE
  )
  writeLines("display \"fixture\"", file.path(root, "_config.do"))
  dina_write_yaml(list(
    project = list(name = "wid-mini"),
    countries = c("ARG", "MEX"),
    years = list(first = 1990L, last = 1992L)
  ), file.path(root, "config", "dina.yml"))
  dina_write_yaml(list(
    version = 1L,
    source_type = "wid",
    source_ids = c("population"),
    explore_output_root = "output/experiments/wid_explore",
    output_root = "output/experiments/wid_include",
    years = list(first = 1990L, from_config = "config/dina.yml"),
    schema = list(population = list(
      required_columns = c("country", "region", "year", "totalpop", "adultpop"),
      key_columns = c("country", "year"),
      numeric_columns = c("totalpop", "adultpop")
    ))
  ), file.path(root, "config", "wid_include.yml"))
  dina_write_yaml(list(sources = list(
    list(
      id = "population",
      family = "wid",
      country = "MULTI",
      method = "wid",
      canonical = c("input_data/population/PopulationLatAm.dta"),
      inbox = c("input_data/_new/other/population/PopulationLatAm.dta"),
      destination = "input_data/population/{basename}",
      fetcher = "code/R/manual-downloaders/fetch_wid_population.R",
      fetch_target = "input_data/_new/other/population/PopulationLatAm.dta",
      notes = "Tiny WID population fixture."
    ),
    list(
      id = "wid-macro",
      family = "wid",
      country = "MULTI",
      method = "wid",
      integration = "none",
      notes = "Unsupported WID fixture source."
    )
  )), file.path(root, "config", "sources.yml"))
  dina_write_yaml(list(tasks = list()), file.path(root, "config", "pipeline.yml"))
  dina_write_yaml(list(items = list()), file.path(root, "config", "todo.yml"))

  current <- wid_fixture_data(offset = current_offset)
  incoming_data <- wid_fixture_data(offset = incoming_offset)
  if (identical(incoming, "schema")) {
    incoming_data$adultpop <- NULL
  } else if (identical(incoming, "duplicate")) {
    incoming_data <- rbind(incoming_data, incoming_data[1L, , drop = FALSE])
  } else if (identical(incoming, "missing_values")) {
    incoming_data$adultpop[2L] <- NA_real_
  } else if (identical(incoming, "missing_years")) {
    incoming_data <- incoming_data[!(incoming_data$country == "Mexico" & incoming_data$year == 1992L), , drop = FALSE]
  }
  wid_fixture_write_dta(root, "input_data/population/PopulationLatAm.dta", current)
  if (!identical(incoming, "missing")) {
    wid_fixture_write_dta(root, "input_data/_new/other/population/PopulationLatAm.dta", incoming_data)
  }
  root
}

test_that("WID explorer suggests fetch when incoming population is missing", {
  root <- wid_fixture_root(incoming = "missing")
  result <- run_wid_explorer(root = root, write_outputs = FALSE)
  expect_equal(result$status, "blocked")
  expect_true(any(result$outputs$validation_report$status == "blocked_missing_incoming_source"))
  expect_true(any(result$outputs$review_actions$next_command == "dina sources fetch population"))
})

test_that("WID explorer writes overlap percentage review tables for valid incoming data", {
  root <- wid_fixture_root()
  result <- run_wid_explorer(root = root, write_outputs = TRUE)
  expect_equal(result$status, "all_good")
  expect_true(file.exists(file.path(result$paths$tables, "overlap_differences.csv")))
  expect_true(file.exists(file.path(result$paths$tables, "overlap_summary.csv")))
  expect_true(file.exists(file.path(result$paths$tables, "overlap_year_summary.csv")))
  expect_true(all(c("totalpop_pct_diff", "adultpop_pct_diff") %in% names(result$outputs$overlap_differences)))
  expect_true(any(result$outputs$unsupported_sources$source_id == "wid-macro"))
})

test_that("WID include blocks invalid schema, duplicate keys, missing values, and missing required years", {
  for (scenario in c("schema", "duplicate", "missing_values", "missing_years")) {
    root <- wid_fixture_root(incoming = scenario)
    include <- run_wid_include(root = root, write_outputs = TRUE, run_id = paste0("wid-", scenario))
    expect_equal(wid_include_manifest_value(include$manifest, "status"), "blocked", info = scenario)
    expect_true(any(include$outputs$include_detail$severity == "blocked"), info = scenario)
    expect_equal(nrow(include$outputs$promotion_plan), 0L, info = scenario)
  }
})

test_that("WID include dry-run stages incoming population without changing canonical", {
  root <- wid_fixture_root(current_offset = 0, incoming_offset = 100)
  before <- haven::read_dta(file.path(root, "input_data", "population", "PopulationLatAm.dta"))
  include <- run_wid_include(root = root, write_outputs = TRUE, run_id = "wid-valid")
  after <- haven::read_dta(file.path(root, "input_data", "population", "PopulationLatAm.dta"))
  expect_equal(wid_include_manifest_value(include$manifest, "status"), "all_good")
  expect_equal(before$totalpop, after$totalpop)
  expect_equal(nrow(include$outputs$promotion_plan), 1L)
  expect_true(file.exists(include$outputs$promotion_plan$from_rel[[1L]]))
})

test_that("WID confirm promotes with backup and restore rolls back canonical", {
  root <- wid_fixture_root(current_offset = 0, incoming_offset = 100)
  include <- run_wid_include(root = root, write_outputs = TRUE, run_id = "wid-confirmable")
  confirm <- wid_include_confirm_sources(root = root, include_run = include$paths$root)
  promoted <- haven::read_dta(file.path(root, "input_data", "population", "PopulationLatAm.dta"))
  expect_equal(as.numeric(promoted$totalpop), wid_fixture_data(offset = 100)$totalpop)
  expect_true(any(confirm$outputs$promote_report$backup_status == "backed_up"))
  restore <- wid_include_restore_sources(root = root, confirm_run = confirm$paths$root)
  restored <- haven::read_dta(file.path(root, "input_data", "population", "PopulationLatAm.dta"))
  expect_equal(as.numeric(restored$totalpop), wid_fixture_data(offset = 0)$totalpop)
  expect_true(any(restore$outputs$restore_report$restore_status == "staged"))
})

test_that("main dina CLI dispatches WID explore, include, table, and fetch follow-up", {
  root <- wid_fixture_root()
  explore <- run_dina_cli(c("sources", "explore", "wid"), root = root)
  expect_equal(explore$status, 0L)
  expect_match(explore$output, "WID Explore")
  expect_match(explore$output, "population")
  expect_match(explore$output, "dina sources include wid --dry-run")

  table <- run_dina_cli(c("sources", "table", "wid", "overlap_summary", "--limit", "2"), root = root)
  expect_equal(table$status, 0L)
  expect_match(table$output, "WID Table: overlap_summary")
  expect_match(table$output, "totalpop_mean_pct_diff")

  include <- run_dina_cli(c("sources", "include", "wid", "--dry-run"), root = root)
  expect_equal(include$status, 0L)
  expect_match(include$output, "WID Include")
  expect_match(include$output, "No production files changed")

  fetch <- run_dina_cli(c("sources", "fetch", "wid", "--dry-run"), root = root)
  expect_equal(fetch$status, 0L)
  expect_match(fetch$output, "dina sources explore wid")
  expect_match(fetch$output, "dina sources include wid --dry-run")
})
