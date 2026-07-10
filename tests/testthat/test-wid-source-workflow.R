source(file.path(repo_root_for_tests, "code", "R", "source-diagnostics", "wid_include.R"))

wid_fixture_write_dta <- function(root, rel, data) {
  path <- file.path(root, rel)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  haven::write_dta(data, path)
  path
}

wid_fixture_raw_rows <- function(countries, years, variables, offset = 0, percentile = NA_character_, age = NA_character_, pop = NA_character_) {
  grid <- expand.grid(
    country = countries,
    year = years,
    variable = variables,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  grid <- grid[order(match(grid$country, countries), grid$year, match(grid$variable, variables)), , drop = FALSE]
  grid$value <- 1000 + offset + 100 * match(grid$country, countries) + 10 * (grid$year - min(years)) + match(grid$variable, variables)
  grid$percentile <- percentile
  grid$age <- age
  grid$pop <- pop
  rownames(grid) <- NULL
  grid
}

wid_fixture_raw_set <- function(offset = 0, invalid = NULL) {
  countries <- c("AR", "MX")
  years <- 2000:2001
  raw <- list(
    population = wid_fixture_raw_rows(countries, years, c("npopul999i", "npopul992i"), offset = offset, age = "999", pop = "i"),
    `wid-macro` = wid_fixture_raw_rows(countries, years, c("mnninc999i", "mgdpro999i", "npopul999i", "agninc992i"), offset = offset),
    `wid-public-spending` = wid_fixture_raw_rows(countries, years, c("meduge999i", "mheage999i", "mexpgo999i", "mcongo999i", "mgdpro999i"), offset = offset),
    `wid-prices-xrates` = wid_fixture_raw_rows(countries, years, c("inyixx999i", "xlceup999i"), offset = offset),
    `wid-export-scaling` = wid_fixture_raw_rows(countries, years, c("anninc992i", "anninc999i", "xlceup999i", "xlceux999i"), offset = offset, percentile = "p0p100"),
    `wid-export-sptinc-check` = wid_fixture_raw_rows(countries, years, c("sptinc992j"), offset = offset, percentile = "p0p20", age = "992")
  )

  if (identical(invalid, "missing_indicator")) {
    raw$`wid-prices-xrates` <- raw$`wid-prices-xrates`[raw$`wid-prices-xrates`$variable != "xlceup999i", , drop = FALSE]
  } else if (identical(invalid, "missing_year")) {
    raw$`wid-public-spending` <- raw$`wid-public-spending`[raw$`wid-public-spending`$year != 2001L, , drop = FALSE]
  } else if (identical(invalid, "duplicate_keys")) {
    raw$`wid-macro` <- rbind(raw$`wid-macro`, raw$`wid-macro`[1L, , drop = FALSE])
  } else if (identical(invalid, "malformed")) {
    raw$`wid-export-scaling`$variable <- NULL
  } else if (identical(invalid, "fetch_failure")) {
    raw$`wid-export-sptinc-check` <- NULL
  }
  raw
}

wid_fixture_contract <- function() {
  list(
    version = 2L,
    source_type = "wid",
    source_ids = c("population", "wid-macro", "wid-public-spending", "wid-prices-xrates", "wid-export-scaling", "wid-export-sptinc-check"),
    explore_output_root = "output/experiments/wid_explore",
    output_root = "output/experiments/wid_include",
    years = list(first = 2000L, from_config = "config/dina.yml"),
    area_sets = list(
      project_latam = c("AR", "MX"),
      export_countries = c("AR", "MX"),
      population_legacy = c("AR", "MX")
    ),
    area_metadata = list(
      list(wid_area = "AR", iso3 = "ARG", country_name = "Argentina", region = "South America"),
      list(wid_area = "MX", iso3 = "MEX", country_name = "Mexico", region = "Central America")
    ),
    artifacts = list(
      population = list(
        canonical = "input_data/wid/population_total_adult_npopul.dta",
        raw = "input_data/wid/raw_population_npopul_ages_999_992.dta",
        type = "population_total_adult",
        output_map = list(totalpop = "npopul999i", adultpop = "npopul992i"),
        request = list(indicators = "npopul", area_set = "population_legacy", ages = c("999", "992"), pop = "i", first_year = 2000L),
        schema = list(required_columns = c("country", "region", "year", "totalpop", "adultpop"), key_columns = c("country", "year"), numeric_columns = c("totalpop", "adultpop"), required_years = "config")
      ),
      `wid-macro` = list(
        canonical = "input_data/wid/macro_national_accounts_indicators.dta",
        raw = "input_data/wid/raw_macro_national_accounts_indicators_ages_999_992.dta",
        type = "raw_subset",
        request = list(indicators = c("mnninc", "mgdpro", "npopul", "agninc"), area_set = "project_latam", ages = c("999", "992"), first_year = 2000L),
        schema = list(required_columns = c("country", "variable", "year", "value"), key_columns = c("country", "variable", "year"), numeric_columns = "value", required_years = "config")
      ),
      `wid-public-spending` = list(
        canonical = "input_data/wid/public_spending_gdp_shares.dta",
        raw = "input_data/wid/raw_public_spending_indicators.dta",
        type = "public_spending_gdp_shares",
        output_map = list(denominator = "mgdpro", shares = list(con = "mcongo", edu = "meduge", exp = "mexpgo", hea = "mheage"), source = "WID_web"),
        derive = list(min_year = 2000L),
        request = list(indicators = c("meduge", "mheage", "mexpgo", "mcongo", "mgdpro"), area_set = "project_latam", first_year = 2000L),
        schema = list(required_columns = c("iso", "year", "source", "exp", "con", "hea", "edu"), key_columns = c("iso", "year"), numeric_columns = c("exp", "con", "hea", "edu"), required_years = "configured_window", allow_missing_numeric = TRUE)
      ),
      `wid-prices-xrates` = list(
        canonical = "input_data/wid/prices_deflator_ppp_eur.dta",
        raw = "input_data/wid/raw_prices_inyixx_xlceup.dta",
        type = "prices_deflator_ppp",
        output_map = list(defl_xxxx = "inyixx", xppp_eur = "xlceup"),
        request = list(indicators = c("inyixx", "xlceup"), area_set = "project_latam", first_year = 2000L),
        schema = list(required_columns = c("country", "countrycode", "year", "defl_xxxx", "xppp_eur"), key_columns = c("country", "year"), numeric_columns = c("defl_xxxx", "xppp_eur"), required_years = "configured_window")
      ),
      `wid-export-scaling` = list(
        canonical = "input_data/wid/export_scaling_anninc_xrates.dta",
        raw = "input_data/wid/raw_export_anninc_xlceup_xlceux_p0p100.dta",
        type = "export_scaling",
        output_map = list(anninc992i = "anninc992", nninc_lcu_constc = c("anninc999", "anninc"), ppp_eur = "xlceup", mer_eur = "xlceux"),
        request = list(indicators = c("anninc", "xlceup", "xlceux"), area_set = "export_countries", perc = "p0p100", first_year = 2000L),
        schema = list(required_columns = c("iso", "year", "anninc992i", "nninc_lcu_constc", "ppp_eur", "mer_eur"), key_columns = c("iso", "year"), numeric_columns = c("anninc992i", "nninc_lcu_constc", "ppp_eur", "mer_eur"), required_years = "configured_window")
      ),
      `wid-export-sptinc-check` = list(
        canonical = "input_data/wid/export_comparison_sptinc_p0p20.dta",
        raw = "input_data/wid/raw_export_sptinc_p0p20_age_992.dta",
        type = "export_sptinc_check",
        output_map = list(country = "country", year = "year", widcode = "variable", p = "percentile", value_web = "value"),
        derive = list(min_year = 2000L),
        request = list(indicators = "sptinc", area_set = "export_countries", perc = "p0p20", ages = "992", first_year = 2000L),
        schema = list(required_columns = c("country", "year", "widcode", "p", "value_web"), key_columns = c("country", "year", "widcode", "p"), numeric_columns = "value_web", required_years = "configured_window")
      )
    ),
    statuses = list(blocked = c("blocked_fetch_failed", "blocked_read_failed", "blocked_schema", "blocked_empty_source", "blocked_duplicate_keys", "blocked_missing_values", "blocked_missing_required_years", "blocked_derivation_failed", "blocked_copy_failed"))
  )
}

wid_fixture_write_raw_fixtures <- function(fixture_dir, raw_set) {
  dir.create(fixture_dir, recursive = TRUE, showWarnings = FALSE)
  for (source_id in names(raw_set)) {
    haven::write_dta(raw_set[[source_id]], file.path(fixture_dir, paste0(source_id, ".dta")))
  }
}

wid_fixture_write_current_artifacts <- function(root, offset = 0) {
  contract <- wid_include_read_contract(root)
  raw_set <- wid_fixture_raw_set(offset = offset)
  for (source_id in names(raw_set)) {
    artifact <- wid_include_artifact(contract, source_id)
    years <- wid_include_years(root, contract, artifact)
    candidate <- wid_include_derive_artifact(raw_set[[source_id]], artifact, years)
    wid_fixture_write_dta(root, artifact$canonical, candidate)
    wid_fixture_write_dta(root, artifact$raw, raw_set[[source_id]])
  }
}

wid_fixture_mark_current_stale <- function(root) {
  contract <- wid_include_read_contract(root)
  for (artifact in wid_include_artifacts(contract)) {
    path <- file.path(root, artifact$canonical)
    if (file.exists(path)) Sys.setFileTime(path, as.POSIXct("2001-01-01", tz = "UTC"))
  }
}

wid_fixture_fetch_explore <- function(root) {
  run_wid_explorer(root = root, write_outputs = TRUE, fetch = TRUE)
}

wid_fixture_root <- function(current = FALSE, current_offset = 0, incoming_offset = 100, invalid = NULL) {
  skip_if_not_installed("haven")
  skip_if_not_installed("yaml")
  skip_if_not_installed("withr")
  root <- tempfile("wid-fixture-")
  fixture_dir <- tempfile("wid-api-fixtures-")
  dir.create(root, recursive = TRUE)
  withr::defer(unlink(root, recursive = TRUE), envir = parent.frame())
  withr::defer(unlink(fixture_dir, recursive = TRUE), envir = parent.frame())
  withr::local_envvar(c(DINA_WID_FIXTURE_DIR = fixture_dir), .local_envir = parent.frame())

  dir.create(file.path(root, "config"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(root, "code", "R", "source-diagnostics"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(root, "output"), recursive = TRUE, showWarnings = FALSE)
  for (module in c("wid_common.R", "wid_explorer.R", "wid_include.R")) {
    file.copy(
      file.path(repo_root_for_tests, "code", "R", "source-diagnostics", module),
      file.path(root, "code", "R", "source-diagnostics", module),
      overwrite = TRUE
    )
  }
  writeLines("display \"fixture\"", file.path(root, "_config.do"))
  dina_write_yaml(list(
    project = list(name = "wid-mini"),
    countries = c("ARG", "MEX"),
    years = list(first = 2000L, last = 2001L)
  ), file.path(root, "config", "dina.yml"))
  dina_write_yaml(wid_fixture_contract(), file.path(root, "config", "wid_include.yml"))
  dina_write_yaml(list(sources = list(
    list(id = "population", family = "wid", country = "MULTI", method = "wid", integration = "sources_explore_include_wid"),
    list(id = "wid-macro", family = "wid", country = "MULTI", method = "wid", integration = "sources_explore_include_wid"),
    list(id = "wid-public-spending", family = "wid", country = "MULTI", method = "wid", integration = "sources_explore_include_wid"),
    list(id = "wid-prices-xrates", family = "prices", country = "MULTI", method = "wid", integration = "sources_explore_include_wid"),
    list(id = "wid-export-scaling", family = "wid", country = "MULTI", method = "wid", integration = "sources_explore_include_wid"),
    list(id = "wid-export-sptinc-check", family = "wid", country = "MULTI", method = "wid", integration = "sources_explore_include_wid"),
    list(id = "wid-population-survey-adjustment", family = "wid", country = "ARG", method = "wid", covered_by = "population", integration = "sources_explore_include_wid")
  )), file.path(root, "config", "sources.yml"))
  dina_write_yaml(list(tasks = list()), file.path(root, "config", "pipeline.yml"))
  dina_write_yaml(list(items = list()), file.path(root, "config", "todo.yml"))

  if (isTRUE(current)) wid_fixture_write_current_artifacts(root, offset = current_offset)
  wid_fixture_write_raw_fixtures(fixture_dir, wid_fixture_raw_set(offset = incoming_offset, invalid = invalid))
  root
}

test_that("WID split modules expose stable public functions", {
  explorer_env <- new.env(parent = globalenv())
  source(file.path(repo_root_for_tests, "code", "R", "source-diagnostics", "wid_explorer.R"), local = explorer_env)
  expect_true(exists("run_wid_explorer", envir = explorer_env, mode = "function"))

  include_env <- new.env(parent = globalenv())
  source(file.path(repo_root_for_tests, "code", "R", "source-diagnostics", "wid_include.R"), local = include_env)
  expect_true(exists("run_wid_explorer", envir = include_env, mode = "function"))
  expect_true(exists("run_wid_include", envir = include_env, mode = "function"))
  expect_true(exists("wid_include_confirm_sources", envir = include_env, mode = "function"))
  expect_true(exists("wid_include_restore_sources", envir = include_env, mode = "function"))
})

test_that("WID area sets and output mappings are read from config", {
  contract <- wid_fixture_contract()
  expect_equal(wid_include_areas("project_latam", contract), c("AR", "MX"))
  contract$area_sets$project_latam <- "ZZ"
  expect_equal(wid_include_areas("project_latam", contract), "ZZ")

  artifact <- wid_include_artifact(wid_fixture_contract(), "population")
  expect_equal(wid_include_output_map(artifact)$totalpop, "npopul999i")
  expect_match(wid_include_mapping_summary(wid_include_output_map(artifact)), "totalpop=npopul999i")
})

test_that("WID explorer reports missing artifacts without fetching", {
  root <- wid_fixture_root(current = FALSE)
  result <- run_wid_explorer(root = root, write_outputs = TRUE)
  expect_equal(result$status, "review")
  expect_true(all(result$outputs$wid_artifact_status$status == "missing_current_artifact"))
  expect_true("wid_request_plan" %in% names(result$outputs))
  expect_true(file.exists(file.path(root, "output", "experiments", "wid_explore", "tables", "wid_request_plan.csv")))
  expect_equal(nrow(result$outputs$wid_request_plan), 6L)
  expect_true(any(grepl("totalpop=npopul999i", result$outputs$wid_request_plan$mapping_summary, fixed = TRUE)))
  expect_equal(result$outputs$wid_request_plan$area_set[result$outputs$wid_request_plan$source_id == "population"], "population_legacy")
  expect_true(any(result$outputs$review_actions$next_command == "dina sources explore wid --fetch"))
  expect_equal(nrow(result$outputs$unsupported_sources), 0L)
  expect_false(dir.exists(file.path(root, "output", "experiments", "wid_include")))
  expect_false(dir.exists(file.path(root, "output", "experiments", "wid_explore", "staged_repo")))
})

test_that("WID explorer reports stale local artifacts", {
  root <- wid_fixture_root(current = TRUE, current_offset = 0)
  canonical <- file.path(root, "input_data", "wid", "population_total_adult_npopul.dta")
  Sys.setFileTime(canonical, as.POSIXct("2001-01-01", tz = "UTC"))
  result <- run_wid_explorer(root = root, write_outputs = FALSE)
  population_status <- result$outputs$wid_artifact_status[result$outputs$wid_artifact_status$source_id == "population", , drop = FALSE]
  expect_true("stale" %in% population_status$status)
  expect_true(any(result$outputs$review_actions$source_id == "population" & result$outputs$review_actions$next_command == "dina sources explore wid --fetch"))
})

test_that("WID explorer fetch fills _new first-time artifacts without promoting", {
  root <- wid_fixture_root(current = FALSE, incoming_offset = 100)
  canonical <- file.path(root, "input_data", "wid", "population_total_adult_npopul.dta")
  incoming <- file.path(root, "input_data", "_new", "wid", "population_total_adult_npopul.dta")
  expect_false(file.exists(canonical))
  expect_false(file.exists(incoming))
  fetch <- wid_fixture_fetch_explore(root)
  expect_equal(wid_include_manifest_value(fetch$manifest, "status"), "fetched")
  expect_false(file.exists(canonical))
  expect_true(file.exists(incoming))
  expect_equal(sum(fetch$outputs$promotion_plan$artifact_type == "derived"), 6L)
  expect_equal(sum(fetch$outputs$promotion_plan$artifact_type == "raw"), 6L)
  expect_true(all(fetch$outputs$wid_artifact_comparison$comparison_status == "no_current_artifact"))
  expect_true(file.exists(file.path(root, "input_data", "_new", "wid", "prices_deflator_ppp_eur.dta")))
  expect_true(any(fetch$outputs$source_inventory$source_set == "_new"))
  expect_false(dir.exists(file.path(root, "output", "experiments", "wid_explore", "staged_repo")))
})

test_that("WID include dry-run consumes _new WID artifacts without promoting", {
  root <- wid_fixture_root(current = FALSE, incoming_offset = 100)
  canonical <- file.path(root, "input_data", "wid", "population_total_adult_npopul.dta")
  incoming <- file.path(root, "input_data", "_new", "wid", "population_total_adult_npopul.dta")
  expect_false(file.exists(canonical))
  wid_fixture_fetch_explore(root)
  expect_true(file.exists(incoming))
  include <- run_wid_include(root = root, write_outputs = TRUE, run_id = "wid-first")
  expect_equal(wid_include_manifest_value(include$manifest, "status"), "all_good")
  expect_false(file.exists(canonical))
  expect_equal(sum(include$outputs$promotion_plan$artifact_type == "derived"), 6L)
  expect_equal(sum(include$outputs$promotion_plan$artifact_type == "raw"), 6L)
  expect_true(all(include$outputs$wid_artifact_comparison$comparison_status == "no_current_artifact"))
  expect_true(file.exists(file.path(include$paths$staged_repo, "input_data", "wid", "prices_deflator_ppp_eur.dta")))
})

test_that("WID confirm promotes first-time artifacts and restore removes them", {
  root <- wid_fixture_root(current = FALSE, incoming_offset = 100)
  wid_fixture_fetch_explore(root)
  include <- run_wid_include(root = root, write_outputs = TRUE, run_id = "wid-confirmable")
  confirm <- wid_include_confirm_sources(root = root, include_run = include$paths$root)
  promoted <- file.path(root, "input_data", "wid", "population_total_adult_npopul.dta")
  expect_true(file.exists(promoted))
  expect_true(all(confirm$outputs$promote_report$promote_status == "staged"))
  restore <- wid_include_restore_sources(root = root, confirm_run = confirm$paths$root)
  expect_false(file.exists(promoted))
  expect_true(all(restore$outputs$restore_report$restore_status == "removed_promoted_destination"))
})

test_that("Later WID include compares candidates against existing artifacts", {
  root <- wid_fixture_root(current = TRUE, current_offset = 0, incoming_offset = 100)
  wid_fixture_mark_current_stale(root)
  wid_fixture_fetch_explore(root)
  include <- run_wid_include(root = root, write_outputs = TRUE, run_id = "wid-later")
  expect_equal(wid_include_manifest_value(include$manifest, "status"), "all_good")
  expect_true(all(include$outputs$wid_artifact_comparison$comparison_status == "compared"))
  expect_true(any(include$outputs$wid_numeric_comparison$abs_diff != 0))
})

test_that("WID include blocks malformed source responses and invalid derived artifacts", {
  scenarios <- c(
    missing_indicator = "blocked_missing_values",
    missing_year = "blocked_missing_required_years",
    duplicate_keys = "blocked_duplicate_keys",
    malformed = "blocked_derivation_failed",
    fetch_failure = "blocked_fetch_failed"
  )
  for (scenario in names(scenarios)) {
    root <- wid_fixture_root(current = FALSE, invalid = scenario)
    fetch <- wid_fixture_fetch_explore(root)
    expect_equal(wid_include_manifest_value(fetch$manifest, "status"), "blocked", info = scenario)
    include <- run_wid_include(root = root, write_outputs = TRUE, run_id = paste0("wid-", scenario))
    expect_equal(wid_include_manifest_value(include$manifest, "status"), "blocked", info = scenario)
    expect_true(scenarios[[scenario]] %in% include$outputs$include_detail$status, info = scenario)
  }
})

test_that("WID fetch failure leaves existing _new artifacts unchanged", {
  root <- wid_fixture_root(current = FALSE, incoming_offset = 100)
  first <- wid_fixture_fetch_explore(root)
  expect_equal(wid_include_manifest_value(first$manifest, "status"), "fetched")
  incoming <- file.path(root, "input_data", "_new", "wid", "prices_deflator_ppp_eur.dta")
  before_hash <- wid_include_hash_path(incoming)

  wid_fixture_write_raw_fixtures(Sys.getenv("DINA_WID_FIXTURE_DIR"), wid_fixture_raw_set(offset = 999, invalid = "missing_indicator"))
  failed <- wid_fixture_fetch_explore(root)
  expect_equal(wid_include_manifest_value(failed$manifest, "status"), "blocked")
  expect_equal(wid_include_hash_path(incoming), before_hash)
})

test_that("WID confirm refuses changed staged artifacts", {
  root <- wid_fixture_root(current = FALSE, incoming_offset = 100)
  wid_fixture_fetch_explore(root)
  include <- run_wid_include(root = root, write_outputs = TRUE, run_id = "wid-fingerprint")
  staged <- include$outputs$promotion_plan$from_rel[include$outputs$promotion_plan$artifact_type == "derived"][[1L]]
  staged_data <- haven::read_dta(staged)
  numeric_col <- names(staged_data)[vapply(staged_data, is.numeric, logical(1L))][[1L]]
  staged_data[[numeric_col]][[1L]] <- staged_data[[numeric_col]][[1L]] + 1
  haven::write_dta(staged_data, staged)
  expect_error(
    wid_include_confirm_sources(root = root, include_run = include$paths$root),
    "changed since dry-run"
  )
})

test_that("main dina CLI dispatches WID explore, include, and table previews", {
  root <- wid_fixture_root(current = FALSE, incoming_offset = 100)
  explore <- run_dina_cli(c("sources", "explore", "wid"), root = root)
  expect_equal(explore$status, 0L)
  expect_match(explore$output, "WID Explore")
  expect_match(explore$output, "population")
  expect_match(explore$output, "dina sources explore wid --fetch")

  include <- run_dina_cli(c("sources", "include", "wid", "--dry-run"), root = root)
  expect_equal(include$status, 0L)
  expect_match(include$output, "WID Include")
  expect_match(include$output, "dina sources explore wid --fetch")

  fetched <- run_dina_cli(c("sources", "explore", "wid", "--fetch"), root = root)
  expect_equal(fetched$status, 0L)
  expect_match(fetched$output, "Overall status: fetched")
  expect_match(fetched$output, "Incoming WID candidates")
  expect_match(fetched$output, "not promoted yet")
  expect_match(fetched$output, "Raw WID extracts present")
  expect_false(grepl("wid-macro\\s+raw\\s+_new", fetched$output))
  expect_match(fetched$output, "dina sources include wid --dry-run")

  include <- run_dina_cli(c("sources", "include", "wid", "--dry-run"), root = root)
  expect_equal(include$status, 0L)
  expect_match(include$output, "WID Include")
  expect_match(include$output, "No production files changed")

  run_wid_include(root = root, write_outputs = TRUE, run_id = "wid-cli-table")
  table <- run_dina_cli(c("sources", "table", "wid", "wid_artifact_comparison", "--run", "wid-cli-table", "--limit", "2"), root = root)
  expect_equal(table$status, 0L)
  expect_match(table$output, "WID Table: wid_artifact_comparison")
  expect_match(table$output, "comparison_status")
})
