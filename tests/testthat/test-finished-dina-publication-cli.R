publication_cli_fixture <- function(root, countries = c("AAA", "BBB")) {
  skip_if_not_installed("haven"); skip_if_not_installed("digest")
  config <- dina_read_yaml(file.path(root, "config", "dina.yml"))
  config$countries <- countries
  dina_write_yaml(config, file.path(root, "config", "dina.yml"))
  dir.create(file.path(root, "input_data", "dina"), recursive = TRUE)
  dir.create(file.path(root, "input_data", "wid"), recursive = TRUE)
  dir.create(file.path(root, "output", "ineqstats"), recursive = TRUE)
  file.copy(file.path(repo_root_for_tests, "code", "R", "source-diagnostics", "finished_dina_publication.R"),
    file.path(root, "code", "R", "source-diagnostics", "finished_dina_publication.R"))
  p <- c(seq(0, .99, .01), seq(.991, .999, .001), seq(.9991, .9999, .0001), seq(.99991, .99999, .00001))
  p <- round(p * 1e5) / 1e5; q <- c(p[-1], 1); mass <- q - p
  sources <- list(); artifacts <- list(); internal <- list(); wid <- list(); areas <- list()
  for (i in seq_along(countries)) {
    country <- countries[i]; id <- paste0(tolower(country), "-supplied")
    source <- list(id = id, family = "dina", country = country, method = "manual",
      integration = "prepared_external", canonical = paste0("input_data/dina/", country, ".dta"),
      prepared = paste0("input_data/dina/", country, "_prepared.dta"),
      inbox = paste0("input_data/_new/dina/", country, ".dta"),
      finished_series = list(version = "v1", first_year = 2022L, last_year = 2022L,
        steps = "nat", units = "esn", publication_step = "nat", publication_unit = "esn",
        citation = "Fixture supplied distribution", doi = "https://doi.org/fixture",
        population = "unconfirmed", currency = "unconfirmed", price_basis = "unconfirmed"))
    avg <- 50 + 100 * (p + q) / 2
    shares <- mass * avg / 100
    rows <- data.frame(country = country, year = 2022L, step = "nat", unit = "esn",
      p = p, thr = 50 + 100 * p, avg = avg, average = 100, bckt_sh = shares,
      topsh = rev(cumsum(rev(shares))), gini = 1 / 6)
    rows$topavg <- rows$topsh * 100 / (1 - p)
    source_path <- file.path(root, source$prepared)
    haven::write_dta(rows, source_path)
    haven::write_dta(rows, file.path(root, source$canonical))
    sources[[i]] <- source
    artifacts[[length(artifacts) + 1L]] <- list(source_id = id, role = "prepared", destination = source_path,
      sha256 = dina_hash_file(source_path))
    original_path <- file.path(root, source$canonical)
    artifacts[[length(artifacts) + 1L]] <- list(source_id = id, role = "original", destination = original_path,
      sha256 = dina_hash_file(original_path))
    later <- rows; later$year <- 2023L; later$average <- 110
    later$avg <- later$avg * 1.1; later$thr <- later$thr * 1.1; later$topavg <- later$topavg * 1.1
    internal[[i]] <- rbind(rows, later)[, c("country", "year", "step", "unit", "p", "thr", "avg", "bckt_sh", "topavg", "topsh")]
    wid[[i]] <- data.frame(iso = paste0("X", i), year = 2022:2023, anninc992i = c(120, 130))
    areas[[i]] <- list(iso3 = country, wid_area = paste0("X", i))
  }
  dina_write_yaml(list(sources = sources), file.path(root, "config", "sources.yml"))
  dina_write_yaml(list(area_metadata = areas), file.path(root, "config", "wid_include.yml"))
  utils::write.csv(do.call(rbind, internal), file.path(root, "output", "ineqstats", "_gpinter_all_topcorrected.csv"), row.names = FALSE)
  haven::write_dta(do.call(rbind, wid), file.path(root, "input_data", "wid", "export_scaling_anninc_xrates.dta"))
  confirm <- file.path(root, "output", "experiments", "finished_dina_include", "confirms", "fixture")
  dina_write_json(list(family = "dina", status = "confirmed", run = confirm, artifacts = artifacts),
    file.path(confirm, "confirmation.json"))
  list(root = root, sources = sources)
}

test_that("export automatically integrates included sparse grouped history and validates scaling", {
  source_cli_for_tests(); root <- mini_repo(); fixture <- publication_cli_fixture(root, c("BRA", "ECU"))
  source <- fixture$sources[[1]]
  base <- as.data.frame(haven::read_dta(file.path(root, source$prepared)))
  supplied <- do.call(rbind, lapply(c(1976, 1990, 1992, 2015), function(year) {x <- base; x$year <- year; x}))
  supplied$requires_harmonization <- 0L; supplied$interpolate_missing_years <- 0L; supplied$extrapolate_years <- 0L
  supplied$fitted_gini <- supplied$gini
  haven::write_dta(supplied, file.path(root, source$prepared))
  source$canonical <- NULL
  meta <- source$finished_series
  meta$format <- "wid_grouped_xlsx"; meta$first_year <- 1926; meta$last_year <- 2015
  meta$harmonization <- list(first_full_year = 1976)
  meta$publication_policy <- list(continuation = "four_group_relative_changes", carry_forward = TRUE,
    include_history = TRUE, observation_years = "accepted_prepared", gini_kind = "gpinter_fitted", require_wid_scaling = TRUE)
  source$finished_series <- meta
  sources <- fixture$sources; sources[[1]] <- source
  dina_write_yaml(list(sources = sources), file.path(root, "config/sources.yml"))
  receipt_path <- file.path(root, "output/experiments/finished_dina_include/confirms/fixture/confirmation.json")
  receipt <- dina_read_json(receipt_path)
  for (i in seq_along(receipt$artifacts)) if (receipt$artifacts[[i]]$source_id == source$id && receipt$artifacts[[i]]$role == "prepared") {
    receipt$artifacts[[i]]$sha256 <- dina_hash_file(file.path(root, source$prepared))
  }
  dina_write_json(receipt, receipt_path)
  internal <- read.csv(file.path(root, "output/ineqstats/_gpinter_all_topcorrected.csv"))
  extra <- do.call(rbind, lapply(2015:2021, function(year) {x <- base; x$year <- year; x}))
  internal <- rbind(internal, extra[names(internal)])
  write.csv(internal, file.path(root, "output/ineqstats/_gpinter_all_topcorrected.csv"), row.names = FALSE)
  scale <- expand.grid(iso = c("X1", "X2"), year = 1976:2024, stringsAsFactors = FALSE)
  scale$anninc992i <- 100
  haven::write_dta(scale, file.path(root, "input_data/wid/export_scaling_anninc_xrates.dta"))
  capture.output(candidate <- dina_finished_publication_prepare(root))
  expect_equal(candidate$status, "ready")
  data <- read.csv(candidate$sources[[source$id]]$artifact)
  expect_equal(sort(unique(data$year)), c(1976, 1990, 1992, 2015:2024))
  expect_true(all(data$gini_method[data$origin == "supplied"] == "gpinter_fitted"))
  expect_true(all(is.na(data$supplied_gini[data$origin == "supplied"])))
  expect_length(dina_finished_publication_state(root)$active, 0L)
  capture.output(integrated <- dina_finished_publication_ensure(root))
  expect_setequal(unique(integrated$data$country), c("BRA", "ECU"))
  expect_identical(dina_finished_publication_state(root)$latest_candidate, candidate$run)
  expect_setequal(names(dina_finished_publication_state(root)$active), c("bra-supplied", "ecu-supplied"))
  expect_true(dina_finished_publication_verify(root, candidate))
  scale$anninc992i[scale$iso == "X1" & scale$year == 1976] <- NA_real_
  haven::write_dta(scale, file.path(root, "input_data/wid/export_scaling_anninc_xrates.dta"))
  capture.output(blocked <- dina_finished_publication_prepare(root))
  expect_equal(blocked$status, "blocked")
  expect_match(blocked$failures[[source$id]], "Missing required WID income benchmark", fixed = TRUE)
  expect_setequal(names(dina_finished_publication_state(root)$active), c("bra-supplied", "ecu-supplied"))
})

test_that("publication holds remove cached supplied contributions without changing accepted inputs", {
  source_cli_for_tests(); root <- mini_repo(); fixture <- publication_cli_fixture(root)
  capture.output(dina_finished_publication_ensure(root))
  before <- dina_finished_publication_state(root)
  paths <- unlist(lapply(fixture$sources, function(s) file.path(root, c(s$canonical, s$prepared))))
  hashes <- tools::md5sum(paths)
  registry <- dina_read_yaml(file.path(root, "config/sources.yml"))
  registry$sources[[1]]$finished_series$publication_policy <- list(
    status = "hold", hold_reason = "Scientific review pending.")
  dina_write_yaml(registry, file.path(root, "config/sources.yml"))
  expect_identical(dina_finished_publication_readiness(root)$ids, "bbb-supplied")
  expect_identical(dina_finished_publication_readiness(root)$status, "refresh_pending")
  expect_identical(names(dina_results_finished_publications(root)), "bbb-supplied")
  expect_error(dina_finished_publication_active(root), "on hold")
  expect_error(dina_finished_publication_prepare(root, list(source = "aaa-supplied")), "on hold")
  capture.output(updated <- dina_finished_publication_ensure(root))
  expect_identical(unique(updated$data$country), "BBB")
  expect_identical(names(dina_finished_publication_state(root)$active), "bbb-supplied")
  expect_true(file.exists(file.path(before$confirmation, "approved-state.json")))
  expect_identical(tools::md5sum(paths), hashes)
  expect_identical(dina_finished_publication_readiness(root)$status, "current")
  registry$sources[[2]]$finished_series$publication_policy <- list(status = "hold")
  dina_write_yaml(registry, file.path(root, "config/sources.yml"))
  expect_identical(dina_finished_publication_readiness(root)$status, "inactive")
  expect_length(dina_results_finished_publications(root), 0L)
  capture.output(empty <- dina_finished_publication_ensure(root))
  expect_equal(nrow(empty$data), 0L)
})

test_that("raw grouped comparison uses direct bracket shares without complementing inconsistent totals", {
  source_cli_for_tests()
  raw <- data.frame(year = c(2015, 2015, 2015, 2015),
    percentile = c("p0p50", "p90p100", "p99p100", "p99.9p100"), value = c(.14, .55, .28, .14))
  expect_equal(dina_finished_publication_plot_stat(raw, .5, TRUE)$value, .14)
  expect_equal(dina_finished_publication_plot_stat(raw, .9, TRUE)$value, .55)
  expect_equal(dina_finished_publication_plot_stat(raw, .99, TRUE)$value, .28)
})

test_that("export integrates all included sources without a separate approval and reuses current cache", {
  source_cli_for_tests(); root <- mini_repo(); fixture <- publication_cli_fixture(root)
  expect_length(dina_finished_publication_state(root)$active, 0L)
  expect_identical(dina_finished_publication_readiness(root)$status, "refresh_pending")
  capture.output(integrated <- dina_finished_publication_ensure(root))
  expect_setequal(unique(integrated$data$country), c("AAA", "BBB"))
  before <- dina_finished_publication_state(root)
  pointer_hash <- dina_hash_file(dina_finished_publication_pointer(root))
  expect_identical(dina_finished_publication_readiness(root)$status, "current")
  capture.output(current <- dina_finished_publication_ensure(root))
  expect_identical(dina_finished_publication_state(root), before)
  expect_identical(dina_hash_file(dina_finished_publication_pointer(root)), pointer_hash)
  expect_identical(current$data, integrated$data)
})

test_that("export refresh adds included sources and reuses a verified preview for its full scope", {
  source_cli_for_tests(); root <- mini_repo(); fixture <- publication_cli_fixture(root)
  capture.output(first <- dina_finished_publication_prepare(root, list(source = "aaa-supplied")))
  dina_finished_publication_activate(root, first)
  initial <- dina_finished_publication_active(root)
  source_paths <- unlist(lapply(fixture$sources, function(s) file.path(root, c(s$canonical, s$prepared))))
  source_hashes <- dina_review_hashes(source_paths)
  path <- file.path(root, "output", "ineqstats", "_gpinter_all_topcorrected.csv")
  rows <- utils::read.csv(path); selected <- rows$country == "AAA" & rows$year == 2023
  for (field in c("avg", "thr", "topavg")) rows[selected, field] <- rows[selected, field] * 1.02
  utils::write.csv(rows, path, row.names = FALSE)
  wid_path <- file.path(root, "input_data", "wid", "export_scaling_anninc_xrates.dta")
  wid <- haven::read_dta(wid_path); wid$anninc992i[wid$iso == "X1" & wid$year == 2023] <- 150
  haven::write_dta(wid, wid_path)
  state <- dina_finished_publication_state(root)
  expect_identical(dina_finished_publication_readiness(root)$status, "refresh_pending")
  expect_identical(dina_finished_publication_state(root), state)
  # A source already included belongs in the export even if the old cache omitted it.
  capture.output(extra <- dina_finished_publication_prepare(root))
  capture.output(refreshed <- dina_finished_publication_ensure(root))
  current <- dina_finished_publication_state(root)
  expect_setequal(names(current$active), c("aaa-supplied", "bbb-supplied"))
  expect_identical(current$active[[1]]$candidate, extra$run)
  expect_setequal(unique(refreshed$data$country), c("AAA", "BBB"))
  expect_equal(refreshed$data$avg[refreshed$data$country == "AAA"], initial$data$avg, tolerance = 1e-12)
  expect_identical(dina_review_hashes(source_paths), source_hashes)
  scale <- utils::read.csv(dina_finished_publication_candidate(root)$tables$scaling)
  expect_equal(scale$wid_mean[scale$country == "AAA" & scale$year == 2023], 150)
  # Refresh stays normalized: the ordinary Stata scaling remains its only WID multiplication.
  expect_equal(unique(refreshed$data$average), 1)
  wid$anninc992i[wid$iso == "X1" & wid$year == 2023] <- 160
  haven::write_dta(wid, wid_path)
  capture.output(reviewed <- dina_finished_publication_prepare(root, list(source = "aaa-supplied")))
  capture.output(dina_finished_publication_ensure(root))
  # A scoped preview cannot omit another included source from production.
  expect_false(identical(dina_finished_publication_state(root)$active[[1]]$candidate, reviewed$run))
  expect_setequal(names(dina_finished_publication_state(root)$active), c("aaa-supplied", "bbb-supplied"))
})

test_that("failed continuation refresh preserves the cache, accepted files and prior exports", {
  source_cli_for_tests(); root <- mini_repo(); fixture <- publication_cli_fixture(root, "AAA")
  capture.output(first <- dina_finished_publication_prepare(root))
  dina_finished_publication_activate(root, first)
  prior <- dina_finished_publication_state(root)
  export <- file.path(root, "output", "prior-export.csv"); writeLines("published", export)
  protected <- c(export, file.path(root, fixture$sources[[1]]$prepared),
    file.path(root, fixture$sources[[1]]$canonical))
  hashes <- dina_review_hashes(protected)
  path <- file.path(root, "output", "ineqstats", "_gpinter_all_topcorrected.csv")
  rows <- utils::read.csv(path); rows$avg[rows$year == 2023 & rows$p == 0] <- -1
  utils::write.csv(rows, path, row.names = FALSE)
  expect_error(capture.output(dina_finished_publication_ensure(root)), "refresh failed")
  after <- dina_finished_publication_state(root)
  expect_identical(after$active, prior$active)
  expect_identical(after$confirmation, prior$confirmation)
  expect_identical(dina_review_hashes(protected), hashes)
  expect_identical(dina_finished_publication_candidate(root)$status, "blocked")
})

test_that("planned inputs defer continuation checks without an extra source or recipe approval", {
  source_cli_for_tests(); root <- mini_repo(); fixture <- publication_cli_fixture(root, "AAA")
  capture.output(first <- dina_finished_publication_prepare(root))
  dina_finished_publication_activate(root, first)
  pointer_hash <- dina_hash_file(dina_finished_publication_pointer(root))
  path <- file.path(root, "output", "ineqstats", "_gpinter_all_topcorrected.csv")
  unlink(path)
  expect_identical(dina_finished_publication_readiness(root, internal_pending = TRUE)$status, "refresh_pending")
  expect_error(dina_finished_publication_readiness(root), "missing|Missing")
  registry <- dina_read_yaml(file.path(root, "config", "sources.yml"))
  registry$sources[[1]]$finished_series$population <- "new scientific definition"
  dina_write_yaml(registry, file.path(root, "config", "sources.yml"))
  expect_identical(dina_finished_publication_readiness(root, internal_pending = TRUE)$status, "refresh_pending")
  registry$sources[[1]]$finished_series$population <- "unconfirmed"
  dina_write_yaml(registry, file.path(root, "config", "sources.yml"))
  engine <- file.path(root, "code", "R", "source-diagnostics", "finished_dina_publication.R")
  cat("\nfinished_dina_publication_recipe <- function() list(version = 999)\n", file = engine, append = TRUE)
  expect_identical(dina_finished_publication_readiness(root, internal_pending = TRUE)$status, "refresh_pending")
  expect_identical(dina_hash_file(dina_finished_publication_pointer(root)), pointer_hash)
})

test_that("only included sources enter automatic integration and altered accepted files still fail", {
  source_cli_for_tests(); root <- mini_repo(); fixture <- publication_cli_fixture(root, "AAA")
  receipts <- file.path(root, "output/experiments/finished_dina_include/confirms")
  receipt_path <- file.path(receipts, "fixture/confirmation.json")
  receipt <- dina_read_json(receipt_path)
  unlink(receipts, recursive = TRUE)
  capture.output(empty <- dina_finished_publication_ensure(root))
  expect_equal(nrow(empty$data), 0L)
  expect_identical(dina_finished_publication_readiness(root)$status, "inactive")
  dina_write_json(receipt, receipt_path)
  capture.output(integrated <- dina_finished_publication_ensure(root))
  expect_equal(unique(integrated$data$country), "AAA")
  prepared <- file.path(root, fixture$sources[[1]]$prepared)
  cat("changed", file = prepared, append = TRUE)
  expect_error(dina_finished_publication_readiness(root, internal_pending = TRUE), "missing or changed")
})

test_that("re-included sources and updated metadata refresh automatically", {
  source_cli_for_tests(); root <- mini_repo(); fixture <- publication_cli_fixture(root, "AAA")
  capture.output(original <- dina_finished_publication_ensure(root))
  prepared <- file.path(root, fixture$sources[[1]]$prepared)
  data <- haven::read_dta(prepared); data$gini <- data$gini + 1e-4
  haven::write_dta(data, prepared)
  receipt_path <- file.path(root, "output/experiments/finished_dina_include/confirms/fixture/confirmation.json")
  receipt <- dina_read_json(receipt_path)
  receipt$artifacts[[1]]$sha256 <- dina_hash_file(prepared)
  dina_write_json(receipt, receipt_path)
  registry_path <- file.path(root, "config/sources.yml")
  registry <- dina_read_yaml(registry_path)
  registry$sources[[1]]$finished_series$citation <- "Updated source citation"
  dina_write_yaml(registry, registry_path)
  expect_identical(dina_finished_publication_readiness(root)$status, "refresh_pending")
  capture.output(updated <- dina_finished_publication_ensure(root))
  expect_true(all(updated$data$citation == "Updated source citation"))
  expect_equal(unique(updated$data$supplied_gini[updated$data$origin == "supplied"]), unique(data$gini))
  expect_identical(dina_finished_publication_readiness(root)$status, "current")
})

test_that("optional integration caching does not prompt for a second approval", {
  source_cli_for_tests(); root <- mini_repo(); publication_cli_fixture(root, "AAA")
  capture.output(state <- dina_finished_publication_include(root, input = function(...) stop("unexpected prompt"), is_terminal = TRUE))
  expect_identical(names(state$active), "aaa-supplied")
})

test_that("publication dry run is generic and needs accepted sources but no inbox or exploration", {
  source_cli_for_tests(); root <- mini_repo(); fixture <- publication_cli_fixture(root)
  source_before <- vapply(fixture$sources, function(s) dina_hash_file(file.path(root, s$prepared)), character(1))
  candidate <- NULL
  capture.output(candidate <- dina_finished_publication_include(root, list("dry-run" = TRUE), is_terminal = FALSE))
  expect_identical(candidate$status, "ready")
  expect_equal(names(candidate$sources), c("aaa-supplied", "bbb-supplied"))
  expect_length(dina_finished_publication_state(root)$active, 0L)
  expect_equal(vapply(fixture$sources, function(s) dina_hash_file(file.path(root, s$prepared)), character(1)), source_before)
  expect_equal(nrow(utils::read.csv(candidate$sources[[1]]$artifact)), 127 * 3)
  expect_equal(length(candidate$figures), 2)
  expect_true(all(file.exists(unlist(candidate$figures))))
  scale <- utils::read.csv(candidate$tables$scaling)
  expect_true(all(scale$status[scale$year == 2024] == "missing"))
  expect_true(all(is.na(scale$wid_mean[scale$year == 2024])))
  expect_no_error(dina_finished_publication_verify(root, dina_finished_publication_candidate(root)))
})

test_that("scoped approvals preserve other active sources and survive fresh source exploration", {
  source_cli_for_tests(); root <- mini_repo(); fixture <- publication_cli_fixture(root)
  capture.output(a <- dina_finished_publication_prepare(root, list(source = "aaa-supplied")))
  first <- dina_finished_publication_activate(root, a)
  expect_equal(unique(dina_finished_publication_active(root)$data$country), "AAA")
  capture.output(b <- dina_finished_publication_prepare(root, list(source = "bbb-supplied")))
  second <- dina_finished_publication_activate(root, b)
  expect_setequal(unique(dina_finished_publication_active(root)$data$country), c("AAA", "BBB"))
  dina_write_json(list(family = "dina", status = "needs_review"), dina_review_pointer(root, "dina"))
  expect_equal(nrow(dina_finished_publication_active(root)$attribution), 6L)
  capture.output(dina_finished_publication_restore(root, second$confirmation))
  expect_equal(unique(dina_finished_publication_active(root)$data$country), "AAA")
  capture.output(dina_finished_publication_restore(root, first$confirmation))
  expect_equal(nrow(dina_finished_publication_active(root)$data), 0L)
})

test_that("citation changes with an empty DOI preserve scientific values and Stata text attribution", {
  source_cli_for_tests(); root <- mini_repo(); fixture <- publication_cli_fixture(root, "AAA")
  capture.output(original <- dina_finished_publication_prepare(root))
  dina_finished_publication_activate(root, original)
  before <- dina_finished_publication_active(root)
  prepared_hash <- dina_hash_file(file.path(root, fixture$sources[[1]]$prepared))

  citation <- paste0("Nabernegg, M. (2026). Extreme Income Inequality in Ecuador-",
    "Dollarization, Commodity Price Boom, and Citizen Revolution.")
  registry <- dina_read_yaml(file.path(root, "config", "sources.yml"))
  registry$sources[[1]]$finished_series$citation <- citation
  registry$sources[[1]]$finished_series$doi <- ""
  dina_write_yaml(registry, file.path(root, "config", "sources.yml"))
  capture.output(candidate <- dina_finished_publication_prepare(root))
  expect_identical(candidate$status, "ready")
  dina_finished_publication_activate(root, candidate)
  active <- dina_finished_publication_active(root)
  expect_type(active$data$doi, "character")
  expect_true(all(active$data$doi == ""))
  expect_false(anyNA(active$data$doi))
  expect_true(all(active$data$citation == citation))
  expect_type(active$attribution$doi, "character")
  scientific <- setdiff(names(before$data), c("citation", "doi"))
  expect_identical(active$data[scientific], before$data[scientific])
  expect_identical(dina_hash_file(file.path(root, fixture$sources[[1]]$prepared)), prepared_hash)

  sys.source(file.path(repo_root_for_tests, "code", "R", "07d_prepare_finished_dina.R"),
    envir = environment())
  config <- dina_read_yaml(file.path(root, "config", "dina.yml"))
  directory <- file.path(root, "output", "stata-publication")
  dina_finished_stata_files(active, config, directory)
  attribution <- haven::read_dta(file.path(directory, "attribution.dta"))
  expect_type(attribution$pub_doi, "character")
  expect_true(all(attribution$pub_doi == ""))
  expect_false(anyNA(attribution$pub_doi))
  expect_true(all(attribution$pub_citation == citation))
})

test_that("publication review rejects relevant changes but ignores unrelated country updates", {
  source_cli_for_tests(); root <- mini_repo(); fixture <- publication_cli_fixture(root)
  capture.output(candidate <- dina_finished_publication_prepare(root, list(source = "aaa-supplied")))
  path <- file.path(root, "output", "ineqstats", "_gpinter_all_topcorrected.csv")
  before <- readLines(path); rows <- utils::read.csv(path)
  rows$avg[rows$country == "BBB"] <- rows$avg[rows$country == "BBB"] * 2
  utils::write.csv(rows, path, row.names = FALSE)
  expect_no_error(dina_finished_publication_verify(root, candidate))
  rows$avg[rows$country == "AAA" & rows$year == 2023] <- rows$avg[rows$country == "AAA" & rows$year == 2023] * 1.01
  utils::write.csv(rows, path, row.names = FALSE)
  expect_error(dina_finished_publication_verify(root, candidate), "relevant internal")
  writeLines(before, path)
  expect_no_error(dina_finished_publication_verify(root, candidate))
  new <- fixture$sources[[2]]; new$id <- "ccc-supplied"; new$country <- "CCC"
  registry <- dina_read_yaml(file.path(root, "config", "sources.yml"))
  registry$sources[[3]] <- new
  dina_write_yaml(registry, file.path(root, "config", "sources.yml"))
  expect_no_error(dina_finished_publication_verify(root, candidate))
  cat("tampered", file = candidate$sources[[1]]$artifact, append = TRUE)
  expect_error(dina_finished_publication_verify(root, candidate), "artifacts or review tables")
})

test_that("failed activation rolls back approvals and explicit mismatch is rejected", {
  source_cli_for_tests(); root <- mini_repo(); fixture <- publication_cli_fixture(root, "AAA")
  capture.output(candidate <- dina_finished_publication_prepare(root))
  pointer <- dina_finished_publication_pointer(root); before <- dina_hash_file(pointer)
  expect_error(dina_finished_publication_activate(root, candidate, save = function(value, path) {
    dina_finished_publication_save(value, path); stop("injected publication failure")
  }), "injected publication failure")
  expect_identical(dina_hash_file(pointer), before)
  expect_length(dina_finished_publication_state(root)$active, 0L)
  expect_error(dina_finished_publication_include(root, list(adjustment = "none", "include-run" = candidate$run), is_terminal = FALSE), "differs from the saved")
  expect_error(dina_finished_publication_include(root, list(source = "bbb-supplied", "include-run" = candidate$run), is_terminal = FALSE), "differ from the saved")
})

test_that("duplicate source groups and changed accepted files block publication", {
  source_cli_for_tests(); root <- mini_repo(); fixture <- publication_cli_fixture(root, "AAA")
  source <- fixture$sources[[1]]
  duplicate <- source; duplicate$id <- "other-aaa"
  registry <- list(sources = list(source, duplicate))
  dina_write_yaml(registry, file.path(root, "config", "sources.yml"))
  confirmation <- file.path(root, "output", "experiments", "finished_dina_include", "confirms", "fixture", "confirmation.json")
  receipt <- dina_read_json(confirmation)
  receipt$artifacts[[2]] <- receipt$artifacts[[1]]; receipt$artifacts[[2]]$source_id <- duplicate$id
  dina_write_json(receipt, confirmation)
  expect_error(dina_finished_publication_sources(root), "Overlapping")
  cat("changed", file = file.path(root, source$prepared), append = TRUE)
  expect_error(dina_finished_publication_sources(root, source$id), "missing or changed")
})

test_that("interrupted activation remains unusable and can restore its dead owner's backup", {
  source_cli_for_tests(); root <- mini_repo(); fixture <- publication_cli_fixture(root, "AAA")
  capture.output(candidate <- dina_finished_publication_prepare(root))
  state <- dina_finished_publication_activate(root, candidate)
  path <- file.path(state$confirmation, "confirmation.json")
  journal <- dina_read_json(path); journal$status <- "publishing"; journal$pid <- 999999L
  dina_write_json(journal, path)
  lock <- file.path(root, "output", "experiments", "finished_dina_publication", ".publishing")
  dir.create(lock)
  dina_write_json(list(pid = journal$pid, confirmation = state$confirmation), file.path(lock, "owner.json"))
  expect_error(dina_finished_publication_active(root), "activation did not complete")
  capture.output(dina_finished_publication_restore(root, state$confirmation))
  expect_length(dina_finished_publication_state(root)$active, 0L)
  expect_false(dir.exists(lock))
  expect_identical(dina_read_json(path)$status, "restored")
})

test_that("a new dry-run after activation can still restore the active generation", {
  source_cli_for_tests(); root <- mini_repo(); fixture <- publication_cli_fixture(root, "AAA")
  capture.output(first <- dina_finished_publication_prepare(root))
  state <- dina_finished_publication_activate(root, first)
  capture.output(second <- dina_finished_publication_prepare(root))
  expect_identical(dina_finished_publication_state(root)$latest_candidate, second$run)
  capture.output(dina_finished_publication_restore(root, state$confirmation))
  expect_length(dina_finished_publication_state(root)$active, 0L)
  expect_identical(dina_finished_publication_state(root)$latest_candidate, second$run)
})

test_that("publication summary separates proposed repairs from intended proportional changes", {
  source_cli_for_tests(); root <- mini_repo(); fixture <- publication_cli_fixture(root, "AAA")
  path <- file.path(root, "output", "ineqstats", "_gpinter_all_topcorrected.csv")
  internal <- utils::read.csv(path)
  use <- internal$year == 2023; p <- internal$p[use]; q <- c(p[-1], 1)
  internal$avg[use] <- 30 + 160 * (p + q) / 2
  internal$thr[use] <- 30 + 160 * p
  internal$bckt_sh[use] <- internal$avg[use] * (q - p) / 110
  internal$topsh[use] <- rev(cumsum(rev(internal$bckt_sh[use])))
  internal$topavg[use] <- internal$topsh[use] * 110 / (1 - p)
  utils::write.csv(internal, path, row.names = FALSE)
  capture.output(candidate <- dina_finished_publication_prepare(root))
  expect_identical(candidate$status, "ready")
  summary <- utils::read.csv(candidate$tables$summary)
  changes <- utils::read.csv(candidate$tables$changes)
  expect_gt(sum(changes$rule == "internal_group_relative_change"), 100)
  expect_true(all(summary$adjusted_means == 0))
  expect_true(all(summary$max_mean_change_percent == 0))
  expect_true(all(summary$raw_mean_reversals == 0))
  expect_equal(summary$threshold_changes[summary$year == 2023],
    sum(changes$year == 2023 & changes$variant == "group_join" & changes$rule %in%
      c("review_threshold_repair", "review_threshold_margin")))
})

test_that("publication warns about unresolved definitions and validates configured membership", {
  source_cli_for_tests(); root <- mini_repo(); fixture <- publication_cli_fixture(root, "AAA")
  capture.output(candidate <- dina_finished_publication_prepare(root))
  problems <- utils::read.csv(candidate$tables$problems)
  definitions <- problems[which(problems$check_id == "source_definition"), ]
  expect_setequal(definitions$field, c("population", "currency", "price_basis"))
  expect_true(all(definitions$status == "warning"))
  expect_match(definitions$detail[definitions$field == "population"], "does not establish compatibility")
  display <- paste(capture.output(dina_finished_publication_show(root, candidate = candidate)), collapse = "\n")
  expect_match(display, "Unresolved source definitions: 3")
  config <- dina_read_yaml(file.path(root, "config", "dina.yml"))
  config$countries <- c("AAA", "CCC")
  dina_write_yaml(config, file.path(root, "config", "dina.yml"))
  expect_no_error(dina_finished_publication_verify(root, candidate))
  config$countries <- "CCC"
  dina_write_yaml(config, file.path(root, "config", "dina.yml"))
  expect_error(dina_finished_publication_verify(root, candidate), "outside the configured country universe")
  expect_error(dina_finished_publication_prepare(root), "outside the configured country universe")
})

test_that("publication review exposes four group totals and refuses the obsolete method", {
  source_cli_for_tests(); root <- mini_repo(); fixture <- publication_cli_fixture(root, "AAA")
  expect_error(dina_finished_publication_prepare(root, list(adjustment = "isotonic")),
    "different continuation method")
  capture.output(candidate <- dina_finished_publication_prepare(root))
  expect_identical(candidate$adjustment, "group_join")
  expect_true("groups" %in% names(candidate$tables))
  expect_setequal(names(candidate$artifacts), c("aaa-supplied_raw", "aaa-supplied_group_join"))
  groups <- utils::read.csv(candidate$tables$groups)
  expect_setequal(unique(groups$variant), c("raw", "group_join"))
  expect_true(all(table(groups$year, groups$variant) == 4L))
  expect_true(all(groups$status == "pass"))
  expect_true(all(abs(groups$income_difference) <= 1e-12))
  summary <- utils::read.csv(candidate$tables$summary)
  expect_equal(summary$groups_checked[summary$origin != "supplied"], c(4L, 4L))
  expect_true(all(summary$group_income_failures == 0))
  display <- paste(capture.output(dina_finished_publication_show(root, "continuation-groups", candidate = candidate)), collapse = "\n")
  expect_match(display, "Target %", fixed = TRUE)
  expect_match(display, "four disjoint groups")
  capture.output(shown <- dina_finished_publication_show(root, "continuation-groups", candidate = candidate))
  expect_true(all(shown$variant == "group_join"))
  figure_copy <- unlist(dina_finished_publication_figure_paths(root, candidate))
  expect_true(all(file.exists(figure_copy)))
  expect_identical(dina_hash_file(figure_copy), dina_hash_file(candidate$figures[[1]]))
  historical <- candidate; historical$tables$groups <- NULL; historical$adjustment <- "isotonic"
  text <- paste(capture.output(dina_finished_publication_show(root, candidate = historical)), collapse = "\n")
  expect_match(text, "Historical candidate using the earlier bracket-level continuation")
  expect_error(dina_finished_publication_show(root, "continuation-groups", candidate = historical), "predates the four-group")
})


test_that("comparison reads the accepted original separately and filters the exact publication group", {
  source_cli_for_tests(); root <- mini_repo(); fixture <- publication_cli_fixture(root, "AAA")
  source <- fixture$sources[[1]]
  prepared <- as.data.frame(haven::read_dta(file.path(root, source$prepared)))
  original <- prepared
  original$topsh[original$p == .5] <- .82
  other_unit <- original; other_unit$unit <- "ind"; other_unit$topsh <- .1
  other_step <- original; other_step$step <- "pon"; other_step$topsh <- .2
  other_country <- original; other_country$country <- "BBB"; other_country$topsh <- .3
  haven::write_dta(rbind(original, other_unit, other_step, other_country), file.path(root, source$canonical))
  receipt_path <- file.path(root, "output", "experiments", "finished_dina_include", "confirms", "fixture", "confirmation.json")
  receipt <- dina_read_json(receipt_path)
  receipt$artifacts[[2]]$sha256 <- dina_hash_file(file.path(root, source$canonical))
  dina_write_json(receipt, receipt_path)
  read <- dina_finished_publication_original(root, source)
  expect_equal(nrow(read), 127L)
  expect_true(all(read$country == "AAA" & read$step == "nat" & read$unit == "esn"))
  expect_equal(read$topsh[read$p == .5], .82)
  expect_false(identical(read$topsh, prepared$topsh))
  haven::write_dta(prepared, file.path(root, source$canonical))
  changed <- dina_finished_publication_original(root, source)
  expect_equal(nrow(changed), 0L)
  expect_match(attr(changed, "unavailable_reason"), "accepted checksum")
  unlink(file.path(root, source$canonical))
  missing <- dina_finished_publication_original(root, source)
  expect_equal(nrow(missing), 0L)
  expect_match(attr(missing, "unavailable_reason"), "unavailable")
})

test_that("original comparison uses supplied cumulative shares instead of nominal width times mean", {
  source_cli_for_tests()
  # Labels cover the plotting cutoffs, but means and shares deliberately imply
  # different population masses. Multiplying nominal widths by means is wrong.
  original <- data.frame(year = 2022L, percentile = c("p0p50", "p50p90", "p90p99", "p99p99.9", "p99.9p100"),
    p = c(0, .5, .9, .99, .999), avg = c(1, 2, 8, 30, 100),
    topsh = c(1, .8, .6, .3, .1))
  expect_equal(dina_finished_publication_plot_stat(original, .5, TRUE)$value, .2)
  expect_equal(dina_finished_publication_plot_stat(original, .9, TRUE)$value, .6)
  expect_equal(dina_finished_publication_plot_stat(original, .99, TRUE)$value, .3)
  expect_equal(dina_finished_publication_plot_stat(original, .999, TRUE)$value, .1)
  original$p <- NULL
  expect_equal(dina_finished_publication_plot_stat(original, .999, TRUE)$value, .1)
  original$percentile <- NULL
  original$p <- c(0, .5, .9, .99, .999 + 5e-8)
  expect_equal(dina_finished_publication_plot_stat(original, .999, TRUE)$value, .1)
  original$p[5] <- .999 + 7e-8
  expect_true(is.na(dina_finished_publication_plot_stat(original, .999, TRUE)$value))
  original <- original[-3, ]
  expect_true(is.na(dina_finished_publication_plot_stat(original, .9, TRUE)$value))
})

test_that("comparison exposes preparation differences without conflating continuation repairs", {
  source_cli_for_tests()
  prepared <- data.frame(year = 2022L, p = c(0, .5, .9, .99, .999),
    avg = c(1, 2, 8, 30, 100))
  original <- prepared
  original$topsh <- c(1, .8, .6, .3, .1)
  proposed <- rbind(prepared, transform(prepared, year = 2023L))
  plot <- dina_finished_publication_plot_data(prepared, prepared, proposed, proposed, original)
  expect_length(plot, 4L)
  expect_equal(plot[[1]]$series[[1]]$value, .2)
  expected <- .5 / sum(c(.5, .4, .09, .009, .001) * prepared$avg)
  expect_equal(plot[[1]]$series[[2]]$value, expected)
  expect_equal(plot[[1]]$difference$difference_pp, 100 * (expected - .2))
  expect_equal(plot[[1]]$series[[4]]$value[1], plot[[1]]$series[[2]]$value)
  expect_equal(plot[[1]]$series[[5]], plot[[1]]$series[[4]])
  missing <- dina_finished_publication_plot_data(prepared, prepared, proposed, proposed)
  expect_equal(nrow(missing[[1]]$series[[1]]), 0L)
  expect_true(is.na(missing[[1]]$difference$difference_pp))
})
