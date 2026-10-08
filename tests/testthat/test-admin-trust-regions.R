test_that("trust review proposes uncovered formatted PIT years and blocks validation", {
  root <- mini_repo()
  dir.create(file.path(root, "input_data", "admin_data", "CHL"), recursive = TRUE)
  dir.create(file.path(root, "intermediary_data", "microdata", "raw", "CHL"), recursive = TRUE)
  file.create(file.path(root, "intermediary_data", "microdata", "raw", "CHL", c("CHL_2009_raw.dta", "CHL_2010_raw.dta")))
  file.copy(file.path(repo_root_for_tests, "input_data", "admin_data", "MEX", "gpinter_MEX_2009.xlsx"),
    file.path(root, "input_data", "admin_data", "CHL", "gpinter_CHL_2009.xlsx"))
  file.copy(file.path(repo_root_for_tests, "input_data", "admin_data", "MEX", "gpinter_MEX_2010.xlsx"),
    file.path(root, "input_data", "admin_data", "CHL", "gpinter_CHL_2010.xlsx"))
  cfg <- dina_config(root)
  cfg$countries <- "CHL"
  cfg$years <- list(first = 2009L, last = 2010L)
  dina_write_yaml(cfg, dina_config_path(root))
  dina_write_yaml(list(version = 1L, decisions = list(
    list(country = "CHL", year = 2010L, trust = .98, trust_wages = .82, basis = "explicit_policy")
  )), dina_admin_trust_regions_path(root))

  review <- dina_admin_trust_review(root, dina_config(root))$proposals
  expect_equal(review$status[review$year == 2010L], "configured")
  expect_equal(review$status[review$year == 2009L], "unresolved")
  check <- dina_admin_trust_validate(root, dina_config(root))
  expect_false(check$valid)
  expect_match(paste(check$errors, collapse = " "), "CHL 2009")

  decisions <- dina_admin_trust_regions(root)$decisions
  decisions <- c(decisions, list(list(country = "CHL", year = 2009L, trust = .98, trust_wages = .82, basis = "explicit_policy")))
  dina_write_yaml(list(version = 1L, decisions = decisions), dina_admin_trust_regions_path(root))
  expect_true(dina_admin_trust_validate(root, dina_config(root))$valid)
  runtime <- dina_write_admin_trust_runtime(root, config = dina_config(root))
  expect_true(file.exists(runtime))
  expect_equal(nrow(utils::read.csv(runtime)), 2L)
})

test_that("recovered legacy prepared inputs are complete and recorded", {
  root <- repo_root_for_tests
  sources <- dina_sources(root)$sources
  ids <- vapply(sources, function(x) x$id %||% "", character(1))
  for (id in c("mex-legacy-prepared-pit", "ury-legacy-prepared-pit")) {
    source <- sources[[match(id, ids)]]
    files <- file.path(root, unlist(source$canonical, use.names = FALSE))
    expect_true(all(file.exists(files)))
    expected <- unlist(source$checksums, use.names = TRUE)
    actual <- vapply(files, dina_hash_file, character(1))
    names(actual) <- basename(files)
    expect_equal(actual[names(expected)], expected)
  }
  expect_false(dir.exists(file.path(root, "input_data", "admin_data", "MEX", "eff-tax-rate")))
  expect_false(dir.exists(file.path(root, "input_data", "admin_data", "URY", "eff-tax-rate")))
})

test_that("trust settings are an Admin artifact, not configuration validation", {
  root <- mini_repo()
  baseline <- file.path(root, "input_data", "_new", "previous_series", "dina_latam_3Oct2024.dta")
  haven::write_dta(data.frame(year = 2022L, iso = "CO", p = "p90p100", widcode = "sptinc992j", value = .5), baseline)
  dina_write_yaml(list(version = 1L, decisions = list()), dina_admin_trust_regions_path(root))
  check <- dina_settings_check(root, session = NULL, validate_runtime = FALSE)
  expect_true(check$valid)
  dina_record_config_validation(root, NULL, check)
  expect_equal(dina_config_validation_state(root, NULL)$code, "validated")
  dina_write_yaml(list(version = 1L, decisions = list(list(country = "COL", year = 2020L, trust = .9, basis = "explicit_policy"))), dina_admin_trust_regions_path(root))
  expect_equal(dina_config_validation_state(root, NULL)$code, "validated")
})

test_that("Admin trust snapshots are scope-specific and detected before BFM", {
  root <- mini_repo()
  cfg <- dina_config(root)
  cfg$countries <- "COL"
  cfg$years <- list(first = 2020L, last = 2020L)
  dina_write_yaml(cfg, dina_config_path(root))
  dina_write_yaml(list(version = 1L, decisions = list()), dina_admin_trust_regions_path(root))
  session <- dina_update_start("2026", root = root)
  expect_match(dina_admin_trust_snapshot_problem(root, session, dina_session_config(session, root, expand_env = FALSE)), "Admin sources have not been included")
  path <- dina_write_admin_trust_runtime(root, session, dina_session_config(session, root, expand_env = FALSE))
  expect_true(file.exists(path))
  expect_true(file.exists(dina_admin_trust_snapshot_manifest_path(root, session)))
  manifest <- dina_read_json(dina_admin_trust_snapshot_manifest_path(root, session))
  expect_equal(manifest$scope, dina_config_scope(session)$id)
  expect_equal(manifest$countries, "COL")
  expect_equal(manifest$first_year, 2020L)
  expect_equal(manifest$last_year, 2021L)
  expect_length(dina_admin_trust_snapshot_problem(root, session, dina_session_config(session, root, expand_env = FALSE)), 0L)
  writeLines("not,a,trust,snapshot", path)
  expect_match(dina_admin_trust_snapshot_problem(root, session, dina_session_config(session, root, expand_env = FALSE)), "modified or corrupted")
  dina_write_admin_trust_runtime(root, session, dina_session_config(session, root, expand_env = FALSE))
  session <- dina_session_config_set(session, root, "years.last", "2022")
  expect_match(dina_admin_trust_snapshot_problem(root, session, dina_session_config(session, root, expand_env = FALSE)), "does not match this scope")
})

test_that("only trust-consuming tasks require the included Admin snapshot", {
  root <- mini_repo()
  session <- dina_update_start("2026", root = root)
  bfm <- list(id = "03a-run-bfm", inputs = character(), outputs = character())
  other <- list(id = "01a-clean-macro-data", inputs = character(), outputs = character())
  expect_match(paste(dina_task_admin_source_issues(bfm, root, session), collapse = " "), "Admin sources have not been included")
  expect_length(dina_task_admin_source_issues(other, root, session), 0L)
  expect_error(dina_pipeline_input_preflight(list(bfm), root = root, session = session), "Admin sources have not been included")
})

test_that("an included legacy Admin review receives its missing scoped snapshot without re-inclusion", {
  source_cli_for_tests()
  root <- mini_repo()
  cfg <- dina_config(root)
  cfg$countries <- "COL"
  cfg$years <- list(first = 2020L, last = 2020L)
  dina_write_yaml(cfg, dina_config_path(root))
  baseline <- file.path(root, "input_data", "_new", "previous_series", "dina_latam_3Oct2024.dta")
  haven::write_dta(data.frame(year = 2020L, iso = "CO", p = "p90p100", widcode = "sptinc992j", value = .5), baseline)
  session <- dina_update_start("2026", root = root)
  dina_write_yaml(list(version = 1L, decisions = list()), dina_admin_trust_regions_path(root))
  source_file <- file.path(root, "input_data", "admin_data", "COL", "accepted.xlsx")
  dir.create(dirname(source_file), recursive = TRUE)
  file.create(source_file)
  run <- file.path(root, "output", "experiments", "admin_pit_include", "runs", "review-legacy")
  dir.create(run, recursive = TRUE)
  record <- list(family = "admin", status = "included", run = run, reviewed_at = dina_now(),
    included_at = dina_now(), inputs = list(), candidates = list(), baseline = list(),
    acceptance_watch = dina_review_watch(source_file), watch = dina_review_watch(source_file))
  dina_review_save(record, root)
  check <- dina_settings_check(root, session = session, validate_runtime = FALSE)
  expect_true(check$valid)
  dina_record_config_validation(root, session, check)

  preflight <- dina_pipeline_input_preflight(list(list(id = "03a-run-bfm", inputs = character(), outputs = character())), root, session)
  expect_true(preflight$admin_trust_snapshot$created)
  expect_true(file.exists(dina_admin_trust_snapshot_path(root, session)))
  expect_length(dina_task_admin_source_issues(list(id = "03a-run-bfm"), root, session), 0L)
  expect_equal(dina_config_validation_state(root, session)$code, "validated")
  expect_false(dina_review_included_watch_changed(dina_review_read(root, "admin")))
})

test_that("Admin Include records trust without invalidating configuration validation", {
  source_cli_for_tests()
  root <- mini_repo()
  baseline <- file.path(root, "input_data", "_new", "previous_series", "dina_latam_3Oct2024.dta")
  haven::write_dta(data.frame(year = 2022L, iso = "CO", p = "p90p100", widcode = "sptinc992j", value = .5), baseline)
  check <- dina_settings_check(root, session = NULL, validate_runtime = FALSE)
  dina_record_config_validation(root, NULL, check)
  run <- file.path(root, "output", "experiments", "admin_pit_include", "runs", "review-fixture")
  dir.create(file.path(run, "tables"), recursive = TRUE)
  utils::write.csv(data.frame(result = character()), file.path(run, "tables", "review_values.csv"), row.names = FALSE)
  utils::write.csv(data.frame(country = character(), source_id = character(), extension_years = character(), overlap_years = character(), missing_in_new_years = character()), file.path(run, "tables", "review_coverage.csv"), row.names = FALSE)
  record <- list(family = "admin", status = "all_good", run = run, reviewed_at = dina_now(), files = 0L,
    comparison_note = "", comparison_error = "", inputs = list(), candidates = list(), baseline = list())
  old_engine <- dina_review_engine
  dina_review_engine <- function(...) list(admin_pit_include_confirm_sources = function(...) {
    list(manifest = data.frame(key = "status", value = "confirmed", stringsAsFactors = FALSE),
      paths = list(root = file.path(root, "output", "experiments", "admin_pit_include", "confirms", "fixture")))
  })
  on.exit(dina_review_engine <<- old_engine, add = TRUE)
  dina_review_include(root, "admin", flags = list(confirm = TRUE), is_terminal = FALSE, record = record)
  expect_equal(dina_config_validation_state(root, NULL)$code, "validated")
  expect_true(file.exists(dina_admin_trust_snapshot_path(root, NULL)))
  expect_true(file.exists(dina_admin_trust_snapshot_manifest_path(root, NULL)))
})

test_that("legacy receipts remain current when only their retired trust hash differs", {
  root <- mini_repo()
  baseline <- file.path(root, "input_data", "_new", "previous_series", "dina_latam_3Oct2024.dta")
  haven::write_dta(data.frame(year = 2022L, iso = "CO", p = "p90p100", widcode = "sptinc992j", value = .5), baseline)
  check <- dina_settings_check(root, session = NULL, validate_runtime = FALSE)
  dina_record_config_validation(root, NULL, check)
  receipt_path <- dina_config_validation_receipt_path(root, NULL)
  receipt <- dina_read_json(receipt_path)
  receipt$receipt_version <- NULL
  receipt$trust_config_hash <- "retired-trust-fingerprint"
  receipt$effective_config_hash <- "legacy-effective-fingerprint"
  dina_write_json(receipt, receipt_path)
  dina_write_yaml(list(version = 1L, decisions = list(list(country = "COL", year = 2020L, trust = .9, basis = "explicit_policy"))), dina_admin_trust_regions_path(root))
  expect_equal(dina_config_validation_state(root, NULL)$code, "validated")
})

test_that("applying a trust proposal activates its complete configuration at once", {
  source(file.path(repo_root_for_tests, "code", "R", "cli", "source_review.R"), local = FALSE)
  proposal <- list(
    version = 2L,
    decisions = list(
      list(country = "BRA", year = 2024L, trust = .85, basis = "explicit_policy", status = "proposed", change = "added"),
      list(country = "CHL", year = 2024L, trust = .80, basis = "explicit_policy", status = "proposed", change = "added")
    ),
    proposal = list(instructions = "candidate only")
  )
  activated <- dina_admin_trust_activate_proposal(proposal)
  expect_true(all(vapply(activated$decisions, function(x) identical(x$status, "active"), logical(1))))
  expect_true(all(vapply(activated$decisions, function(x) is.null(x$change), logical(1))))
  expect_null(activated$proposal)
})

test_that("trust review items keep retained COL settings in the complete proposal", {
  proposals <- data.frame(
    country = c("BRA", "CHL", "COL"), year = c(2024L, 2024L, 2023L),
    proposal_trust = c(.85, .80, .90), current_trust = c(NA_real_, NA_real_, .90),
    status = c("configuration_pending", "configuration_pending", "configured"),
    basis = c("carried_forward", "carried_forward", "explicit_policy"),
    evidence = c("BRA evidence", "CHL evidence", "COL evidence"), stringsAsFactors = FALSE
  )
  coverage <- data.frame(country = c("BRA", "CHL", "COL"), extension_years = c("2024", "2024", "2023"), stringsAsFactors = FALSE)
  items <- dina_admin_trust_review_items(proposals, coverage)
  expect_equal(items$country, c("BRA", "CHL", "COL"))
  expect_equal(items$action, c("add proposed value", "add proposed value", "verify existing value"))
  expect_equal(items$method, c("extend prior setting", "extend prior setting", "existing policy"))
})

test_that("admin review verification retains its explicit repository root", {
  source(file.path(repo_root_for_tests, "code", "R", "cli", "source_review.R"), local = FALSE)
  root <- mini_repo()
  path <- dina_admin_trust_regions_path(root)
  dina_write_yaml(list(version = 1L, decisions = list()), path)
  record <- list(
    family = "admin",
    inputs = dina_review_hashes(path),
    candidates = list(),
    baseline = list(),
    trust_config_hash = dina_hash_file(path)
  )
  withr::with_dir(tempdir(), expect_silent(dina_review_verify(record, root = root)))
})

test_that("publisher-derived trust skips the zero bracket deterministically", {
  root <- mini_repo()
  path <- file.path(root, "prepared.xlsx")
  openxlsx::write.xlsx(data.frame(p = c(.41, .84, .96), thr = c(0, 100, 1000)), path)
  selected <- dina_admin_trust_publisher_cutoff(path)
  expect_true(selected$valid)
  expect_equal(selected$trust, .84)
  expect_equal(selected$threshold, 100)
  expect_match(selected$evidence, "first positive threshold")

  openxlsx::write.xlsx(data.frame(p = c(.84, .96), thr = c(100, 1000)), path)
  expect_false(dina_admin_trust_publisher_cutoff(path)$valid)
})

test_that("legacy profiles distinguish repeated policy from unreconstructable derivation", {
  rows <- dina_admin_trust_rows(repo_root_for_tests)
  expect_equal(rows$basis[rows$country == "BRA" & rows$year == 2023L], "explicit_policy")
  expect_equal(rows$provenance_status[rows$country == "BRA" & rows$year == 2023L], "inferred_policy")
  expect_equal(rows$basis[rows$country == "ARG" & rows$year == 2021L], "publisher_derived")
  expect_equal(rows$provenance_status[rows$country == "ARG" & rows$year == 2021L], "legacy_unverified")
})

test_that("trust scope covers actual two-stage overlap, not extrapolated years", {
  root <- mini_repo()
  dir.create(file.path(root, "input_data", "admin_data", "ARG"), recursive = TRUE)
  dir.create(file.path(root, "intermediary_data", "microdata", "raw", "ARG"), recursive = TRUE)
  file.create(file.path(root, "intermediary_data", "microdata", "raw", "ARG", "ARG_2020_raw.dta"))
  openxlsx::write.xlsx(data.frame(p = c(.4, .8), thr = c(0, 100)), file.path(root, "input_data", "admin_data", "ARG", "diverse_ARG_2020.xlsx"))
  openxlsx::write.xlsx(data.frame(p = c(.3, .7), thr = c(0, 100)), file.path(root, "input_data", "admin_data", "ARG", "wage_ARG_2020.xlsx"))
  cfg <- dina_config(root)
  cfg$countries <- "ARG"
  cfg$years <- list(first = 2020L, last = 2024L)
  rows <- dina_admin_trust_required_all_rows(root, cfg)
  expect_equal(rows$year, 2020L)
  expect_true(rows$requires_trust_wages)
  expect_false(any(rows$year == 2024L))
})
