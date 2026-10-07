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

test_that("trust settings are part of configuration validation identity", {
  root <- mini_repo()
  baseline <- file.path(root, "input_data", "_new", "previous_series", "dina_latam_3Oct2024.dta")
  haven::write_dta(data.frame(year = 2022L, iso = "CO", p = "p90p100", widcode = "sptinc992j", value = .5), baseline)
  dina_write_yaml(list(version = 1L, decisions = list()), dina_admin_trust_regions_path(root))
  check <- dina_settings_check(root, session = NULL, validate_runtime = FALSE)
  expect_true(check$valid)
  dina_record_config_validation(root, NULL, check)
  expect_equal(dina_config_validation_state(root, NULL)$code, "validated")
  dina_write_yaml(list(version = 1L, decisions = list(list(country = "COL", year = 2020L, trust = .9, basis = "explicit_policy"))), dina_admin_trust_regions_path(root))
  expect_equal(dina_config_validation_state(root, NULL)$code, "out_of_date")
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
