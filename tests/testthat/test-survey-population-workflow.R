source(file.path(repo_root_for_tests, "code", "R", "source-diagnostics", "survey_sources_include.R"))

survey_pop_fixture_write_dta <- function(root, rel, data) {
  path <- file.path(root, rel)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  haven::write_dta(data, path)
  path
}

survey_pop_fixture_root <- function(existing_survey_pop = FALSE, incoming = FALSE) {
  skip_if_not_installed("haven")
  skip_if_not_installed("yaml")
  root <- tempfile("survey-pop-fixture-")
  dir.create(root, recursive = TRUE)
  if (requireNamespace("withr", quietly = TRUE)) {
    withr::defer(unlink(root, recursive = TRUE), envir = parent.frame())
  }
  dir.create(file.path(root, "config"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(root, "code", "R", "source-diagnostics"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(root, "input_data", "surveys_CEPAL"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(root, "input_data", "_new", "surveys"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(root, "input_data", "population"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(root, "output"), recursive = TRUE, showWarnings = FALSE)
  file.copy(
    file.path(repo_root_for_tests, "code", "R", "source-diagnostics", "survey_sources_include.R"),
    file.path(root, "code", "R", "source-diagnostics", "survey_sources_include.R"),
    overwrite = TRUE
  )
  file.copy(
    file.path(repo_root_for_tests, "code", "R", "source-diagnostics", "survey_population_include.R"),
    file.path(root, "code", "R", "source-diagnostics", "survey_population_include.R"),
    overwrite = TRUE
  )
  writeLines("display \"fixture\"", file.path(root, "_config.do"))
  dina_write_yaml(list(
    project = list(name = "survey-pop-mini"),
    countries = c("ARG", "MEX"),
    years = list(first = 2000L, last = 2001L)
  ), file.path(root, "config", "dina.yml"))
  dina_write_yaml(list(
    version = 1L,
    source_type = "surveys",
    source_ids = c("surveys-cepal"),
    explore_output_root = "output/experiments/survey_population_explore",
    output_root = "output/experiments/survey_population_include",
    identity = list(countries_from_config = "config/dina.yml"),
    stata_read = list(encoding_fallbacks = c("latin1")),
    paths = list(
      canonical_surveys = "input_data/surveys_CEPAL",
      incoming_surveys = "input_data/_new/surveys",
      population = "input_data/wid/population_total_adult_npopul.dta",
      survey_pop = "intermediary_data/population/SurveyPop.dta"
    )
  ), file.path(root, "config", "survey_population_include.yml"))
  dina_write_yaml(list(sources = list(list(
    id = "surveys-cepal",
    family = "surveys",
    country = "MULTI",
    method = "manual",
    canonical = c("input_data/surveys_CEPAL/*/*.dta"),
    inbox = c("input_data/_new/surveys/*"),
    destination = "input_data/surveys_CEPAL/{relative}",
    transformer = c("code/R/source-diagnostics/survey_sources_include.R"),
    integration = "survey_population",
    notes = "Tiny survey fixture."
  ))), file.path(root, "config", "sources.yml"))
  dina_write_yaml(list(tasks = list()), file.path(root, "config", "pipeline.yml"))
  dina_write_yaml(list(items = list()), file.path(root, "config", "todo.yml"))

  pop <- data.frame(
    country = c("Argentina", "Argentina", "Mexico", "Mexico"),
    region = c("South America", "South America", "Central America", "Central America"),
    year = c(2000L, 2001L, 2000L, 2001L),
    totalpop = c(1000, 1100, 2000, 2100),
    adultpop = c(800, 880, 1200, 1260),
    stringsAsFactors = FALSE
  )
  survey_pop_fixture_write_dta(root, "input_data/wid/population_total_adult_npopul.dta", pop)

  arg <- data.frame(`_fep` = c(10, 10, 30), edad = c(30, 10, 40), id_hogar = c(1, 1, 2), check.names = FALSE)
  mex <- data.frame(`_fep` = c(100, 50), edad = c(25, 12), check.names = FALSE)
  survey_pop_fixture_write_dta(root, "input_data/surveys_CEPAL/ARG/ARG_2000N.dta", arg)
  survey_pop_fixture_write_dta(root, "input_data/surveys_CEPAL/MEX/MEX_2000N.dta", mex)
  if (isTRUE(incoming)) {
    incoming_mex <- data.frame(`_fep` = c(200, 100), edad = c(40, 8), check.names = FALSE)
    survey_pop_fixture_write_dta(root, "input_data/_new/surveys/MEX_2001N.dta", incoming_mex)
  }
  if (isTRUE(existing_survey_pop)) {
    dir.create(file.path(root, "intermediary_data", "population"), recursive = TRUE, showWarnings = FALSE)
    haven::write_dta(data.frame(country = "ARG", year = 2000L, totpop = 1, pct_adults = 1, adults = 1, nonadults = 0, totpop_i = 1, pct_adults_i = 1, totpop_ie = 1, pct_adults_ie = 1), file.path(root, "intermediary_data", "population", "SurveyPop.dta"))
    Sys.setFileTime(file.path(root, "intermediary_data", "population", "SurveyPop.dta"), Sys.time() - 3600)
  }
  root
}

test_that("legacy survey_population_include wrapper loads the survey workflow", {
  env <- new.env(parent = globalenv())
  source(file.path(repo_root_for_tests, "code", "R", "source-diagnostics", "survey_population_include.R"), local = env)
  expect_true(exists("run_survey_pop_explorer", envir = env, mode = "function"))
  expect_true(exists("run_survey_pop_include", envir = env, mode = "function"))
})

test_that("survey explorer reports missing and stale SurveyPop.dta", {
  root <- survey_pop_fixture_root()
  missing <- run_survey_pop_explorer(root = root, write_outputs = FALSE)
  expect_equal(missing$outputs$survey_pop_status$status, "missing")
  expect_equal(missing$outputs$survey_pop_status$severity, "action_required")
  expect_equal(missing$status, "action_required")
  expect_equal(missing$outputs$survey_pop_status$next_command, "dina sources include surveys --dry-run")
  expect_equal(missing$outputs$survey_source_summary$status, "all_good")

  stale_root <- survey_pop_fixture_root(existing_survey_pop = TRUE)
  stale <- run_survey_pop_explorer(root = stale_root, write_outputs = FALSE)
  expect_equal(stale$outputs$survey_pop_status$status, "stale")
})

test_that("survey include dry-run stages candidate SurveyPop without promoting", {
  root <- survey_pop_fixture_root(incoming = TRUE)
  include <- run_survey_pop_include(root = root, write_outputs = TRUE, run_id = "survey-valid")
  expect_equal(survey_pop_manifest_value(include$manifest, "status"), "all_good")
  expect_true(file.exists(file.path(include$paths$staged_repo, "intermediary_data", "population", "SurveyPop.dta")))
  expect_false(file.exists(file.path(root, "intermediary_data", "population", "SurveyPop.dta")))
  expect_true(any(include$outputs$promotion_plan$artifact_type == "survey_population"))
  expect_true(any(include$outputs$promotion_plan$artifact_type == "survey_source"))
  expect_true(any(include$outputs$survey_source_comparison$comparison_status == "new_year"))
  expect_true(any(include$outputs$staged_source_mappings$to_rel == "input_data/surveys_CEPAL/MEX/MEX_2001N.dta"))
})

test_that("survey confirm writes SurveyPop and can restore it", {
  root <- survey_pop_fixture_root(incoming = TRUE)
  include <- run_survey_pop_include(root = root, write_outputs = TRUE, run_id = "survey-confirmable")
  confirm <- survey_pop_confirm_sources(root = root, include_run = include$paths$root)
  survey_pop <- file.path(root, "intermediary_data", "population", "SurveyPop.dta")
  expect_true(file.exists(survey_pop))
  promoted <- haven::read_dta(survey_pop)
  arg_2000 <- promoted[promoted$country == "ARG" & promoted$year == 2000L, ]
  expect_equal(as.numeric(arg_2000$totpop), 1000)
  expect_equal(round(as.numeric(arg_2000$pct_adults), 6), 80)
  restore <- survey_pop_restore_sources(root = root, confirm_run = confirm$paths$root)
  expect_true(any(restore$outputs$restore_report$restore_status == "removed_promoted_destination"))
  expect_false(file.exists(survey_pop))
})

test_that("incoming N1 survey variant stages and promotes as canonical N", {
  root <- survey_pop_fixture_root()
  incoming_mex <- data.frame(`_fep` = c(300, 150), edad = c(45, 9), check.names = FALSE)
  survey_pop_fixture_write_dta(root, "input_data/_new/surveys/MEX_2001N1.dta", incoming_mex)

  include <- run_survey_pop_include(root = root, write_outputs = TRUE, run_id = "survey-variant")
  expect_equal(survey_pop_manifest_value(include$manifest, "status"), "all_good")
  expect_equal(include$outputs$survey_source_summary$filename_variants, 1L)
  expect_true(any(include$outputs$survey_source_candidates$original_basename == "MEX_2001N1.dta"))
  expect_true(any(include$outputs$survey_source_candidates$destination == "input_data/surveys_CEPAL/MEX/MEX_2001N.dta"))
  expect_true(any(include$outputs$staged_source_mappings$from_rel == "input_data/_new/surveys/MEX_2001N1.dta"))
  expect_true(any(include$outputs$staged_source_mappings$to_rel == "input_data/surveys_CEPAL/MEX/MEX_2001N.dta"))

  survey_pop_confirm_sources(root = root, include_run = include$paths$root)
  expect_true(file.exists(file.path(root, "input_data", "surveys_CEPAL", "MEX", "MEX_2001N.dta")))
  expect_false(file.exists(file.path(root, "input_data", "surveys_CEPAL", "MEX", "MEX_2001N1.dta")))
})

test_that("retroactive overlap changes are reported without blocking", {
  root <- survey_pop_fixture_root()
  changed_mex <- data.frame(`_fep` = c(500, 25), edad = c(25, 12), check.names = FALSE)
  survey_pop_fixture_write_dta(root, "input_data/_new/surveys/MEX_2000N.dta", changed_mex)

  include <- run_survey_pop_include(root = root, write_outputs = TRUE, run_id = "survey-retro")
  expect_equal(survey_pop_manifest_value(include$manifest, "status"), "all_good")
  expect_equal(include$outputs$survey_source_summary$overlaps, 1L)
  expect_equal(include$outputs$survey_source_summary$changed_overlaps, 1L)
  expect_true(any(include$outputs$survey_source_comparison$comparison_status == "retro_overlap_changed"))
})

test_that("multiple incoming primary variants for one country-year block as ambiguous", {
  root <- survey_pop_fixture_root()
  incoming_mex <- data.frame(`_fep` = c(300, 150), edad = c(45, 9), check.names = FALSE)
  survey_pop_fixture_write_dta(root, "input_data/_new/surveys/MEX_2001N.dta", incoming_mex)
  survey_pop_fixture_write_dta(root, "input_data/_new/surveys/MEX_2001N1.dta", incoming_mex)

  include <- run_survey_pop_include(root = root, write_outputs = TRUE, run_id = "survey-ambiguous")
  expect_equal(survey_pop_manifest_value(include$manifest, "status"), "blocked")
  expect_equal(include$outputs$survey_source_summary$ambiguous_incoming, 1L)
  expect_true(any(include$outputs$survey_source_candidates$blocker == "ambiguous_incoming_primary"))
  expect_true(all(grepl("^blocked_ambiguous_incoming_primary$", include$outputs$staged_source_mappings$copy_status)))
})

test_that("incoming survey missing minimum variables blocks include", {
  root <- survey_pop_fixture_root()
  missing_fep <- data.frame(edad = c(45, 9), check.names = FALSE)
  survey_pop_fixture_write_dta(root, "input_data/_new/surveys/MEX_2001N.dta", missing_fep)

  include <- run_survey_pop_include(root = root, write_outputs = TRUE, run_id = "survey-missing-required")
  expect_equal(survey_pop_manifest_value(include$manifest, "status"), "blocked")
  expect_equal(include$outputs$survey_source_summary$missing_required, 1L)
  expect_true(any(include$outputs$survey_source_candidates$blocker == "missing_required_columns"))
  expect_true(any(include$outputs$staged_source_mappings$copy_status == "blocked_missing_required_columns"))
})

test_that("resilient Stata reader records configured fallback recovery", {
  reader_env <- environment(survey_pop_read_dta_resilient)
  original <- get("survey_pop_read_dta_once", envir = reader_env)
  calls <- character()
  assign("survey_pop_read_dta_once", function(path, encoding = NULL, columns = NULL, n_max = Inf) {
    label <- if (is.null(encoding)) "native" else encoding
    calls <<- c(calls, label)
    if (is.null(encoding)) stop("invalid metadata encoding")
    data.frame(`_fep` = numeric(), edad = numeric(), check.names = FALSE)
  }, envir = reader_env)
  on.exit(assign("survey_pop_read_dta_once", original, envir = reader_env), add = TRUE)

  read <- survey_pop_read_dta_resilient("legacy.dta", encoding_fallbacks = c("latin1"))
  expect_true(read$ok)
  expect_true(read$recovered)
  expect_equal(read$encoding, "latin1")
  expect_equal(read$attempted_encodings, "native,latin1")
  expect_equal(calls, c("native", "latin1"))
})

test_that("survey identity can be inferred safely from filename and parent path", {
  root <- survey_pop_fixture_root()
  inferred <- data.frame(`_fep` = c(100, 50), edad = c(30, 10), check.names = FALSE)
  survey_pop_fixture_write_dta(
    root,
    "input_data/_new/surveys/MEX_archives/household-survey-2001.dta",
    inferred
  )

  explore <- run_survey_pop_explorer(root = root, write_outputs = FALSE)
  hit <- explore$outputs$source_inventory[
    explore$outputs$source_inventory$original_basename == "household-survey-2001.dta",
    ,
    drop = FALSE
  ]
  expect_equal(hit$country, "MEX")
  expect_equal(hit$year, 2001L)
  expect_equal(hit$identity_method, "path_inferred")
  expect_equal(hit$status, "ok")
  expect_equal(hit$destination, "input_data/surveys_CEPAL/MEX/MEX_2001N.dta")
})

test_that("conflicting inferred identities block instead of guessing", {
  root <- survey_pop_fixture_root()
  survey <- data.frame(`_fep` = c(100, 50), edad = c(30, 10), check.names = FALSE)
  survey_pop_fixture_write_dta(
    root,
    "input_data/_new/surveys/ARG_archives/MEX-2001-survey.dta",
    survey
  )

  explore <- run_survey_pop_explorer(root = root, write_outputs = FALSE)
  hit <- explore$outputs$survey_source_candidates[
    explore$outputs$survey_source_candidates$original_basename == "MEX-2001-survey.dta",
    ,
    drop = FALSE
  ]
  expect_equal(hit$blocker, "ambiguous_identity")
  expect_equal(hit$severity, "blocked")
})

test_that("all available countries receive country-specific annual SurveyPop grids", {
  root <- survey_pop_fixture_root()
  bol <- data.frame(`_fep` = c(100, 50), edad = c(30, 10), check.names = FALSE)
  survey_pop_fixture_write_dta(root, "input_data/_new/surveys/BOL_2001N.dta", bol)
  survey_pop_fixture_write_dta(root, "input_data/_new/surveys/BOL_2003N.dta", bol)

  explore <- run_survey_pop_explorer(root = root, write_outputs = TRUE)
  bol_coverage <- explore$outputs$year_coverage[explore$outputs$year_coverage$country == "BOL", ]
  expect_equal(bol_coverage$grid_years, "2001-2003")
  expect_equal(bol_coverage$selected_years, "2001,2003")
  expect_equal(bol_coverage$interpolated_years, "2002")

  include <- run_survey_pop_include(root = root, write_outputs = TRUE, run_id = "survey-dynamic-grid")
  expect_true(any(include$outputs$staged_source_mappings$to_rel == "input_data/surveys_CEPAL/BOL/BOL_2001N.dta"))
  bol_status <- include$outputs$candidate_source_status[include$outputs$candidate_source_status$country == "BOL", ]
  expect_equal(bol_status$year, 2001:2003)
  expect_equal(bol_status$source_status[[2L]], "interpolated_grid_year")
  bol_candidate <- include$outputs$survey_pop_comparison[include$outputs$survey_pop_comparison$country == "BOL", ]
  expect_equal(bol_candidate$year, 2001:2003)
})

test_that("invalid incoming replacement is usable canonically but blocked from selection", {
  root <- survey_pop_fixture_root()
  invalid <- data.frame(edad = c(30, 10), check.names = FALSE)
  survey_pop_fixture_write_dta(root, "input_data/_new/surveys/MEX_2000N.dta", invalid)

  explore <- run_survey_pop_explorer(root = root, write_outputs = FALSE)
  coverage <- explore$outputs$year_coverage[explore$outputs$year_coverage$country == "MEX", ]
  expect_match(coverage$usable_years, "2000")
  expect_false(grepl("2000", coverage$selected_years, fixed = TRUE))
  expect_equal(coverage$blocked_years, "2000")
})

test_that("explore defers overlap comparison and reports explicit coverage stages", {
  root <- survey_pop_fixture_root()
  changed_mex <- data.frame(`_fep` = c(500, 25), edad = c(25, 12), check.names = FALSE)
  survey_pop_fixture_write_dta(root, "input_data/_new/surveys/MEX_2000N.dta", changed_mex)

  explore <- run_survey_pop_explorer(root = root, write_outputs = FALSE)
  summary <- explore$outputs$survey_source_summary
  expect_equal(summary$overlap_comparison_status, "not_evaluated")
  expect_true(is.na(summary$changed_overlaps))
  expect_false("effective_years" %in% names(explore$outputs$year_coverage))
  expect_true(all(c("grid_years", "usable_years", "selected_years", "blocked_years", "interpolated_years") %in% names(explore$outputs$year_coverage)))
})

test_that("local incoming survey corpus is a usable regression corpus", {
  incoming_root <- file.path(repo_root_for_tests, "input_data", "_new", "surveys")
  files <- if (dir.exists(incoming_root)) {
    list.files(incoming_root, pattern = "\\.dta$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  } else {
    character()
  }
  skip_if(!length(files), "Local incoming survey corpus is not available.")

  contract <- survey_pop_read_contract(repo_root_for_tests)
  inventory <- survey_pop_source_inventory(repo_root_for_tests, contract)
  incoming <- inventory[inventory$source_set == "incoming", , drop = FALSE]
  candidates <- survey_pop_source_candidates(inventory)
  incoming_candidates <- candidates[candidates$source_set == "incoming", , drop = FALSE]

  expect_equal(nrow(incoming), length(files))
  expect_true(all(incoming$file_class == "primary_survey"))
  expect_true(all(incoming$status == "ok"))
  expect_true(all(incoming_candidates$blocker == ""))
  expect_true(any(incoming$read_recovered))
  recovered <- incoming[incoming$read_recovered, , drop = FALSE]
  sample_read <- survey_pop_read_survey(
    recovered$file[[1L]],
    recovered$country[[1L]],
    encoding_fallbacks = survey_pop_stata_encoding_fallbacks(contract)
  )
  expect_true(sample_read$ok)
  expect_true(sample_read$recovered)
})

test_that("main dina CLI dispatches survey explore, include, and table", {
  root <- survey_pop_fixture_root(incoming = TRUE)
  explore <- run_dina_cli(c("sources", "explore", "surveys"), root = root)
  expect_equal(explore$status, 0L)
  expect_match(explore$output, "Survey Sources Explore")
  expect_match(explore$output, "1\\. Source intake preflight \\(primary\\)")
  expect_match(explore$output, "2\\. Derived SurveyPop preview \\(secondary\\)")
  expect_match(explore$output, "dina sources include surveys --dry-run")

  table <- run_dina_cli(c("sources", "table", "surveys", "survey_source_summary"), root = root)
  expect_equal(table$status, 0L)
  expect_match(table$output, "Survey Sources Table: survey_source_summary")

  include <- run_dina_cli(c("sources", "include", "surveys", "--dry-run"), root = root)
  expect_equal(include$status, 0L)
  expect_match(include$output, "Survey Sources Include")
  expect_match(include$output, "1\\. Source promotion review \\(required\\)")
  expect_match(include$output, "Staging result: .* survey source files staged")
  expect_match(include$output, "2\\. Derived SurveyPop check \\(secondary\\)")
  expect_match(include$output, "SurveyPop candidate: .* grid rows")
  expect_match(include$output, "Review before confirm \\(in order\\)")
  expect_match(include$output, "survey_source_comparison")
  expect_match(include$output, "staged_source_mappings")
  expect_match(include$output, "promotion_plan")
  expect_match(include$output, "survey_pop_comparison")
  expect_match(include$output, "No production files changed")
})
