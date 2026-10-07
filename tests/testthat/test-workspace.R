workspace_session <- function(root) {
  session <- list(id = "2026-fixture", year = 2026L, status = "initialized", task_runs = list())
  dir.create(dina_update_dir(session$id, root), recursive = TRUE, showWarnings = FALSE)
  writeLines(session$id, dina_active_update_file(root))
  dina_save_session(session, root)
  dina_write_suggested_session_config_override(session, root)
  session
}

workspace_baseline <- function(root, name = "dina_latam_3Oct2024.dta") {
  path <- file.path(root, "input_data", "_new", "previous_series", name)
  haven::write_dta(data.frame(year = 2022:2023, iso = "CO", p = "p90p100", widcode = "sptinc992j", value = c(.5, .6)), path)
  dina_relative(path, root)
}

test_that("SNA empty cells retain applicability and extraction evidence", {
  source_cli_for_tests()
  old <- data.frame(country = "COL", year = 2000:2006, value = c(NA, NA, NA, NA, 100, 0, NA))
  new <- old; new$value[5:7] <- c(NA, 10, 4)
  rows <- dina_review_compare(old, new, c("country", "year"), "value")
  detail <- data.frame(country = "COL", year = 2000:2002, variable = "value",
    status = c("ok_contract_missing", "warning_missing_expected_value", "warning_ambiguous_match"), extract_status = c("excluded_by_contract", "no_code_match", "duplicate_conflict"))
  rows <- dina_review_sna_absences(rows, detail)
  expect_equal(rows$result, c("expected absence", "expected value missing", "expected value missing", "outside coverage", "value missing", "revised", "value added"))
  expect_match(rows$absence_reason[2], "no code match")
  output <- paste(capture.output(dina_review_show_rows(rows[6, ], c("old_value", "new_value", "percent"))), collapse = "\n")
  expect_match(output, "baseline is zero")
  expect_false(grepl("undefined", output))
})

test_that("destination blockers and expected absences are classified explicitly", {
  source_cli_for_tests()
  outputs <- list(include_detail = data.frame(country = c("CHL", "URY"), year = 2000,
    variable = "value", status = c("warning_missing_expected_value", "ok_contract_missing")),
    staged_source_mappings = data.frame(country = rep("MEX", 21), source = paste0("CSI_", 3:23, ".xlsx"), action = "blocked", status = "ambiguous_destination"))
  problems <- dina_review_problems(list(outputs = outputs))
  expect_equal(sum(problems$severity == "Blocker"), 21)
  expect_equal(sum(problems$severity == "Warning"), 1)
  expect_false(any(problems$status == "ok_contract_missing"))
  expect_equal(problems$severity[1], "Blocker")
  expect_match(problems$reason[1], "more than one destination")
})

test_that("saved SNA reviews retain raw evidence while the normal view stays in configured direct-input scope", {
  source_cli_for_tests()
  root <- mini_repo(); run <- file.path(root, "review"); dir.create(file.path(run, "tables"), recursive = TRUE)
  values <- data.frame(country = c(rep("COL", 86), rep("ECU", 41), rep("DOM", 40)), year = c(rep(2023, 127), rep(2000, 40)),
    measure = paste0("v", 1:167), old_value = c(rep(100, 127), rep(NA, 40)), new_value = c(rep(110, 86), rep(NA, 81)),
    difference = c(rep(10, 86), rep(NA, 81)), percent = c(rep(10, 86), rep(NA, 81)), result = c(rep("revised", 86), rep("value missing", 41), rep("not checked", 40)))
  tables <- list(review_values = values, review_coverage = dina_review_coverage(NULL, values, "sna"), review_problems = data.frame(),
    include_detail = data.frame(), staged_source_mappings = data.frame(country = "MEX", source = paste0("CSI_", 3:23, ".xlsx"), action = "blocked", status = "ambiguous_destination"))
  for (name in names(tables)) write.csv(tables[[name]], file.path(run, "tables", paste0(name, ".csv")), row.names = FALSE)
  record <- list(run = run, family = "sna", status = "blocked", comparison_note = "Fixture scope")
  before <- dina_hash_path(run)
  text <- paste(capture.output(dina_review_print(record)), collapse = "\n")
  expect_match(text, "Country summary")
  expect_match(text, "Direct pipeline inputs")
  expect_match(text, "not assessed")
  expect_false(grepl("not checked|undefined|missing proposed", text))
  expect_equal(nrow(dina_review_table(record, "revisions")), 86)
  expect_equal(dina_hash_path(run), before)
})

test_that("source states distinguish empty, unfinished, stale and accepted reviews", {
  source_cli_for_tests(); root <- mini_repo()
  expect_equal(dina_review_family_status(root, "sna")$code, "empty")
  incoming <- file.path(root, "input_data", "_new", "sna"); dir.create(incoming, recursive = TRUE)
  writeLines("candidate", file.path(incoming, "data"))
  expect_equal(dina_review_family_status(root, "sna")$code, "unexplored")
  record <- list(family = "sna", run = "fixture", status = "all_good", reviewed_at = dina_now(), watch = dina_review_watch(incoming))
  dina_review_save(record, root)
  expect_equal(dina_review_family_status(root, "sna")$code, "ready")
  record$status <- "included"; record$included_at <- dina_now(); dina_review_save(record, root)
  expect_equal(dina_review_family_status(root, "sna")$code, "included")
  expect_null(dina_review_recommendation(root))
  Sys.sleep(2.1)
  writeLines("different candidate", file.path(incoming, "data"))
  state <- dina_review_family_status(root, "sna")
  expect_equal(state$code, "included_stale")
  expect_equal(state$label, "Included · recheck needed")
  expect_equal(dina_review_state_label(state), "Included · recheck needed")
  expect_match(state$reason, "incoming source files changed", fixed = TRUE)
  expect_equal(dina_review_recommendation(root)$proposal$command, "dina sources explore sna")
  record$status <- "inclusion_failed"; dina_review_save(record, root)
  expect_equal(dina_review_family_status(root, "sna")$code, "failed")
})

test_that("accepted source status ignores CLI code changes but detects source changes", {
  source_cli_for_tests(); root <- mini_repo()
  incoming <- file.path(root, "input_data", "_new", "wid")
  dir.create(incoming, recursive = TRUE)
  writeLines("candidate", file.path(incoming, "population.dta"))
  cli_file <- file.path(root, "code", "R", "cli", "source_report.R")
  dir.create(dirname(cli_file), recursive = TRUE)
  writeLines("first presentation", cli_file)
  record <- list(family = "wid", run = "fixture", status = "included", reviewed_at = dina_now(),
    included_at = dina_now(), watch = dina_review_watch(c(incoming, cli_file)))
  dina_review_save(record, root)
  writeLines("improved presentation", cli_file)
  expect_equal(dina_review_family_status(root, "wid")$code, "included")
  Sys.sleep(2.1)
  writeLines("new candidate", file.path(incoming, "population.dta"))
  expect_equal(dina_review_family_status(root, "wid")$code, "included_stale")
})

test_that("accepted source status ignores runtime configuration changes", {
  source_cli_for_tests(); root <- mini_repo()
  incoming <- file.path(root, "input_data", "_new", "sna")
  config <- file.path(root, "config", "dina.yml")
  dir.create(incoming, recursive = TRUE)
  writeLines("candidate", file.path(incoming, "source.xlsx"))
  writeLines("runtime: old", config)
  record <- list(family = "sna", run = "fixture", status = "included", reviewed_at = dina_now(),
    included_at = dina_now(), acceptance_watch = dina_review_watch(c(incoming, config)))
  dina_review_save(record, root)
  writeLines("runtime: new", config)
  state <- dina_review_family_status(root, "sna")
  expect_equal(state$code, "included")
  expect_match(state$reason, "Saved candidate accepted")
})

test_that("pipeline-derived files beside legacy source folders do not stale an accepted review", {
  source_cli_for_tests(); root <- mini_repo()
  source_dir <- file.path(root, "input_data", "admin_data", "COL")
  dir.create(source_dir, recursive = TRUE)
  writeLines("accepted source", file.path(source_dir, "source.xlsx"))
  record <- list(family = "admin", run = "fixture", status = "included", reviewed_at = dina_now(),
    included_at = dina_now(), acceptance_watch = dina_review_watch(source_dir))
  dina_review_save(record, root)
  derived_dir <- file.path(source_dir, "eff-tax-rate")
  dir.create(derived_dir)
  writeLines("pipeline output", file.path(derived_dir, "COL_effrates_2024.dta"))
  expect_equal(dina_review_family_status(root, "admin")$code, "included")
})

test_that("baseline suggestions preserve explicit selection and never discover a baseline", {
  source_cli_for_tests(); root <- mini_repo()
  expect_equal(dina_update_suggested_config_override(root)$export_validation$previous_update_file, "")
  a <- workspace_baseline(root, "alternative-a.dta")
  expect_equal(dina_update_suggested_config_override(root)$export_validation$previous_update_file, "")
  b <- workspace_baseline(root, "alternative-b.dta")
  expect_equal(dina_update_suggested_config_override(root)$export_validation$previous_update_file, "")
  cfg <- dina_config(root, expand_env = FALSE); cfg$export_validation$previous_update_file <- a
  expect_equal(dina_update_suggested_config_override(root, cfg)$export_validation$previous_update_file, a)
  session <- workspace_session(root)
  input <- textConnection("2\n"); on.exit(close(input))
  capture.output(dina_settings_choose_baseline(root, session, input, TRUE))
  expect_equal(dina_session_config(session, root)$export_validation$previous_update_file, b)
  expect_match(paste(readLines(dina_session_config_path(session$id, root)), collapse = "\n"), "# Settings")
})

test_that("lightweight settings checks defer comparison-baseline validation", {
  source_cli_for_tests(); root <- mini_repo(); workspace_baseline(root); session <- workspace_session(root)
  check <- dina_settings_check(root, session, validate_baseline = FALSE)
  expect_true(check$valid)
  expect_length(check$baseline, 0L)
  expect_match(paste(dina_workspace_lines(root, session), collapse = "\n"), "Not validated")
})

test_that("full configuration validation reports its baseline-read progress", {
  source_cli_for_tests(); root <- mini_repo(); workspace_baseline(root); session <- workspace_session(root)
  messages <- character()
  check <- dina_settings_check(root, session, progress = function(message) messages <<- c(messages, message))
  expect_true(check$valid)
  expect_match(paste(messages, collapse = "\n"), "Checking configuration settings")
  expect_match(paste(messages, collapse = "\n"), "Reading comparison baseline")
  expect_match(paste(messages, collapse = "\n"), "fields and keys are valid")
})

test_that("configuration validation receipts persist and become stale only when inputs change", {
  source_cli_for_tests(); root <- mini_repo(); workspace_baseline(root); session <- workspace_session(root)
  capture.output(dina_settings_print(root, session, progress = function(...) NULL))
  session <- dina_load_session(root = root)
  expect_equal(session$config_validation$status, "passed")
  expect_true(nzchar(session$config_validation$baseline$sha256))
  expect_equal(dina_config_validation_state(root, session)$code, "validated")
  original_check <- dina_baseline_check
  dina_baseline_check <- function(...) stop("Dashboard must not read the baseline")
  expect_match(paste(dina_workspace_lines(root, session), collapse = "\n"), "Validated")
  dina_baseline_check <- original_check
  path <- dina_session_config_path(session$id, root)
  original_override <- readLines(path)
  writeLines(c(original_override, "# Changed after validation"), path)
  expect_equal(dina_config_validation_state(root, dina_load_session(root = root))$code, "out_of_date")
  writeLines(original_override, path)
  capture.output(dina_settings_print(root, dina_load_session(root = root), progress = function(...) NULL))
  benchmark_path <- dina_config_path(root)
  original_benchmark <- readLines(benchmark_path)
  writeLines(c(original_benchmark, "# Changed after validation"), benchmark_path)
  expect_equal(dina_config_validation_state(root, dina_load_session(root = root))$code, "out_of_date")
  writeLines(original_benchmark, benchmark_path)
  capture.output(dina_settings_print(root, dina_load_session(root = root), progress = function(...) NULL))
  baseline <- file.path(root, "input_data", "_new", "previous_series", "dina_latam_3Oct2024.dta")
  Sys.setFileTime(baseline, Sys.time() + 60)
  expect_equal(dina_config_validation_state(root, dina_load_session(root = root))$code, "out_of_date")
})

test_that("failed and legacy configuration validation states are explicit", {
  source_cli_for_tests(); root <- mini_repo(); session <- workspace_session(root)
  capture.output(dina_settings_print(root, session, progress = function(...) NULL))
  session <- dina_load_session(root = root)
  expect_equal(session$config_validation$status, "failed")
  expect_equal(dina_config_validation_state(root, session)$code, "needs_attention")
  session$config_validation <- NULL; dina_save_session(session, root)
  unlink(dina_config_validation_receipt_path(root, session))
  expect_equal(dina_config_validation_state(root, dina_load_session(root = root))$code, "not_validated")
})

test_that("every task requires an explicit current configuration validation", {
  skip_if_not_installed("processx")
  source_cli_for_tests(); root <- mini_repo(); workspace_baseline(root); session <- workspace_session(root)
  fake_stata <- file.path(root, "fake-stata")
  marker <- file.path(root, "output", "export-preflight-ran")
  writeLines(c("#!/bin/sh", paste("touch", shQuote(marker))), fake_stata)
  Sys.chmod(fake_stata, "0755")
  task <- list(id = "export-fixture", type = "stata", requirements = "configuration_validation",
    script = "code/Stata/01a.do", inputs = character(), outputs = marker)
  session <- withr::with_envvar(c(DINA_STATA_CMD = fake_stata), {
    check <- dina_settings_check(root, session, validate_runtime = FALSE)
    dina_record_config_validation(root, session, check)
  })
  result <- withr::with_envvar(c(DINA_STATA_CMD = fake_stata), {
    dina_run_task(task, root, session, dry_run = FALSE, force = TRUE)
  })
  expect_equal(result$status, "succeeded")
  expect_true(file.exists(marker))
  expect_equal(dina_load_session(root = root)$config_validation$status, "passed")

  bad_root <- mini_repo(); bad_session <- workspace_session(bad_root)
  bad_marker <- file.path(bad_root, "output", "export-preflight-ran")
  bad_stata <- file.path(bad_root, "fake-stata")
  writeLines(c("#!/bin/sh", paste("touch", shQuote(bad_marker))), bad_stata); Sys.chmod(bad_stata, "0755")
  expect_error(withr::with_envvar(c(DINA_STATA_CMD = bad_stata), {
    dina_run_task(task, bad_root, bad_session, dry_run = FALSE, force = TRUE)
  }), "dina update config check")
  expect_false(file.exists(bad_marker))
})

test_that("configuration checks preserve commented edits and separate baseline availability", {
  source_cli_for_tests(); root <- mini_repo(); workspace_baseline(root); session <- workspace_session(root)
  expect_true(dina_settings_check(root, session)$valid)
  path <- dina_session_config_path(session$id, root)
  writeLines(c(readLines(path), "# My update notes"), path)
  before <- readLines(path)
  dina_write_suggested_session_config_override(session, root, overwrite = FALSE)
  expect_equal(readLines(path), before)
  unlink(file.path(root, "input_data", "_new", "previous_series", "dina_latam_3Oct2024.dta"))
  check <- dina_settings_check(root, session)
  expect_length(check$errors, 0); expect_length(check$baseline, 1)
  writeLines(c("years:", "  first: 2026", "  last: 2000", "unknown_setting: true"), path)
  expect_match(paste(dina_settings_check(root, session)$errors, collapse = " "), "Unsupported.*ordered")
  output <- paste(capture.output(dina_update_config_edit(session, root, launch_editor = FALSE)), collapse = "\n")
  expect_match(output, "Opening:")
  writeLines("years: [", path)
  expect_false(dina_settings_check(root, session)$valid)
})

test_that("configuration proposes a detected Stata executable and saves it only after confirmation", {
  source_cli_for_tests(); root <- mini_repo(); session <- workspace_session(root)
  app_root <- file.path(root, "test-applications")
  executable <- file.path(app_root, "StataFixture.app", "Contents", "MacOS", "stata-mp")
  dir.create(dirname(executable), recursive = TRUE)
  writeLines(c("#!/bin/sh", "exit 0"), executable)
  Sys.chmod(executable, "0755")

  withr::with_envvar(c(DINA_STATA_CMD = "", DINA_STATA_APP_DIRS = app_root), {
    status <- dina_settings_stata_status(root, session)
    expect_false(status$available)
    expect_equal(status$proposal, executable)
    expect_match(paste(dina_settings_stata_lines(root, session), collapse = "\n"), "Use detected Stata")
    session <- dina_session_config_set(session, root, "stata.command", status$proposal)
    saved <- dina_settings_stata_status(root, session)
    expect_true(saved$available)
    expect_equal(saved$command, executable)
    expect_equal(saved$source, "this update's configuration")
  })
})

test_that("baseline validation catches missing fields and duplicate keys", {
  source_cli_for_tests(); root <- mini_repo(); rel <- workspace_baseline(root)
  full <- file.path(root, rel); data <- haven::read_dta(full)
  haven::write_dta(rbind(data, data), full)
  expect_match(dina_baseline_check(rel, root), "duplicate")
  haven::write_dta(data["value"], full)
  expect_match(dina_baseline_check(rel, root), "required fields")
})

test_that("comparison baselines must stay in the configured baseline directory", {
  source_cli_for_tests(); root <- mini_repo(); workspace_baseline(root)
  expect_match(dina_baseline_check("previous_series/dina_latam_3Oct2024.dta", root), "must be stored in input_data/_new/previous_series")
})

test_that("pipeline tracking separates recorded outcomes from file observations", {
  source_cli_for_tests(); root <- mini_repo(); session <- workspace_session(root)
  task <- dina_task_map(root)[[1]]
  session$task_runs[[task$id]] <- list(status = "succeeded", ended_at = dina_now(), run_id = "fixture")
  dina_save_session(session, root)
  snapshot <- dina_pipeline_snapshot(root)
  expect_equal(snapshot$latest, task$id)
  row <- snapshot$rows[snapshot$rows$task == task$id, ]
  expect_equal(row$recorded_run, "succeeded"); expect_false(row$files == "current")
  before <- dina_hash_path(file.path(root, "output"))
  capture.output(dina_pipeline_print(root))
  expect_equal(dina_hash_path(file.path(root, "output")), before)
})

test_that("Results records graph provenance and detects changed or incomplete exports", {
  source_cli_for_tests(); root <- mini_repo(); workspace_baseline(root)
  cfg <- dina_session_config(NULL, root, expand_env = FALSE); date <- "7Sep2026"
  dina_results_begin(root, date, cfg)
  expect_error(dina_results_complete(root, date, cfg), "incomplete")
  expect_false(file.exists(dina_results_metadata_path(root, date)))
  dir.create(file.path(root, "output", "latest_wid_series"), recursive = TRUE)
  for (group in names(dina_results_graph_names())) writeLines("PDF fixture", file.path(root, "output", "figures", "updates", paste0("update-", date, "-sptinc992j-", group, ".pdf")))
  for (file in c(paste0("dina_latam_", date, ".dta"), paste0("dina_latam_wide_", date, ".dta"), paste0("dina_latam_", date, "_amory.dta"))) writeLines("series fixture", file.path(root, "output", "latest_wid_series", file))
  expect_true(all(dina_results_inventory(root)$graphs$status == "Generation baseline unknown"))
  dina_results_complete(root, date, cfg)
  expect_true(all(dina_results_inventory(root)$graphs$status == "Matches recorded baseline and settings"))
  config_path <- file.path(root, "config", "dina.yml")
  config_before <- readLines(config_path)
  changed <- cfg; changed$years$last <- 2025L
  dina_write_yaml(changed, config_path)
  expect_true(all(dina_results_inventory(root)$graphs$status == "Settings changed; regenerate export"))
  writeLines(config_before, config_path)
  baseline_path <- file.path(root, cfg$export_validation$previous_update_file)
  baseline_before <- readBin(baseline_path, "raw", n = file.info(baseline_path)$size)
  data <- haven::read_dta(baseline_path); data$value[1] <- .9; haven::write_dta(data, baseline_path)
  expect_true(all(dina_results_inventory(root)$graphs$status == "Baseline changed or missing; regenerate export"))
  writeBin(baseline_before, baseline_path)
  opened <- NULL
  dina_results_open(root, "t10", viewer = function(path) opened <<- path)
  expect_match(opened, "t10.pdf", fixed = TRUE)
  writeLines("changed PDF", opened)
  expect_true(all(dina_results_inventory(root)$graphs$status == "Generated artifacts changed or missing"))
  dina_results_begin(root, date, cfg)
  expect_false(file.exists(dina_results_metadata_path(root, date)))
  expect_true(all(dina_results_inventory(root)$graphs$status == "Generation baseline unknown"))
})

test_that("home and typed result inspection do not mutate or launch anything", {
  source_cli_for_tests(); root <- mini_repo()
  before <- dina_hash_path(root)
  text <- paste(capture.output(dina_workspace_home(root, is_terminal = FALSE)), collapse = "\n")
  for (area in c("Configuration", "Sources", "Pipeline", "Results")) expect_match(text, area)
  expect_false(grepl("Run recommended action|Next likely command", text))
  expect_equal(run_dina_cli(c("results", "show"), root)$status, 0)
  expect_equal(dina_hash_path(root), before)
})


test_that("source selection returns to the same home without silently choosing SNA", {
  source_cli_for_tests(); root <- mini_repo()
  input <- textConnection("2\n3\n5\nq\nq\n"); on.exit(close(input))
  text <- paste(capture.output(dina_workspace_home(root, input, TRUE)), collapse = "\n")
  expect_match(text, "Household surveys")
  expect_match(text, "dina sources explore surveys", fixed = TRUE)
  expect_match(text, "No incoming files")
  expect_false(dir.exists(file.path(root, "output", "experiments")))
})

test_that("explicitly unselected baseline does not block other runtime configuration", {
  root <- mini_repo(); cfg <- dina_config(root)
  cfg$export_validation$previous_update_file <- ""
  cfg$export_validation$previous_update_date <- ""
  expect_true(any(grepl('global previous_update ""', dina_render_config_do(cfg), fixed = TRUE)))
})


test_that("WID country summaries normalize aliases without changing comparison keys", {
  source_cli_for_tests()
  rows <- data.frame(country = c("Argentina", "AR", NA), iso = c(NA, NA, "AR"), year = 2000)
  metadata <- list(list(wid_area = "AR", iso3 = "ARG", country_name = "Argentina"))
  rows$review_country <- dina_review_country(rows, metadata)
  expect_equal(rows$review_country, rep("ARG", 3))
  expect_equal(rows$country, c("Argentina", "AR", NA))
  expect_equal(rows$iso, c(NA, NA, "AR"))
  text <- paste(capture.output(dina_review_show_rows(rows, country = "ARG")), collapse = "\n")
  expect_match(text, "Argentina")
})

test_that("damaged graph metadata remains inspectable without claiming verification", {
  source_cli_for_tests(); root <- mini_repo()
  dir <- file.path(root, "output", "figures", "updates"); dir.create(dir, recursive = TRUE)
  writeLines("PDF", file.path(dir, "update-7Sep2026-sptinc992j-t10.pdf"))
  writeLines('{"status":', file.path(dir, "comparison-7Sep2026.json"))
  expect_equal(dina_results_inventory(root)$graphs$status, "Generation baseline unknown")
  output <- paste(capture.output(dina_results_open(root, "t10", viewer = function(path) invisible(path))), collapse = "\n")
  expect_match(output, "Verifying the selected graph")
})


test_that("configuration validation rejects an empty comparison period and invalid units", {
  source_cli_for_tests(); root <- mini_repo(); session <- workspace_session(root)
  dina_write_yaml(list(years = list(first = 2024L, last = 2025L), run = list(units = "unknown"),
    export_validation = list(last_year = 2023L, unit = c("ind", "esn"))), dina_session_config_path(session$id, root))
  errors <- paste(dina_settings_check(root, session)$errors, collapse = " ")
  expect_match(errors, "must not precede")
  expect_match(errors, "Run units")
  expect_match(errors, "Choose one supported")
})


test_that("configuration menus retain settings, origins and navigation on screen", {
  source_cli_for_tests(); root <- mini_repo(); workspace_baseline(root); session <- workspace_session(root)
  initial_context <- dina_settings_lines(root, session)
  contexts <- list(); actions <- c("check", "full", "back"); pauses <- 0L
  dina_menu_select <- function(..., context) {
    contexts[[length(contexts) + 1L]] <<- context
    actions[[length(contexts)]]
  }
  dina_cli_prompt_value <- function(...) { pauses <<- pauses + 1L; "" }
  output <- paste(capture.output(dina_workspace_config(root, is_terminal = TRUE)), collapse = "\n")
  expect_equal(contexts[[1]], initial_context)
  first <- paste(contexts[[1]], collapse = "\n")
  expect_match(first, "Benchmark: config/dina.yml", fixed = TRUE)
  expect_match(first, "Benchmark +This update")
  expect_match(first, "Run years +2000–2023 +2000–2024 [*]")
  expect_false(grepl("config.override.yml|export_validation[.]|Effective settings =", first))
  expect_match(output, "This update's editable file", fixed = TRUE)
  expect_match(output, "config.override.yml", fixed = TRUE)
  expect_true(all(nchar(contexts[[1]], type = "width") <= 80L))
  expect_match(paste(contexts[[2]], collapse = "\n"), "Needs attention")
  expect_match(output, "Effective YAML:")
  expect_equal(pauses, 2L)
  expect_equal(length(contexts), 3L)
})

test_that("update creation review keeps its configuration in the menu context", {
  source_cli_for_tests(); root <- mini_repo(); workspace_baseline(root); session <- workspace_session(root)
  shown <- NULL
  dina_menu_select <- function(..., context) { shown <<- context; "continue" }
  dina_review_update_config_override(session, root, is_terminal = TRUE)
  expect_equal(shown, dina_settings_lines(root, session))
})

test_that("home never scans task freshness even when no source action is pending", {
  source_cli_for_tests(); root <- mini_repo(); session <- workspace_session(root)
  calls <- 0L
  dina_task_status <- function(...) { calls <<- calls + 1L; list(status = "current") }
  output <- paste(capture.output(dina_workspace_home(root, is_terminal = FALSE)), collapse = "\n")
  expect_equal(calls, 0L)
  expect_match(output, "File freshness: checked when you open Pipeline", fixed = TRUE)
  expect_match(output, "Recorded failures: 0")
  expect_false(grepl("tasks need attention|All declared outputs", output))
  dina_pipeline_snapshot(root, session)
  expect_gt(calls, 0L)
  task <- names(dina_task_map(root))[1]
  session$task_runs[[task]] <- list(status = "failed", ended_at = dina_now())
  dina_save_session(session, root)
  expect_equal(dina_dashboard_state_fast(session, root)$state, "failed")
  expect_match(paste(dina_workspace_lines(root, session), collapse = "\n"), "Recorded failures: 1")
})

test_that("configuration editor commands are graphical launch commands without wait flags", {
  source_cli_for_tests()
  expect_equal(dina_editor_command("open -t"), list(command = "open", args = "-t"))
  expect_equal(dina_editor_command("code")$args, character())
  expect_equal(dina_editor_command("'/tmp/my editor' --option 'two words'"), list(command = "/tmp/my editor", args = c("--option", "two words")))
})

test_that("configuration edit launches an external editor without terminal validation", {
  source_cli_for_tests(); root <- mini_repo(); workspace_baseline(root); session <- workspace_session(root)
  before <- dina_hash_file(dina_session_config_path(session$id, root))
  launched <- NULL
  notices <- character()
  dina_launch_editor <- function(path, editor) { launched <<- list(path = path, editor = editor); 0L }
  dina_cli_ok <- function(text) notices <<- c(notices, text)
  checked <- 0L; checker <- dina_settings_check
  dina_settings_check <- function(...) { checked <<- checked + 1L; checker(...) }
  output <- paste(capture.output(dina_update_config_edit(session, root, editor = "code")), collapse = "\n")
  expect_equal(launched, list(path = dina_session_config_path(session$id, root), editor = "code"))
  expect_match(paste(notices, collapse = "\n"), "Opened the update file in your editor")
  expect_match(output, "Validate configuration and baseline")
  expect_equal(checked, 0L)
  expect_equal(dina_hash_file(dina_session_config_path(session$id, root)), before)
})

test_that("configuration editor launch errors do not validate or mutate settings", {
  source_cli_for_tests(); root <- mini_repo(); workspace_baseline(root); session <- workspace_session(root)
  before <- dina_hash_file(dina_session_config_path(session$id, root))
  dina_launch_editor <- function(...) 1L
  warnings <- character()
  dina_cli_warn <- function(text) warnings <<- c(warnings, text)
  checked <- 0L; checker <- dina_settings_check
  dina_settings_check <- function(...) { checked <<- checked + 1L; checker(...) }
  output <- paste(capture.output(dina_update_config_edit(session, root, editor = "missing-editor")), collapse = "\n")
  expect_match(paste(warnings, collapse = "\n"), "Could not open the editor")
  expect_equal(checked, 0L)
  expect_equal(dina_hash_file(dina_session_config_path(session$id, root)), before)
})
