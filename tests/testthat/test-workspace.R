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

test_that("saved SNA reviews show revisions before empty grids without rewriting evidence", {
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
  expect_match(text, "86 numerical revisions")
  expect_match(text, "41 lost values")
  expect_match(text, "more than one destination \\(21\\)")
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
  record$status <- "included"; dina_review_save(record, root)
  expect_equal(dina_review_family_status(root, "sna")$code, "included")
  expect_null(dina_review_recommendation(root))
  writeLines("different candidate", file.path(incoming, "data"))
  expect_equal(dina_review_family_status(root, "sna")$code, "stale")
  record$status <- "inclusion_failed"; dina_review_save(record, root)
  expect_equal(dina_review_family_status(root, "sna")$code, "failed")
})

test_that("baseline suggestions preserve explicit selection and never choose by recency", {
  source_cli_for_tests(); root <- mini_repo()
  expect_equal(dina_update_suggested_config_override(root)$export_validation$previous_update_file, "")
  a <- workspace_baseline(root, "alternative-a.dta")
  expect_equal(dina_update_suggested_config_override(root)$export_validation$previous_update_file, a)
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
  output <- paste(capture.output(dina_update_config_edit(session, root, open_editor = FALSE)), collapse = "\n")
  expect_match(output, "Settings need attention")
  expect_false(grepl("settings validated", output))
  writeLines("years: [", path)
  expect_false(dina_settings_check(root, session)$valid)
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
  expect_silent(dina_results_open(root, "t10", viewer = function(path) invisible(path)))
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
  contexts <- list(); actions <- c("check", "full", "back"); pauses <- 0L
  dina_menu_select <- function(..., context) {
    contexts[[length(contexts) + 1L]] <<- context
    actions[[length(contexts)]]
  }
  dina_cli_prompt_value <- function(...) { pauses <<- pauses + 1L; "" }
  output <- paste(capture.output(dina_workspace_config(root, is_terminal = TRUE)), collapse = "\n")
  expect_equal(contexts[[1]], dina_settings_lines(root, session))
  first <- paste(contexts[[1]], collapse = "\n")
  expect_match(first, "Benchmark: config/dina.yml", fixed = TRUE)
  expect_match(first, "Benchmark +This update")
  expect_match(first, "Run years +2000–2023 +2000–2024 [*]")
  expect_false(grepl("config.override.yml|export_validation[.]|Effective settings =", first))
  expect_match(output, "This update's editable file", fixed = TRUE)
  expect_match(output, "config.override.yml", fixed = TRUE)
  expect_true(all(nchar(contexts[[1]], type = "width") <= 80L))
  expect_match(paste(contexts[[2]], collapse = "\n"), "Settings checked again")
  expect_match(output, "Effective YAML:")
  expect_equal(pauses, 1L)
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
  expect_match(output, "File freshness is checked when you open Pipeline", fixed = TRUE)
  expect_match(output, "0 recorded failures")
  expect_false(grepl("tasks need attention|All declared outputs", output))
  dina_pipeline_snapshot(root, session)
  expect_gt(calls, 0L)
  task <- names(dina_task_map(root))[1]
  session$task_runs[[task]] <- list(status = "failed", ended_at = dina_now())
  dina_save_session(session, root)
  expect_equal(dina_dashboard_state_fast(session, root)$state, "failed")
  expect_match(paste(dina_workspace_lines(root, session), collapse = "\n"), "1 recorded failures")
})

test_that("editor instructions explain saving and GUI launchers wait", {
  source_cli_for_tests()
  expect_match(paste(dina_editor_help("vi"), collapse = " "), "press i.*:wq.*:q!")
  expect_match(paste(dina_editor_help("nano"), collapse = " "), "Ctrl[+]O.*Ctrl[+]X")
  expect_match(paste(dina_editor_help("open -t"), collapse = " "), "Cmd[+]S.*Cmd[+]Q")
  expect_true("-W" %in% dina_editor_command("open -t")$args)
  expect_true("--wait" %in% dina_editor_command("code")$args)
  expect_equal(sum(dina_editor_command("code -w")$args %in% c("-w", "--wait")), 1L)
  expect_equal(dina_editor_command("'/tmp/my editor' --option 'two words'"), list(command = "/tmp/my editor", args = c("--option", "two words")))
})

test_that("editor receives literal arguments and paths containing spaces", {
  skip_on_os("windows")
  source_cli_for_tests(); root <- mini_repo()
  editor <- file.path(root, "fixture editor")
  log <- file.path(root, "editor arguments")
  path <- file.path(root, "settings 'quoted' $value.yml")
  writeLines(c("#!/bin/sh", paste("printf '%s\\n' \"$@\" >", shQuote(log))), editor)
  Sys.chmod(editor, "0755")
  expect_equal(dina_open_editor(path, paste(shQuote(editor), "--option 'two words'")), 0L)
  expect_equal(readLines(log), c("--option", "two words", path))
})

test_that("cancelled, unopened and failed editors do not record a configuration edit", {
  source_cli_for_tests(); root <- mini_repo(); workspace_baseline(root); session <- workspace_session(root)
  before <- dina_hash_path(dina_update_dir(session$id, root))
  dina_open_editor <- function(...) 0L
  output <- paste(capture.output(dina_update_config_edit(session, root, open_editor = TRUE)), collapse = "\n")
  expect_match(output, "No changes saved")
  expect_equal(dina_hash_path(dina_update_dir(session$id, root)), before)
  capture.output(dina_update_config_edit(session, root, open_editor = FALSE))
  expect_equal(dina_hash_path(dina_update_dir(session$id, root)), before)
  dina_open_editor <- function(...) 1L
  checked <- 0L; checker <- dina_settings_check
  dina_settings_check <- function(...) { checked <<- checked + 1L; checker(...) }
  capture.output(dina_update_config_edit(session, root, open_editor = TRUE))
  expect_equal(checked, 1L) # Only the pre-edit summary; no success validation.
  expect_equal(dina_hash_path(dina_update_dir(session$id, root)), before)
})

test_that("saved editor changes are reloaded before validation", {
  source_cli_for_tests(); root <- mini_repo(); workspace_baseline(root); session <- workspace_session(root)
  path <- dina_session_config_path(session$id, root)
  dina_open_editor <- function(path, ...) { writeLines(c("# Keep my note", "years:", "  first: 2030", "  last: 2000"), path); 0L }
  output <- paste(capture.output(dina_update_config_edit(session, root, open_editor = TRUE)), collapse = "\n")
  expect_match(output, "Update file changed")
  expect_match(output, "Settings need attention")
  expect_match(output, "ordered integer bounds")
  expect_equal(dina_load_session(root = root)$config_override_hash, dina_hash_file(path))
  expect_equal(readLines(path)[1], "# Keep my note")
})


test_that("editing feedback stays visible until returning to Configuration", {
  source_cli_for_tests(); root <- mini_repo(); workspace_baseline(root); workspace_session(root)
  actions <- c("edit", "back"); events <- character()
  dina_menu_select <- function(...) { action <- actions[1]; actions <<- actions[-1]; events <<- c(events, action); action }
  dina_update_config_edit <- function(...) { events <<- c(events, "editor result"); invisible(NULL) }
  dina_cli_prompt_value <- function(...) { events <<- c(events, "return prompt"); "" }
  dina_workspace_config(root, is_terminal = TRUE)
  expect_equal(events, c("edit", "editor result", "return prompt", "back"))
})

test_that("an editor that removes the override cannot validate inherited defaults", {
  source_cli_for_tests(); root <- mini_repo(); workspace_baseline(root); session <- workspace_session(root)
  manifest <- dina_load_session(root = root)
  dina_open_editor <- function(path, ...) { unlink(path); 0L }
  checked <- 0L; checker <- dina_settings_check
  dina_settings_check <- function(...) { checked <<- checked + 1L; checker(...) }
  capture.output(dina_update_config_edit(session, root, open_editor = TRUE))
  expect_equal(checked, 1L)
  expect_equal(dina_load_session(root = root), manifest)
})
