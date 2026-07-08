Sys.unsetenv("LC_ALL")

dina_session_state_condition <- get("testthat_state_condition", asNamespace("testthat"))
assignInNamespace("testthat_state_condition", function(before, after, call = NULL) NULL, ns = "testthat")
if (requireNamespace("withr", quietly = TRUE)) {
  withr::defer(
    assignInNamespace("testthat_state_condition", dina_session_state_condition, ns = "testthat"),
    envir = testthat:::teardown_env()
  )
}

expect_true <- function(object, info = NULL, label = NULL) {
  value <- force(object)
  if (!isTRUE(value)) testthat::fail(info %||% "Expected TRUE.")
  invisible(value)
}

expect_false <- function(object, info = NULL, label = NULL) {
  value <- force(object)
  if (!identical(value, FALSE)) testthat::fail(info %||% "Expected FALSE.")
  invisible(value)
}

test_that("update start creates a lightweight workspace and active pointer", {
  root <- mini_repo()
  touch(file.path(root, "input_data", "source_2024.xlsx"), "2024-01-01")
  session <- dina_update_start("2026", root = root)

  expect_equal(session$id, sprintf("2026-update-%s", format(Sys.Date(), "%m-%d")))
  expect_true(file.exists(file.path(root, "output", "updates", ".active_update")))
  expect_true(file.exists(file.path(root, session$config_override)))
  expect_true(nzchar(session$config_override_hash))
  override <- dina_config_override(session, root)
  expect_equal(override$years$last, 2024L)
  expect_equal(override$export_validation$last_year, 2025L)
  effective <- dina_session_config(session, root, expand_env = FALSE)
  expect_equal(effective$years$last, 2024L)
  expect_equal(effective$export_validation$last_year, 2025L)
  expect_true(file.exists(file.path(root, "output", "updates", session$id, "repo_state", "start", "metadata.json")))
  expect_false(dir.exists(file.path(root, "output", "updates", session$id, "source_staging")))
  expect_equal(session$todo$checked, character())
  expect_equal(session$source_baseline$hash_mode, "all")
  expect_true(dina_signature_has_hash(session$source_scan[["source-a"]]$files[[1]]))
  expect_true(dir.exists(file.path(root, "input_data", "_new", "fixture")))

  loaded <- dina_load_session(root = root)
  expect_equal(loaded$id, session$id)
  expect_equal(loaded$status, "initialized")
  expect_true(is.null(loaded$gate_records))
  expect_true(is.null(loaded$checklist))

  restarted <- dina_update_restart(session$id, root = root, yes = TRUE)
  expect_true(isTRUE(restarted$restarted))
  expect_true(file.exists(dina_session_config_override_path(session$id, root)))
  expect_equal(dina_config_override(restarted$new_session, root)$years$last, 2024L)
})

test_that("dashboard state is no_active_update without session", {
  root <- mini_repo()
  state <- dina_session_state(NULL, root = root)
  expect_equal(state$state, "no_active_update")
})

test_that("update restart preserves valid todo state and drops stale ids", {
  root <- mini_repo()
  session <- dina_update_start("2026", root = root)
  session <- dina_update_todo_state(session, root = root, id = "review-config", checked = TRUE)
  session$todo$checked <- c(session$todo$checked, "stale-todo")
  dina_save_session(session, root)

  restarted <- dina_update_restart(session$id, root = root, yes = TRUE)

  expect_equal(restarted$new_session$todo$checked, "review-config")
  expect_equal(dina_load_session(root = root)$todo$checked, "review-config")
})

test_that("update start and restart print config proposal in CLI", {
  root <- mini_repo()

  started <- run_dina_cli(c("update", "start", "2026", "--yes"), root = root)
  expect_equal(started$status, 0L)
  expect_match(started$output, "Config Proposal")
  expect_match(started$output, "config.override.yml")
  expect_match(started$output, "key[[:space:]]+current[[:space:]]+proposed[[:space:]]+reason")
  expect_match(started$output, "years\\.last[[:space:]]+2023[[:space:]]+2024[[:space:]]+next update year")
  expect_match(started$output, "export_validation\\.last_year[[:space:]]+2024[[:space:]]+2025[[:space:]]+next export validation year")
  expect_match(started$output, "Override YAML")
  expect_match(started$output, "years:")
  expect_match(started$output, "last: 2024")
  expect_match(started$output, "export_validation:")
  expect_match(started$output, "last_year: 2025")
  expect_match(started$output, "Effective config:")
  expect_match(started$output, "project:")
  expect_match(started$output, "Edit protocol:")
  expect_match(started$output, "Do not edit config/dina.yml")
  expect_match(started$output, "dina update config show")
  expect_match(started$output, "Workflow reminder: `dina help workflow`")
  expect_false(grepl("Config Override", started$output, fixed = TRUE))

  session <- dina_load_session(root = root)
  dina_session_config_set(session, root = root, key = "run.lang", value = "spa")
  shown <- run_dina_cli(c("update", "config", "show"), root = root)
  expect_equal(shown$status, 0L)
  expect_match(shown$output, "Config Proposal")
  expect_match(shown$output, "run\\.lang[[:space:]]+eng[[:space:]]+spa[[:space:]]+manual override")
  expect_match(shown$output, "Effective config:")
  expect_match(shown$output, "lang: spa")
  expect_match(shown$output, "Edit protocol:")

  swap <- file.path(root, "output", "updates", dina_current_update(root), ".config.override.yml.swp")
  writeLines("swap", swap)
  edit <- run_dina_cli(c("update", "config", "edit"), root = root)
  expect_equal(edit$status, 0L)
  expect_match(edit$output, "Update Config Edit")
  expect_match(edit$output, "Edit protocol:")
  expect_match(edit$output, "No editor was opened")
  expect_match(edit$output, "Possible editor swap file")
  expect_match(edit$output, "dina update config show")

  update_id <- dina_current_update(root)
  restarted <- run_dina_cli(c("update", "restart", update_id, "--yes"), root = root)
  expect_equal(restarted$status, 0L)
  expect_match(restarted$output, "Config Proposal")
  expect_match(restarted$output, "config.override.yml")
  expect_match(restarted$output, "years\\.last[[:space:]]+2023[[:space:]]+2024")
  expect_match(restarted$output, "last: 2024")
  expect_match(restarted$output, "Effective config:")
  expect_match(restarted$output, "Edit protocol:")
  expect_false(grepl("Config Override", restarted$output, fixed = TRUE))
})

test_that("plain dashboard prints status and recommendation without common commands", {
  source_cli_for_tests()
  root <- mini_repo()
  dina_update_start("2026", root = root)

  output <- capture.output(dina_print_dashboard(root, is_terminal = FALSE))
  text <- paste(output, collapse = "\n")
  expect_match(text, "DINA-LatAm CLI")
  expect_match(text, "Project status:")
  expect_match(text, "Recommended:")
  expect_false(grepl("Common commands", text, fixed = TRUE))
  expect_false(grepl("DINA Actions", text, fixed = TRUE))
})

test_that("plain dashboard does not scan task freshness before rendering", {
  source_cli_for_tests()
  root <- mini_repo()
  dina_update_start("2026", root = root)

  env <- environment(dina_print_dashboard)
  original <- get("dina_all_task_status", envir = env)
  on.exit(assign("dina_all_task_status", original, envir = env), add = TRUE)
  assign("dina_all_task_status", function(...) stop("dashboard should not scan task freshness", call. = FALSE), envir = env)

  con <- textConnection("q\n")
  output <- capture.output(dina_print_dashboard(root, input = con, is_terminal = TRUE))
  close(con)

  text <- paste(output, collapse = "\n")
  expect_match(text, "pipeline status not checked")
  expect_match(text, "DINA Actions")
  expect_match(text, "dina run stale --dry-run")
})

test_that("interactive dashboard combines status, recommendation, and actions", {
  source_cli_for_tests()
  root <- mini_repo()
  dina_update_start("2026", root = root)

  con <- textConnection("q\n")
  output <- capture.output(dina_print_dashboard(root, input = con, is_terminal = TRUE))
  close(con)

  text <- paste(output, collapse = "\n")
  expect_match(text, "Project status:")
  expect_match(text, "Recommended:")
  expect_match(text, "DINA Actions")
  expect_match(text, "Run recommended action")
  expect_match(text, "Commands menu")
  expect_false(grepl("Common commands", text, fixed = TRUE))
})

test_that("interactive dashboard recommended action runs the proposal", {
  source_cli_for_tests()
  root <- mini_repo()
  dina_update_start("2026", root = root)

  con <- textConnection("1\n")
  output <- capture.output(dina_print_dashboard(root, input = con, is_terminal = TRUE))
  close(con)

  text <- paste(output, collapse = "\n")
  expect_match(text, "dina run stale --dry-run")
  expect_match(text, "task1: dry_run")
})

test_that("session state ignores retired source workflow and recommends runs, todos, then close", {
  root <- mini_repo()
  session <- dina_update_start("2026", root = root)
  dir.create(file.path(root, "input_data", "_new", "fixture"), recursive = TRUE, showWarnings = FALSE)
  writeLines("new", file.path(root, "input_data", "_new", "fixture", "source_2025.xlsx"))

  state <- dina_session_state(session, root = root)
  expect_equal(state$state, "build_ready")
  expect_match(state$recommendation, "dina run stale --dry-run")
  expect_equal(state$proposal$command, "dina run stale --dry-run")
  expect_equal(state$proposal$next_command, "dina run stale")
  expect_false(grepl("dina sources review", state$recommendation, fixed = TRUE))

  dir.create(file.path(root, "output"), recursive = TRUE, showWarnings = FALSE)
  writeLines("input", file.path(root, "input_data", "a.txt"))
  writeLines("a", file.path(root, "output", "a.txt"))
  writeLines("b", file.path(root, "output", "b.txt"))
  session$task_runs$task1 <- list(status = "succeeded")
  session$task_runs$task2 <- list(status = "succeeded")
  dina_save_session(session, root)
  session <- dina_load_session(root = root)

  state <- dina_session_state(session, root = root)
  expect_equal(state$state, "todo_pending")
  expect_match(state$recommendation, "dina todo")

  for (id in dina_todo_known_ids(root)) {
    session <- dina_update_todo_state(session, root = root, id = id, checked = TRUE)
  }
  state <- dina_session_state(session, root = root)
  expect_equal(state$state, "review_ready")
  expect_match(state$recommendation, "dina update close --dry-run")
  expect_equal(state$proposal$command, "dina update close --dry-run")
  expect_equal(state$proposal$next_command, "dina update close")
})

test_that("session state recommends country-SNA explore when that inbox has files", {
  root <- mini_repo()
  dina_write_yaml(list(sources = list(
    list(
      id = "country-sna-aaa",
      family = "country_sna",
      country = "AAA",
      method = "manual",
      canonical = "input_data/sna_country_data/AAA/*.xlsx",
      inbox = "input_data/_new/country_sna/AAA/*.xlsx",
      destination = "input_data/sna_country_data/AAA/{basename}",
      transformer = "code/Stata/01b-add-country-sna.do",
      notes = "Country-SNA fixture."
    )
  )), file.path(root, "config", "sources.yml"))
  session <- dina_update_start("2026", root = root)
  dir.create(file.path(root, "input_data", "_new", "country_sna", "AAA"), recursive = TRUE, showWarnings = FALSE)
  writeLines("new", file.path(root, "input_data", "_new", "country_sna", "AAA", "cei_2024.xlsx"))

  state <- dina_session_state(session, root = root)
  expect_equal(state$state, "sources_pending")
  expect_equal(state$proposal$command, "dina sources explore sna")
  expect_equal(state$proposal$next_command, "dina sources include sna --dry-run")

  status <- run_dina_cli(c("update", "status"), root = root)
  expect_equal(status$status, 0L)
  expect_match(status$output, "dina sources explore sna")
  expect_match(status$output, "dina sources include sna --dry-run")
  expect_match(status$output, "SNA incoming files can be explored")

  explore_root <- file.path(root, "output", "experiments", "country_sna_explore")
  dir.create(file.path(explore_root, "logs"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(explore_root, "tables"), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(
    data.frame(key = c("run_id", "output_root"), value = c("explore-test", explore_root), stringsAsFactors = FALSE),
    file.path(explore_root, "logs", "explore_manifest.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    dina_country_sna_inbox_signature(root),
    file.path(explore_root, "tables", "source_fingerprints.csv"),
    row.names = FALSE
  )
  state <- dina_session_state(session, root = root)
  expect_equal(state$state, "sources_explored")
  expect_equal(state$proposal$command, "dina sources include sna --dry-run")

  include_run <- file.path(root, "output", "experiments", "country_sna_include", "runs", "include-test")
  dir.create(file.path(include_run, "logs"), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(
    data.frame(
      key = c("dry_run", "status", "exploration_run"),
      value = c("TRUE", "all_good", explore_root),
      stringsAsFactors = FALSE
    ),
    file.path(include_run, "logs", "include_manifest.csv"),
    row.names = FALSE
  )
  state <- dina_session_state(session, root = root)
  expect_equal(state$state, "sources_include_ready")
  expect_match(state$proposal$command, "dina sources include sna --confirm")

  confirm_run <- file.path(root, "output", "experiments", "country_sna_include", "confirms", "confirm-test")
  dir.create(file.path(confirm_run, "logs"), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(
    data.frame(
      key = c("include_run", "status"),
      value = c(include_run, "confirmed"),
      stringsAsFactors = FALSE
    ),
    file.path(confirm_run, "logs", "confirm_manifest.csv"),
    row.names = FALSE
  )
  state <- dina_session_state(session, root = root)
  expect_equal(state$state, "sources_confirmed")
  expect_equal(state$proposal$command, "dina run 01b --dry-run")
})

test_that("update status prints structured recommendation details", {
  root <- mini_repo()
  dina_update_start("2026", root = root)
  dir.create(file.path(root, "input_data", "_new", "fixture"), recursive = TRUE, showWarnings = FALSE)
  writeLines("new", file.path(root, "input_data", "_new", "fixture", "source_2025.xlsx"))

  status <- run_dina_cli(c("update", "status"), root = root)
  expect_equal(status$status, 0L)
  expect_match(status$output, "Recommended:")
  expect_match(status$output, "Why:")
  expect_match(status$output, "Focus:")
  expect_match(status$output, "Expected action:")
  expect_match(status$output, "Next likely command:")
  expect_match(status$output, "Incoming source files: 1")
  expect_match(status$output, "no source validation/preparation workflow is implemented yet")
  expect_match(status$output, "dina run stale --dry-run")
  expect_false(grepl("dina sources review", status$output, fixed = TRUE))
  expect_false(grepl("dina sources integrate", status$output, fixed = TRUE))
})
