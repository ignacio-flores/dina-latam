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

test_that("update creation shows compact settings and preserves editor access", {
  root <- mini_repo()
  started <- run_dina_cli(c("update", "start", "2026", "--yes"), root)
  expect_equal(started$status, 0L)
  expect_match(started$output, "Configuration")
  expect_match(started$output, "Run years +2000–2023 +2000–2024")
  expect_match(started$output, "Comparison through +2024 +2025")
  expect_false(grepl("Override YAML|Effective YAML", started$output))
  session <- dina_load_session(root = root)
  path <- dina_session_config_path(session$id, root)
  expect_match(paste(readLines(path), collapse = "\n"), "# Settings for this update")
  dina_session_config_set(session, root, key = "run.lang", value = "esp")
  shown <- run_dina_cli(c("update", "config", "show", "--full"), root)
  expect_equal(shown$status, 0L)
  expect_match(shown$output, "Effective YAML:")
  expect_match(shown$output, "lang: esp")
  edit <- run_dina_cli(c("update", "config", "edit"), root, env = "EDITOR=/usr/bin/true")
  expect_equal(edit$status, 0L)
  expect_match(edit$output, "settings need attention")
  expect_false(grepl("settings validated", edit$output))
  restarted <- run_dina_cli(c("update", "restart", session$id, "--yes"), root)
  expect_equal(restarted$status, 0L)
  expect_match(restarted$output, "Configuration")
})

test_that("plain dashboard shows four areas and a secondary suggestion", {
  source_cli_for_tests(); root <- mini_repo(); dina_update_start("2026", root = root)
  text <- paste(capture.output(dina_print_dashboard(root, is_terminal = FALSE)), collapse = "\n")
  for (area in c("Configuration:", "Sources:", "Pipeline:", "Results:", "Suggestion:")) expect_match(text, area)
  expect_false(grepl("Project status:|Run recommended action|Next likely command", text))
})

test_that("home defers file scans and retains a concrete inspection suggestion", {
  source_cli_for_tests(); root <- mini_repo(); session <- dina_update_start("2026", root = root)
  expect_equal(dina_dashboard_state_fast(session, root)$state, "pipeline_uninspected")
  expect_equal(dina_session_state(session, root)$state, "build_ready")
  input <- textConnection("q\n"); on.exit(close(input))
  text <- paste(capture.output(dina_print_dashboard(root, input, TRUE)), collapse = "\n")
  expect_match(text, "Workspace")
  expect_match(text, "dina run list", fixed = TRUE)
})

test_that("pipeline inspection returns home without running a task", {
  source_cli_for_tests(); root <- mini_repo(); dina_update_start("2026", root = root)
  before <- dina_hash_path(file.path(root, "output"))
  input <- textConnection("3\nq\nq\n"); on.exit(close(input))
  text <- paste(capture.output(dina_print_dashboard(root, input, TRUE)), collapse = "\n")
  expect_match(text, "Explain a task")
  expect_true(sum(grepl("Update workspace", strsplit(text, "\n")[[1]])) >= 2)
  expect_equal(dina_hash_path(file.path(root, "output")), before)
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

test_that("source recommendations follow each family and invalidate changed reviews", {
  for (family in c("sna", "admin", "surveys", "wid")) {
    root <- mini_repo()
    session <- dina_update_start("2026", root = root)
    incoming <- file.path(root, "input_data", "_new", family)
    dir.create(incoming, recursive = TRUE, showWarnings = FALSE)
    path <- file.path(incoming, "new.txt")
    writeLines("new", path)
    state <- dina_session_state(session, root)
    expect_equal(state$proposal$command, paste("dina sources explore", family))
    record <- list(family = family, run = "fixture", status = "all_good", reviewed_at = dina_now(), watch = dina_review_watch(incoming))
    dina_write_json(record, dina_review_pointer(root, family))
    expect_equal(dina_session_state(session, root)$proposal$command, paste("dina sources include", family))
    record$status <- "included"
    dina_write_json(record, dina_review_pointer(root, family))
    expect_true(is.null(dina_review_recommendation(root)))
    writeLines("changed incoming", path)
    expect_equal(dina_session_state(session, root)$proposal$command, paste("dina sources explore", family))
  }
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
  expect_match(status$output, "Why:")
  expect_match(status$output, "Expected action:")
  expect_match(status$output, "Next likely command:")
  expect_match(status$output, "Incoming source files: 1")
  expect_match(status$output, "Review incoming sources by family")
  expect_match(status$output, "dina run stale --dry-run")
  expect_false(grepl("dina sources review", status$output, fixed = TRUE))
  expect_false(grepl("dina sources integrate", status$output, fixed = TRUE))
})
