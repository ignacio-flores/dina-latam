test_that("update start creates a lightweight workspace and active pointer", {
  root <- mini_repo()
  touch(file.path(root, "input_data", "source_2024.xlsx"), "2024-01-01")
  session <- dina_update_start("2026", root = root)

  expect_equal(session$id, sprintf("2026-update-%s", format(Sys.Date(), "%m-%d")))
  expect_true(file.exists(file.path(root, "output", "updates", ".active_update")))
  expect_false(file.exists(file.path(root, session$config_override)))
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
})

test_that("dashboard state is no_active_update without session", {
  root <- mini_repo()
  state <- dina_session_state(NULL, root = root)
  expect_equal(state$state, "no_active_update")
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

  dir.create(file.path(root, "output"), recursive = TRUE)
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

test_that("update status prints structured recommendation details", {
  root <- mini_repo()
  dina_update_start("2026", root = root)
  dir.create(file.path(root, "input_data", "_new", "fixture"), recursive = TRUE, showWarnings = FALSE)
  writeLines("new", file.path(root, "input_data", "_new", "fixture", "source_2025.xlsx"))

  status <- run_dina_cli(c("update", "status"), root = root)
  expect_equal(status$status, 0L)
  expect_match(status$output, "Recommended:")
  expect_match(status$output, "Why:")
  expect_match(status$output, "Todo:")
  expect_match(status$output, "Expected action:")
  expect_match(status$output, "Next likely command:")
  expect_match(status$output, "Incoming source files: 1")
  expect_match(status$output, "no source validation/preparation workflow is implemented yet")
  expect_match(status$output, "dina run stale --dry-run")
  expect_false(grepl("dina sources review", status$output, fixed = TRUE))
  expect_false(grepl("dina sources integrate", status$output, fixed = TRUE))
})
