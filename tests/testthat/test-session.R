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

test_that("session state recommends sources, runs, todos, then close", {
  root <- mini_repo()
  session <- dina_update_start("2026", root = root)
  dir.create(file.path(root, "input_data", "_new", "fixture"), recursive = TRUE, showWarnings = FALSE)
  writeLines("new", file.path(root, "input_data", "_new", "fixture", "source_2025.xlsx"))

  state <- dina_session_state(session, root = root)
  expect_equal(state$state, "sources_pending")
  expect_match(state$recommendation, "dina sources review")
  expect_equal(state$proposal$command, "dina sources review")
  expect_equal(state$proposal$todo_id, "review-sources")
  expect_match(state$proposal$why, "input_data/_new")
  expect_equal(state$proposal$next_command, "dina sources integrate SOURCE")

  unlink(file.path(root, "input_data", "_new", "fixture", "source_2025.xlsx"))
  state <- dina_session_state(session, root = root)
  expect_equal(state$state, "build_ready")
  expect_match(state$recommendation, "dina run stale --dry-run")
  expect_equal(state$proposal$command, "dina run stale --dry-run")
  expect_equal(state$proposal$next_command, "dina run stale")

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
  expect_match(status$output, "dina sources integrate SOURCE")
})
