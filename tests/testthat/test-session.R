test_that("update start creates a session and active pointer", {
  root <- mini_repo()
  touch(file.path(root, "input_data", "source_2024.xlsx"), "2024-01-01")
  session <- dina_update_start("2026", root = root)
  expect_equal(session$id, sprintf("2026-update-%s", format(Sys.Date(), "%m-%d")))
  expect_true(file.exists(file.path(root, "output", "updates", ".active_update")))
  expect_true(file.exists(file.path(root, session$config_file)))
  expect_equal(session$source_baseline$hash_mode, "all")
  expect_true(dina_signature_has_hash(session$source_scan[["source-a"]]$files[[1]]))
  expect_true(dir.exists(file.path(root, "input_data", "_new", "fixture")))

  loaded <- dina_load_session(root = root)
  expect_equal(loaded$id, session$id)
  expect_equal(loaded$status, "initialized")
  expect_true(is.list(loaded$gate_records))
  expect_true(is.null(loaded$checklist))
})

test_that("dashboard state is no_active_update without session", {
  root <- mini_repo()
  state <- dina_session_state(NULL, root = root)
  expect_equal(state$state, "no_active_update")
})

test_that("session state follows roadmap gates before recommending pipeline", {
  root <- mini_repo()
  session <- dina_update_start("2026", root = root)
  state <- dina_session_state(session, root = root)
  expect_equal(state$state, "gate_pending")
  expect_match(state$recommendation, "dina update gate parameters")

  record <- dina_update_mark_gate(session, root = root, target = "parameters/year-scope", status = "done")
  expect_equal(record$status, "done")
  session <- dina_load_session(root = root)
  state <- dina_session_state(session, root = root)
  expect_equal(state$next_gate, "tax-admin")
  expect_error(dina_update_mark_gate(session, root = root, target = "tax-admin/raw-accepted", status = "deferred"), "requires --note")
})
