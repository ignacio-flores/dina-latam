test_that("update start creates a session and active pointer", {
  root <- mini_repo()
  touch(file.path(root, "input_data", "source_2024.xlsx"), "2024-01-01")
  session <- dina_update_start("2026", root = root)
  expect_equal(session$id, sprintf("2026-update-%s", format(Sys.Date(), "%m-%d")))
  expect_true(file.exists(file.path(root, "output", "updates", ".active_update")))
  expect_true(file.exists(file.path(root, session$config_file)))
  expect_equal(session$source_baseline$hash_mode, "all")
  expect_true(dina_signature_has_hash(session$source_scan[["source-a"]]$files[[1]]))

  loaded <- dina_load_session(root = root)
  expect_equal(loaded$id, session$id)
  expect_equal(loaded$status, "initialized")
})

test_that("dashboard state is no_active_update without session", {
  root <- mini_repo()
  state <- dina_session_state(NULL, root = root)
  expect_equal(state$state, "no_active_update")
})

test_that("session state requires source review before recommending pipeline", {
  root <- mini_repo()
  session <- dina_update_start("2026", root = root)
  state <- dina_session_state(session, root = root)
  expect_equal(state$state, "sources_unreviewed")
  expect_match(state$recommendation, "dina sources status")

  review <- dina_sources_complete(session, root = root, status = "no-new-data")
  expect_equal(review$status, "no-new-data")
  session <- dina_load_session(root = root)
  state <- dina_session_state(session, root = root)
  expect_false(identical(state$state, "sources_unreviewed"))
  expect_error(dina_sources_complete(session, root = root, status = "deferred"), "needs --note")
})
