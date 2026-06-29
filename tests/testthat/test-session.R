test_that("update start creates a session and active pointer", {
  root <- mini_repo()
  session <- dina_update_start("2026", root = root)
  expect_equal(session$id, sprintf("2026-update-%s", format(Sys.Date(), "%m-%d")))
  expect_true(file.exists(file.path(root, "output", "updates", ".active_update")))
  expect_true(file.exists(file.path(root, session$config_file)))

  loaded <- dina_load_session(root = root)
  expect_equal(loaded$id, session$id)
  expect_equal(loaded$status, "initialized")
})

test_that("dashboard state is no_active_update without session", {
  root <- mini_repo()
  state <- dina_session_state(NULL, root = root)
  expect_equal(state$state, "no_active_update")
})
