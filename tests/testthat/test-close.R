test_that("update close report summarizes state without enforcing blockers", {
  root <- mini_repo()
  session <- dina_update_start("2026", root = root)
  dir.create(file.path(root, "input_data", "_new", "fixture"), recursive = TRUE, showWarnings = FALSE)
  writeLines("new", file.path(root, "input_data", "_new", "fixture", "source_2025.xlsx"))
  session <- dina_update_todo_state(session, root = root, id = "review-config", checked = TRUE)

  source_cli_for_tests()
  report <- dina_update_close_report(session, root)

  expect_true(nrow(report$sources) >= 1L)
  expect_true(nrow(report$incoming) >= 1L)
  expect_true(nrow(report$tasks) >= 1L)
  expect_true(nrow(report$todo) >= 1L)
  expect_equal(sum(report$todo$checked), 1L)
  expect_true(is.logical(report$config_override))
})

test_that("update close dry-run prints report and leaves status unchanged", {
  root <- mini_repo()
  dina_update_start("2026", root = root)

  result <- run_dina_cli(c("update", "close", "--dry-run"), root = root)

  expect_equal(result$status, 0L)
  expect_match(result$output, "Update Close Report")
  expect_match(result$output, "Dry-run only")
  expect_equal(dina_load_session(root = root)$status, "initialized")
})

test_that("update close marks workspace closed after reporting", {
  root <- mini_repo()
  dina_update_start("2026", root = root)

  result <- run_dina_cli(c("update", "close"), root = root)

  expect_equal(result$status, 0L)
  expect_match(result$output, "Update workspace closed")
  loaded <- dina_load_session(root = root)
  expect_equal(loaded$status, "closed")
  expect_true(nzchar(loaded$closed_at))
})
