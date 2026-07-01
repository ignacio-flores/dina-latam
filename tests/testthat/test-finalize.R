test_that("finalize refuses missing required outputs", {
  root <- mini_repo()
  session <- dina_update_start("2026", root = root)
  result <- dina_finalize_update(session, root, force = FALSE)
  expect_false(result$ok)
  expect_true(length(result$blockers) > 0)
})

test_that("finalize succeeds with force and records immutable status", {
  root <- mini_repo()
  session <- dina_update_start("2026", root = root)
  dir.create(file.path(root, "output", "latest_wid_series"), recursive = TRUE)
  writeLines("x", file.path(root, "output", "latest_wid_series", "fake.dta"))
  result <- dina_finalize_update(session, root, force = TRUE)
  expect_true(result$ok)
  loaded <- dina_load_session(root = root)
  expect_equal(loaded$status, "finalized")
  expect_true(is.list(loaded$final_repo_diff))
})

test_that("finalize can promote effective config to benchmark", {
  root <- mini_repo()
  session <- dina_update_start("2026", root = root)
  session <- dina_session_config_set(session, root = root, key = "years.last", value = "2024")
  expect_equal(dina_config(root, expand_env = FALSE)$years$last, 2023L)
  dir.create(file.path(root, "output", "latest_wid_series"), recursive = TRUE)
  writeLines("x", file.path(root, "output", "latest_wid_series", "fake.dta"))

  result <- dina_finalize_update(session, root, force = TRUE, promote_config = TRUE)
  expect_true(result$ok)
  expect_true(result$config_promoted)
  expect_equal(dina_config(root, expand_env = FALSE)$years$last, 2024L)
  expect_false(any(grepl("global last_y 2024", readLines(file.path(root, "_config.do")), fixed = TRUE)))
})
