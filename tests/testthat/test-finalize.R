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
})
