test_that("task freshness sees missing outputs", {
  root <- mini_repo()
  touch(file.path(root, "input_data", "a.txt"), "2024-01-01")
  task <- dina_task_map(root)[["task1"]]
  status <- dina_task_status(task, root, session = NULL)
  expect_equal(status$status, "missing_outputs")
})

test_that("task freshness sees stale and current outputs", {
  root <- mini_repo()
  touch(file.path(root, "input_data", "a.txt"), "2024-01-02")
  touch(file.path(root, "output", "a.txt"), "2024-01-01")
  task <- dina_task_map(root)[["task1"]]
  expect_equal(dina_task_status(task, root, session = NULL)$status, "stale")

  touch(file.path(root, "output", "a.txt"), Sys.time() + 3600)
  expect_equal(dina_task_status(task, root, session = NULL)$status, "current")
})
