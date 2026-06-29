test_that("make export writes runnable make targets", {
  root <- mini_repo()
  path <- file.path(root, "Makefile.dina")
  dina_make_export(path, root)
  expect_true(file.exists(path))
  text <- readLines(path)
  expect_true(any(grepl("task1:", text, fixed = TRUE)))
  expect_true(any(grepl("dina run --task task1", text, fixed = TRUE)))
})
