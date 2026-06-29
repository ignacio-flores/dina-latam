test_that("source scan detects years from filenames quickly", {
  root <- mini_repo()
  touch(file.path(root, "input_data", "source_2020_2022.xlsx"), "2024-01-01")
  scan <- dina_scan_sources(root)
  expect_equal(scan[["source-a"]]$detected_years, c(2020L, 2022L))
  expect_equal(scan[["source-missing"]]$detected_years, integer())
})

test_that("source diff classifies new years and missing files", {
  root <- mini_repo()
  touch(file.path(root, "input_data", "source_2024.xlsx"), "2024-01-01")
  current <- dina_scan_sources(root)
  previous <- list(`source-a` = list(files = list(), detected_years = c(2020L, 2021L)))
  diff <- dina_classify_source_changes(current, previous)
  expect_true("new_year" %in% diff[["source-a"]]$classes)
  expect_true("local_missing" %in% diff[["source-missing"]]$classes)
})
