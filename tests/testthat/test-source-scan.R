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

test_that("project source registry is a complete readable catalog", {
  registry <- dina_sources(repo_root_for_tests)$sources
  ids <- vapply(registry, function(source) source$id, character(1))

  required_fields <- c("id", "family", "country", "method")
  missing_fields <- character()
  missing_locators <- character()
  for (source in registry) {
    for (field in required_fields) {
      if (length(dina_source_values(dina_source_field(source, field))) == 0) {
        missing_fields <- c(missing_fields, paste(source$id %||% "<unknown>", field, sep = ":"))
      }
    }
    has_locator <- any(vapply(
      c("canonical", "url", "urls", "downloader", "notes"),
      function(field) length(dina_source_values(dina_source_field(source, field))) > 0,
      logical(1)
    ))
    if (!has_locator) {
      missing_locators <- c(missing_locators, source$id)
    }
  }
  expect_equal(missing_fields, character())
  expect_equal(missing_locators, character())

  expect_true(all(c(
    "country-sna-index",
    "wid-prices-xrates",
    "wb-xrates",
    "wb-inflation"
  ) %in% ids))

  moved_downloaders <- c(
    "code/R/manual-downloaders/01a_download-raw-un-sna.R",
    "code/R/manual-downloaders/01c_download_countrysna.R",
    "code/R/manual-downloaders/bra_admin_downloader.R",
    "code/R/manual-downloaders/bra_minwage_downloader.R"
  )
  expect_true(all(file.exists(file.path(repo_root_for_tests, moved_downloaders))))

  downloader_paths <- unlist(lapply(registry, function(source) {
    dina_source_values(dina_source_field(source, "downloader"))
  }), use.names = FALSE)
  downloader_paths <- downloader_paths[nzchar(downloader_paths)]
  expect_true(all(file.exists(file.path(repo_root_for_tests, downloader_paths))))
  expect_false(any(grepl("code/R/functions/bra_.*downloader|code/R/01a_download|code/R/01c_download", downloader_paths)))
})
