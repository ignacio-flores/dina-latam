Sys.unsetenv("LC_ALL")

dina_compress_state_condition <- get("testthat_state_condition", asNamespace("testthat"))
assignInNamespace("testthat_state_condition", function(before, after, call = NULL) NULL, ns = "testthat")
if (requireNamespace("withr", quietly = TRUE)) {
  withr::defer(
    assignInNamespace("testthat_state_condition", dina_compress_state_condition, ns = "testthat"),
    envir = testthat:::teardown_env()
  )
}

expect_true <- function(object, info = NULL, label = NULL) {
  value <- force(object)
  if (!isTRUE(value)) testthat::fail(info %||% "Expected TRUE.")
  invisible(value)
}

expect_false <- function(object, info = NULL, label = NULL) {
  value <- force(object)
  if (!identical(value, FALSE)) testthat::fail(info %||% "Expected FALSE.")
  invisible(value)
}

test_that("compress input planning excludes admin microdata by default", {
  root <- mini_repo()
  dir.create(file.path(root, "input_data", "admin_data", "MEX"), recursive = TRUE)
  dir.create(file.path(root, "input_data", "admin_data", "URY"), recursive = TRUE)
  dir.create(file.path(root, "input_data", "admin_data", "CHL"), recursive = TRUE)

  plan <- dina_compress_input_plan(root = root)
  expect_equal(plan$included_root, "input_data")
  expect_true("admin-microdata" %in% plan$excluded_types)
  expect_true(all(c("input_data/admin_data/MEX", "input_data/admin_data/URY") %in% plan$excluded_paths$path))

  all_plan <- dina_compress_input_plan(root = root, all = TRUE)
  expect_equal(all_plan$excluded_types, character())
  expect_equal(nrow(all_plan$excluded_paths), 0L)

  include_plan <- dina_compress_input_plan(root = root, include = "admin-microdata")
  expect_equal(include_plan$excluded_types, character())

  explicit_plan <- dina_compress_input_plan(root = root, all = TRUE, exclude = "admin-microdata")
  expect_true("admin-microdata" %in% explicit_plan$excluded_types)
})

test_that("compress input can plan against the Dropbox mirror", {
  root <- mini_repo()
  dropbox <- tempfile("dina-dropbox-")
  dir.create(file.path(dropbox, "input_data"), recursive = TRUE)
  writeLines("x", file.path(dropbox, "input_data", "x.txt"))

  withr::with_envvar(c(DINA_DROPBOX_ROOT = dropbox), {
    plan <- dina_compress_input_plan(root = root, dropbox = TRUE)
    expect_equal(plan$source_root, normalizePath(dropbox, mustWork = FALSE))
    expect_match(plan$output, "output/archives/input-data-")
    expect_true(startsWith(plan$output, normalizePath(dropbox, mustWork = FALSE)))
  })
})

test_that("compress input CLI previews and writes zip archives", {
  root <- mini_repo()
  writeLines("ordinary", file.path(root, "input_data", "ordinary.txt"))
  dir.create(file.path(root, "input_data", "admin_data", "MEX"), recursive = TRUE)
  dir.create(file.path(root, "input_data", "admin_data", "URY"), recursive = TRUE)
  writeLines("mex", file.path(root, "input_data", "admin_data", "MEX", "secret.txt"))
  writeLines("ury", file.path(root, "input_data", "admin_data", "URY", "secret.txt"))

  preview <- run_dina_cli(c("compress", "input", "--dry-run"), root = root)
  expect_equal(preview$status, 0L)
  expect_match(preview$output, "Compress Input Preview")
  expect_match(preview$output, "admin-microdata")
  expect_match(preview$output, "input_data/admin_data/MEX")
  expect_match(preview$output, "Dry-run only")

  zip_path <- file.path(root, "output", "archives", "custom-input.zip")
  written <- run_dina_cli(c("compress", "input", "--output", zip_path), root = root)
  expect_equal(written$status, 0L)
  expect_true(file.exists(zip_path))
  files <- utils::unzip(zip_path, list = TRUE)$Name
  expect_true("input_data/ordinary.txt" %in% files)
  expect_false(any(grepl("input_data/admin_data/MEX", files, fixed = TRUE)))
  expect_false(any(grepl("input_data/admin_data/URY", files, fixed = TRUE)))
})

test_that("admin microdata is a distinct public source type", {
  root <- mini_repo()
  dina_write_yaml(list(sources = list(
    list(id = "admin-tax", family = "admin_tax", country = "CHL", method = "manual", integration = "none", notes = "PIT"),
    list(id = "mex-admin-microdata", family = "admin_microdata", country = "MEX", method = "manual", canonical = "input_data/admin_data/MEX", destination = "input_data/admin_data/MEX", transformer = "code/Stata/02b-prepare-admin-microdata.do", notes = "MEX microdata"),
    list(id = "ury-admin-microdata", family = "admin_microdata", country = "URY", method = "manual", canonical = "input_data/admin_data/URY", destination = "input_data/admin_data/URY", transformer = "code/Stata/02b-prepare-admin-microdata.do", notes = "URY microdata")
  )), file.path(root, "config", "sources.yml"))

  micro <- run_dina_cli(c("sources", "list", "admin-microdata", "--no-menu"), root = root)
  expect_equal(micro$status, 0L)
  expect_match(micro$output, "mex-admin-microdata")
  expect_match(micro$output, "ury-admin-microdata")
  expect_false(grepl("admin-tax", micro$output, fixed = TRUE))

  admin <- run_dina_cli(c("sources", "list", "admin", "--no-menu"), root = root)
  expect_equal(admin$status, 0L)
  expect_match(admin$output, "admin-tax")
  expect_false(grepl("mex-admin-microdata", admin$output, fixed = TRUE))
})
