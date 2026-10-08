source(file.path(repo_root_for_tests, "code", "R", "admin_cleaners", "gpinter_admin.R"))

test_that("local gpinter writes a BFM-compatible workbook and records exclusions", {
  skip_if_not_installed("gpinter")
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("readxl")
  root <- mini_repo()
  method <- file.path(root, "config", "admin_gpinter.yml")
  dina_write_yaml(list(version = 1L, excluded_countries = "ECU"), method)
  clean <- file.path(root, "input_data", "admin_data", "PER", "_clean", "total-PER.xlsx")
  dir.create(dirname(clean), recursive = TRUE)
  sample <- data.frame(
    year = c(2018L, rep(NA_integer_, 3L)),
    country = c("PER", rep(NA_character_, 3L)),
    average = c(2.25, rep(NA_real_, 3L)),
    p = c(0, 0.5, 0.9, 0.99),
    thr = c(0, 1, 5, 20),
    bracketavg = c(0.5, 2, 10, 30)
  )
  openxlsx::write.xlsx(list(`2018` = sample), clean)

  dina_admin_gpinter_run(root, config = list(countries = c("PER", "ECU"), years = list(first = 2000L, last = 2024L)))
  generated <- file.path(root, "input_data", "admin_data", "PER", "gpinter_output", "total-pos-PER.xlsx")
  expect_equal(readxl::excel_sheets(generated), c("series", "PER, 2018"))
  table <- readxl::read_excel(generated, sheet = "PER, 2018")
  expect_equal(nrow(table), 127L)
  expect_true(all(c("p", "thr", "bracketavg", "topavg") %in% names(table)))
  expect_equal(table$average[[1L]], 2.25)
  expect_false(dir.exists(file.path(root, "input_data", "admin_data", "ECU", "gpinter_output")))
  countries <- utils::read.csv(file.path(root, "output", "data_reports", "admin_gpinter_countries.csv"))
  expect_equal(countries$status, c("generated", "excluded"))
})

test_that("BRA placeholder average comes from its current ptot source", {
  skip_if_not_installed("gpinter")
  skip_if_not_installed("openxlsx")
  root <- mini_repo()
  country_dir <- file.path(root, "input_data", "admin_data", "BRA")
  clean <- file.path(country_dir, "_clean", "total-pre-BRA.xlsx")
  dir.create(dirname(clean), recursive = TRUE)
  input <- data.frame(average = c(0, NA, NA, NA), p = c(0, 0.5, 0.9, 0.99),
                      thr = c(0, 1, 5, 20), bracketavg = c(0.5, 2, 10, 30))
  openxlsx::write.xlsx(data.frame(average = 2.25), file.path(country_dir, "ptot_2000.xlsx"))
  fit <- dina_admin_gpinter_fit(input, "BRA", "2000", clean)
  expect_equal(fit$average, 2.25)
  openxlsx::write.xlsx(list(`2000` = input), clean)
  dina_write_yaml(list(version = 1L), file.path(root, "config", "admin_gpinter.yml"))
  dina_admin_gpinter_run(root, config = list(countries = "BRA", years = list(first = 2000L, last = 2000L)))
  manifest <- jsonlite::read_json(file.path(root, "output", "data_reports", "admin_gpinter_manifest.json"))
  expect_equal(manifest$workbooks[[1L]]$auxiliary_sources[[1L]]$path,
               "input_data/admin_data/BRA/ptot_2000.xlsx")
})
