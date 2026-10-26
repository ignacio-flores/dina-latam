source(file.path(repo_root_for_tests, "code/R/01b_prepare_annual_sna.R"))

test_that("country-SNA YAML remains complete under Stata's C locale", {
  engine <- new.env(parent = globalenv())
  sys.source(file.path(repo_root_for_tests, "code/R/source-diagnostics/country_sna_include.R"), engine)
  path <- tempfile(fileext = ".yml")
  on.exit(unlink(path))
  writeLines(c("# Cuentas econ\u00f3micas \u2014 accepted source", "variables: [D1_cei]",
    "country_rules: {AAA: {magnitude: 1000000}}"), path, useBytes = TRUE)
  result <- withr::with_locale(c(LC_CTYPE = "C"), engine$country_sna_include_read_yaml(path))
  expect_equal(result$variables, "D1_cei")
  expect_equal(result$country_rules$AAA$magnitude, 1000000)
})

annual_sna_fixture <- function() {
  root <- tempfile("annual-sna-")
  dir.create(file.path(root, "code/R/source-diagnostics"), recursive = TRUE)
  file.copy(file.path(repo_root_for_tests, "code/R/source-diagnostics/country_sna_include.R"),
    file.path(root, "code/R/source-diagnostics/country_sna_include.R"))
  dir.create(file.path(root, "config"))
  dir.create(file.path(root, "inputs"))
  contract <- list(output_root = "unused", years = list(first = 2020, last = 2022),
    countries = "AAA", thresholds = list(zero_as_missing = FALSE),
    code_aliases = list("D.1" = "D.1", "B.5" = "B.5"),
    variables = list(primitives = list(
      list(name = "D1_cei", account = "D.1", role = "households_r"),
      list(name = "B5g_cei", account = "B.5", role = "households_r"))),
    country_rules = list(AAA = list(adapter = "workbook_table", magnitude = 1000000,
      source = list(type = "year_pattern", pattern = "inputs/cei_{year}.xlsx"),
      layouts = list(list(years = list(min = 2000), range = "A21:C23",
        sheet = list(type = "year_variants"),
        columns = list(code = "A", code_long = "B", households_r = "C"))))))
  yaml::write_yaml(contract, file.path(root, "config/country_sna_include.yml"))
  root
}

annual_sna_year <- function(root, year, sheet = paste0("CEI_", year), conflict = FALSE) {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, sheet)
  x <- data.frame(code = c("D.1", "B.5", if (conflict) "D.1" else ""),
    label = c("Wages", "Primary income", ""), value = c(100, 150, if (conflict) 101 else NA))
  openxlsx::writeData(wb, sheet, x, startRow = 21, colNames = FALSE, keepNA = FALSE)
  openxlsx::saveWorkbook(wb, file.path(root, "inputs", paste0("cei_", year, ".xlsx")))
}

test_that("annual CEI assembly preserves source cells and standardized values", {
  root <- annual_sna_fixture()
  on.exit(unlink(root, recursive = TRUE))
  annual_sna_year(root, 2020)
  annual_sna_year(root, 2021, sheet = "2021")
  source <- file.path(root, "inputs/cei_2020.xlsx")
  original <- readBin(source, "raw", n = file.info(source)$size)
  output <- file.path(root, "derived/cei.xlsx")
  result <- dina_prepare_annual_sna("AAA", 2000, 2022, output, root)
  expect_identical(result$years, 2020:2021)
  expect_identical(readxl::excel_sheets(output), c("2020", "2021"))
  expect_equal(result$values$value_standardized, c(1e8, 1.5e8, 1e8, 1.5e8))
  expect_identical(readBin(source, "raw", n = file.info(source)$size), original)
  expect_equal(readxl::read_excel(output, sheet = "2020", range = "A21:C23", col_names = FALSE),
    readxl::read_excel(source, sheet = "CEI_2020", range = "A21:C23", col_names = FALSE))
})

test_that("failed annual CEI preparation retains the previous derived workbook", {
  root <- annual_sna_fixture()
  on.exit(unlink(root, recursive = TRUE))
  annual_sna_year(root, 2020)
  output <- file.path(root, "derived/cei.xlsx")
  dina_prepare_annual_sna("AAA", 2020, 2022, output, root)
  original <- readBin(output, "raw", n = file.info(output)$size)
  annual_sna_year(root, 2022)
  expect_error(dina_prepare_annual_sna("AAA", 2020, 2022, output, root), "inside coverage: 2021")
  annual_sna_year(root, 2021, sheet = "wrong")
  expect_error(dina_prepare_annual_sna("AAA", 2020, 2022, output, root), "Missing CEI sheet")
  unlink(file.path(root, "inputs/cei_2021.xlsx"))
  annual_sna_year(root, 2021, conflict = TRUE)
  expect_error(dina_prepare_annual_sna("AAA", 2020, 2022, output, root), "Invalid accepted CEI")
  expect_identical(readBin(output, "raw", n = file.info(output)$size), original)
})
