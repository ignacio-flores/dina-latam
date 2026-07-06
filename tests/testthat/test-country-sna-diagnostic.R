source(file.path(repo_root_for_tests, "code", "R", "source-diagnostics", "country_sna_current_extractor_diagnostic.R"))

test_that("country SNA helpers parse Excel columns and normalize codes", {
  expect_equal(country_sna_excel_col_to_index(c("A", "Z", "AA", "BV")), c(1, 26, 27, 74))
  expect_equal(country_sna_range_col_position("K", "F49:T146"), 6)
  expect_equal(country_sna_normalize_code(c(" D4 ", "D.4", "B5b", "B.2b / B.3b")), c("D.4", "D.4", "B.5B", "B.2B/B.3B"))
})

test_that("country SNA sheet resolver accepts year sheet variants", {
  if (requireNamespace("openxlsx", quietly = TRUE)) {
    root <- tempfile("country-sna-sheet-")
    dir.create(root, recursive = TRUE)
    path <- file.path(root, "fixture.xlsx")
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "CEI_2024p")
    openxlsx::writeData(wb, "CEI_2024p", data.frame(x = 1))
    openxlsx::saveWorkbook(wb, path, overwrite = TRUE)

    resolved <- country_sna_resolve_sheet(path, 2024L, list(type = "year_variants"))
    expect_equal(resolved$sheet, "CEI_2024p")
    expect_equal(resolved$status, "matched")
  } else {
    expect_true(TRUE)
  }
})

test_that("country SNA comparison classifies anecdotal and substantive revisions", {
  value_rows <- function(values) {
    data.frame(
      country = "AAA",
      year = 2023L,
      variable = names(values),
      value_type = "primitive",
      value = as.numeric(values),
      value_raw = as.character(values),
      extract_status = "matched",
      source_file = "fixture.xlsx",
      sheet = "2023",
      cell_range = "A1:B3",
      row_in_range = seq_along(values),
      excel_row = seq_along(values),
      excel_col = "B",
      role = "households_r",
      matched_code = names(values),
      code_long = names(values),
      stringsAsFactors = FALSE
    )
  }
  old <- value_rows(c(a = 100, b = 100, c = 0))
  new <- value_rows(c(a = 100.5, b = 102, c = 1))

  detail <- country_sna_compare_values(old, new)
  expect_true("anecdotal_lt_1pct" %in% detail$change_magnitude)
  expect_true("substantive_ge_1pct" %in% detail$change_magnitude)
  expect_true("old_zero" %in% detail$status)

  summary <- country_sna_ratio_summary(detail)
  expect_equal(summary$anecdotal_changes, 1L)
  expect_equal(summary$substantive_changes, 1L)
})

test_that("country SNA extension logic keeps Mexico focused on current extractor years", {
  info <- country_sna_extension_info("MEX", 2003:2023, c(2003:2024, 2319), 2000:2023)
  expect_equal(info$beyond_active, 2024L)
  expect_false(2319L %in% info$new_years)
})

test_that("country SNA layout helper distinguishes year sheets from account sheets", {
  if (requireNamespace("openxlsx", quietly = TRUE)) {
    root <- tempfile("country-sna-layout-")
    dir.create(root, recursive = TRUE)
    old_path <- file.path(root, "old.xlsx")
    new_path <- file.path(root, "new.xlsx")

    old_wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(old_wb, "CEI_2023p")
    openxlsx::writeData(old_wb, "CEI_2023p", data.frame(x = 1))
    openxlsx::saveWorkbook(old_wb, old_path, overwrite = TRUE)

    new_wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(new_wb, "EBE")
    openxlsx::addWorksheet(new_wb, "YDB")
    openxlsx::writeData(new_wb, "EBE", data.frame(x = 1))
    openxlsx::writeData(new_wb, "YDB", data.frame(x = 1))
    openxlsx::saveWorkbook(new_wb, new_path, overwrite = TRUE)

    old_layout <- country_sna_layout_file(old_path)
    new_layout <- country_sna_layout_file(new_path)
    expect_equal(old_layout$year_sheet_count, 1L)
    expect_equal(new_layout$account_sheet_count, 2L)
  } else {
    expect_true(TRUE)
  }
})

test_that("country SNA stem resolver reports xls/xlsx ambiguity", {
  root <- tempfile("country-sna-stem-")
  dir.create(file.path(root, "input_data", "sna_country_data", "CHL"), recursive = TRUE)
  writeLines("old", file.path(root, "input_data", "sna_country_data", "CHL", "CEI_merged.xls"))
  writeLines("new", file.path(root, "input_data", "sna_country_data", "CHL", "CEI_merged.xlsx"))

  resolved <- country_sna_resolve_single_file(
    list(type = "single_stem", stem = "input_data/sna_country_data/CHL/CEI_merged"),
    root
  )
  expect_match(paste(resolved$warnings, collapse = "\n"), "Multiple candidate workbooks")
})

test_that("country SNA diagnostic can smoke-test real CHL files when present", {
  old_path <- file.path(repo_root_for_tests, "input_data", "sna_country_data", "CHL", "CEI_merged.xlsx")
  new_path <- file.path(repo_root_for_tests, "input_data", "_new", "country_sna", "CHL", "CEI_anuario_2013-2024.xls")
  if (file.exists(old_path) && file.exists(new_path)) {
    result <- country_sna_diagnose_country("CHL", country_sna_rules()$CHL, 2000:2023, repo_root_for_tests)
    expect_equal(result$country_summary$country, "CHL")
    expect_true(nrow(result$cell_detail) > 0)
  } else {
    expect_true(TRUE)
  }
})
