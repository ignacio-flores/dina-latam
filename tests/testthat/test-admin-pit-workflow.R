source(file.path(repo_root_for_tests, "code", "R", "source-diagnostics", "admin_pit_explorer.R"))
source(file.path(repo_root_for_tests, "code", "R", "source-diagnostics", "admin_pit_include.R"))

Sys.unsetenv("LC_ALL")
admin_pit_state_condition <- get("testthat_state_condition", asNamespace("testthat"))
assignInNamespace("testthat_state_condition", function(before, after, call = NULL) NULL, ns = "testthat")
if (requireNamespace("withr", quietly = TRUE)) {
  withr::defer(
    assignInNamespace("testthat_state_condition", admin_pit_state_condition, ns = "testthat"),
    envir = testthat:::teardown_env()
  )
}

admin_pit_write_workbook <- function(path, sheet, marker = "ok", years = NULL) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, sheet)
  openxlsx::writeData(wb, sheet, data.frame(marker = marker), colNames = FALSE)
  if (!is.null(years)) {
    openxlsx::writeData(wb, sheet, data.frame(year = as.integer(years)), startRow = 8, colNames = TRUE)
  }
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
}

admin_pit_expect_true <- function(value, info = "expected TRUE") {
  if (!isTRUE(value)) testthat::fail(info)
  testthat::succeed()
}

admin_pit_expect_false <- function(value, info = "expected FALSE") {
  if (!identical(value, FALSE)) testthat::fail(info)
  testthat::succeed()
}

admin_pit_col_files <- function(path, years, nested = FALSE, omit = integer()) {
  folder <- if (isTRUE(nested)) file.path(path, basename(path)) else path
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  for (year in setdiff(years, omit)) {
    index <- year - 2007L
    writeLines("fixture", file.path(folder, sprintf("%s_Cuantiles_Ingreso_Bruto_Naturales_%s_F-210.xlsx", index, year)))
  }
}

admin_pit_fixture_repo <- function(block_col = FALSE, backup_overlap = FALSE) {
  root <- tempfile("admin-pit-fixture-")
  dir.create(root, recursive = TRUE)
  if (requireNamespace("withr", quietly = TRUE)) {
    withr::defer(unlink(root, recursive = TRUE), envir = parent.frame())
  }
  dir.create(file.path(root, "config"), recursive = TRUE)
  dir.create(file.path(root, "code", "R", "source-diagnostics"), recursive = TRUE)
  dir.create(file.path(root, "code", "R", "admin_cleaners"), recursive = TRUE)
  dir.create(file.path(root, "code", "Stata", "tax-data"), recursive = TRUE)
  file.copy(
    file.path(repo_root_for_tests, "code", "R", "source-diagnostics", c("admin_pit_explorer.R", "admin_pit_include.R", "admin_pit_cli.R")),
    file.path(root, "code", "R", "source-diagnostics"),
    overwrite = TRUE
  )
  file.copy(
    file.path(repo_root_for_tests, "code", "R", "admin_cleaners", "admin_pit_candidate_cleaners.R"),
    file.path(root, "code", "R", "admin_cleaners"),
    overwrite = TRUE
  )
  file.copy(
    file.path(repo_root_for_tests, "config", c("admin_pit_explorer.yml", "admin_pit_include.yml")),
    file.path(root, "config"),
    overwrite = TRUE
  )
  writeLines("legacy chl cleaner", file.path(root, "code", "R", "02c_clean_admin_chl.R"))
  writeLines("legacy bra cleaner", file.path(root, "code", "R", "02c_clean_admin_bra.R"))
  writeLines("legacy col do", file.path(root, "code", "Stata", "tax-data", "COL-diverse.do"))

  dina_write_yaml(list(countries = c("CHL", "BRA", "COL"), years = list(first = 2000L, last = 2023L)), file.path(root, "config", "dina.yml"))
  dir.create(file.path(root, "output", "updates", "fixture-update"), recursive = TRUE)
  writeLines("fixture-update", file.path(root, "output", "updates", ".active_update"))
  dina_write_yaml(list(years = list(last = 2024L)), file.path(root, "output", "updates", "fixture-update", "config.override.yml"))

  dina_write_yaml(list(sources = list(
    list(id = "chl-pit-total", family = "admin_tax", country = "CHL", method = "manual", canonical = "input_data/admin_data/CHL/PUB_Total_*.xlsx", inbox = "input_data/_new/admin_tax/PUB_Total_*.xlsx", destination = "input_data/admin_data/CHL/{basename}", notes = "Chile PIT fixture."),
    list(id = "bra-pit-total", family = "admin_tax", country = "BRA", method = "manual", canonical = "input_data/admin_data/BRA/gn-irpf-ac*.xlsx", inbox = "input_data/_new/admin_tax/gn-irpf-ac*.xlsx", destination = "input_data/admin_data/BRA/{basename}", notes = "Brazil PIT fixture."),
    list(id = "col-pit-total", family = "admin_tax", country = "COL", method = "manual", canonical = "input_data/admin_data/COL/1_Cuantiles_Ingreso_Bruto_Naturales_2014-*", inbox = "input_data/_new/admin_tax/1_Cuantiles_Ingreso_Bruto_Naturales_2014-*", destination = "input_data/admin_data/COL/{basename}", notes = "Colombia PIT fixture."),
    list(id = "bra-minwage", family = "admin_tax_aux", country = "BRA", method = "manual", canonical = "input_data/admin_data/BRA/downloads/wiki_minwage.csv", inbox = "input_data/_new/admin_tax/wiki_minwage.csv", destination = "input_data/admin_data/BRA/downloads/{basename}", notes = "Brazil min wage fixture."),
    list(id = "chl-uta", family = "admin_tax_aux", country = "CHL", method = "manual", canonical = "input_data/admin_data/CHL/uta_december.csv", inbox = "input_data/_new/admin_tax/chl_uta_december.csv", destination = "input_data/admin_data/CHL/uta_december.csv", notes = "Chile UTA fixture."),
    list(id = "mex-admin-microdata", family = "admin_microdata", country = "MEX", method = "manual", canonical = "input_data/admin_data/MEX", notes = "Unsupported admin microdata fixture.")
  )), file.path(root, "config", "sources.yml"))

  admin_pit_write_workbook(file.path(root, "input_data", "admin_data", "CHL", "PUB_Total_2022.xlsx"), "Datos", "old-2022", years = 2005:2022)
  if (isTRUE(backup_overlap)) {
    admin_pit_write_workbook(file.path(root, "input_data", "admin_data", "CHL", "PUB_Total_2024.xlsx"), "Datos", "old-2024", years = 2005:2024)
  }
  admin_pit_write_workbook(file.path(root, "input_data", "_new", "admin_tax", "PUB_Total_2024.xlsx"), "Datos", "new-2024", years = 2005:2024)
  admin_pit_write_workbook(file.path(root, "input_data", "admin_data", "BRA", "gn-irpf-ac2023.xlsx"), "Tab8", "old-2023")
  admin_pit_write_workbook(file.path(root, "input_data", "_new", "admin_tax", "gn-irpf-ac2024.xlsx"), "Tab8", "new-2024")
  writeLines("year,minwage\n2024,1412", file.path(root, "input_data", "_new", "admin_tax", "wiki_minwage.csv"))
  writeLines("year,uta\n2024,75981", file.path(root, "input_data", "_new", "admin_tax", "chl_uta_december.csv"))
  for (file in c("tab_gc_1991_2000.xls", "tab_gc_1963_1981.xlsx", "tab_gc_1998_2009.xlsx", "tab_gc_wage_1998_2009.xlsx", "Data_1998-2009sinKG.xlsx")) {
    writeLines("static fixture", file.path(root, "input_data", "admin_data", "CHL", file))
  }
  admin_pit_write_workbook(file.path(root, "input_data", "admin_data", "BRA", "ptot_2000.xlsx"), "Sheet1", "static-2000")
  old_col <- file.path(root, "input_data", "admin_data", "COL", "1_Cuantiles_Ingreso_Bruto_Naturales_2014-2022")
  new_col <- file.path(root, "input_data", "_new", "admin_tax", "1_Cuantiles_Ingreso_Bruto_Naturales_2014-2023")
  admin_pit_col_files(old_col, 2014:2022)
  admin_pit_col_files(new_col, 2014:2023, nested = TRUE, omit = if (isTRUE(block_col)) 2023L else integer())
  root
}

admin_pit_hashes <- function(root) {
  paths <- c(
    file.path(root, "code", "R", "02c_clean_admin_chl.R"),
    file.path(root, "code", "R", "02c_clean_admin_bra.R"),
    file.path(root, "code", "Stata", "tax-data", "COL-diverse.do")
  )
  stats::setNames(vapply(paths, digest::digest, character(1), file = TRUE, algo = "sha256"), basename(paths))
}

test_that("isolated PIT admin explorer detects supported sources, years, unsupported rows, and active override", {
  skip_if_not_installed("openxlsx")
  root <- admin_pit_fixture_repo()
  result <- run_admin_pit_explorer(root = root, write_outputs = FALSE)

  expect_equal(admin_pit_explorer_supported_ids(result$contract), c("chl-pit-total", "bra-pit-total", "col-pit-total"))
  expect_equal(result$manifest$value[result$manifest$key == "year_last"], "2024")
  admin_pit_expect_true(all(result$outputs$structure_summary$structure_status == "structure_evidence_available"))
  admin_pit_expect_true(any(result$outputs$extension_summary$source_id == "chl-pit-total" & result$outputs$extension_summary$old_years == paste(2005:2022, collapse = ",")))
  admin_pit_expect_true(any(result$outputs$extension_summary$source_id == "chl-pit-total" & result$outputs$extension_summary$extension_years == "2023,2024"))
  admin_pit_expect_true(any(result$outputs$extension_summary$source_id == "col-pit-total" & result$outputs$extension_summary$extension_years == "2023"))
  admin_pit_expect_true(all(c("bra-minwage", "mex-admin-microdata") %in% result$outputs$unsupported_sources$source_id))
})

test_that("isolated PIT admin explorer blocks missing expected Colombia files", {
  skip_if_not_installed("openxlsx")
  root <- admin_pit_fixture_repo(block_col = TRUE)
  result <- run_admin_pit_explorer(root = root, write_outputs = FALSE)
  col <- result$outputs$structure_summary[result$outputs$structure_summary$source_id == "col-pit-total", , drop = FALSE]
  expect_equal(col$structure_status, "blocked_structure_mismatch")
  expect_match(col$structure_evidence, "missing_expected_files")
})

test_that("isolated PIT admin include stages, confirms, backs up, restores, and leaves legacy files untouched", {
  skip_if_not_installed("digest")
  skip_if_not_installed("openxlsx")
  root <- admin_pit_fixture_repo(backup_overlap = TRUE)
  before <- admin_pit_hashes(root)
  explore <- run_admin_pit_explorer(root = root, write_outputs = TRUE)
  include <- run_admin_pit_include(root = root, exploration_run = explore$paths$root, write_outputs = TRUE, cleaner_mode = "mock")
  expect_equal(include$manifest$value[include$manifest$key == "status"], "all_good")
  admin_pit_expect_true(all(include$outputs$include_summary$status == "all_good"))
  admin_pit_expect_true(file.exists(file.path(include$paths$staged_repo, "input_data", "admin_data", "CHL", "_clean", "total-pre-CHL.xlsx")))
  admin_pit_expect_true(file.exists(file.path(include$paths$staged_repo, "input_data", "admin_data", "BRA", "_clean", "total-pre-BRA.xlsx")))

  confirm <- admin_pit_include_confirm_sources(root = root, include_run = include$paths$root)
  admin_pit_expect_true(file.exists(file.path(root, "input_data", "admin_data", "BRA", "gn-irpf-ac2024.xlsx")))
  admin_pit_expect_true(file.exists(file.path(root, "input_data", "admin_data", "COL", "1_Cuantiles_Ingreso_Bruto_Naturales_2014-2023")))
  admin_pit_expect_true(file.exists(file.path(root, "input_data", "admin_data", "CHL", "_clean", "total-pre-CHL.xlsx")))
  admin_pit_expect_true(all(c("raw_source", "clean_output", "aux_source") %in% confirm$outputs$promote_report$artifact_type))
  chl_2024 <- file.path(root, "input_data", "admin_data", "CHL", "PUB_Total_2024.xlsx")
  expect_equal(openxlsx::read.xlsx(chl_2024, sheet = "Datos", colNames = FALSE)[1, 1], "new-2024")

  restore <- admin_pit_include_restore_sources(root = root, confirm_run = confirm$paths$root)
  expect_equal(openxlsx::read.xlsx(chl_2024, sheet = "Datos", colNames = FALSE)[1, 1], "old-2024")
  admin_pit_expect_false(file.exists(file.path(root, "input_data", "admin_data", "BRA", "gn-irpf-ac2024.xlsx")))
  admin_pit_expect_false(file.exists(file.path(root, "input_data", "admin_data", "COL", "1_Cuantiles_Ingreso_Bruto_Naturales_2014-2023")))
  admin_pit_expect_false(file.exists(file.path(root, "input_data", "admin_data", "CHL", "_clean", "total-pre-CHL.xlsx")))
  admin_pit_expect_true(any(restore$outputs$restore_report$restore_status == "removed_promoted_destination"))
  expect_equal(admin_pit_hashes(root), before)
})

test_that("isolated PIT admin confirm refuses changed incoming source fingerprints", {
  skip_if_not_installed("digest")
  skip_if_not_installed("openxlsx")
  root <- admin_pit_fixture_repo()
  explore <- run_admin_pit_explorer(root = root, write_outputs = TRUE)
  include <- run_admin_pit_include(root = root, exploration_run = explore$paths$root, write_outputs = TRUE, cleaner_mode = "mock")

  admin_pit_write_workbook(file.path(root, "input_data", "_new", "admin_tax", "PUB_Total_2024.xlsx"), "Datos", "changed-after-dry-run")

  expect_error(admin_pit_include_confirm_sources(root = root, include_run = include$paths$root), "fingerprints changed since dry-run")
  admin_pit_expect_false(file.exists(file.path(root, "input_data", "admin_data", "BRA", "gn-irpf-ac2024.xlsx")))
})

test_that("isolated COL cleaner uses a temporary patched do-file only", {
  skip_if_not_installed("digest")
  original <- file.path(repo_root_for_tests, "code", "Stata", "tax-data", "COL-diverse.do")
  before <- digest::digest(file = original, algo = "sha256")
  paths <- list(logs = tempfile("admin-pit-col-logs-"))
  dir.create(paths$logs, recursive = TRUE)
  do_file <- admin_pit_include_col_temp_do(
    repo_root_for_tests,
    paths,
    source_dir = "/tmp/admin-pit-col-source",
    output_dir = "/tmp/admin-pit-col-output",
    first_year = 2015L,
    last_year = 2023L
  )
  lines <- readLines(do_file, warn = FALSE)
  admin_pit_expect_true(any(grepl('global route "/tmp/admin-pit-col-source"', lines, fixed = TRUE)))
  admin_pit_expect_true(any(grepl('forvalues y = 2015/`lasty_col_tax', lines, fixed = TRUE)))
  admin_pit_expect_true(any(grepl('"/tmp/admin-pit-col-output/total-`v\'-COL.xlsx"', lines, fixed = TRUE)))
  expect_equal(digest::digest(file = original, algo = "sha256"), before)
})

test_that("standalone isolated admin PIT entrypoint runs without touching the mainstream CLI", {
  skip_if_not_installed("openxlsx")
  root <- admin_pit_fixture_repo()
  cli <- file.path(root, "code", "R", "source-diagnostics", "admin_pit_cli.R")
  output <- system2(file.path(R.home("bin"), "Rscript"), c(cli, "explore", "--country", "CHL"), stdout = TRUE, stderr = TRUE, env = sprintf("DINA_REPO_ROOT=%s", root))
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  expect_equal(status, 0L)
  expect_match(paste(output, collapse = "\n"), "PIT Admin Explore")
})
