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

admin_pit_write_minwage <- function(path, years, changed_year = NULL) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  values <- years * 10
  if (!is.null(changed_year)) {
    values[years %in% changed_year] <- values[years %in% changed_year] + 1
  }
  utils::write.csv(data.frame(year = as.integer(years), minwage = as.numeric(values)), path, row.names = FALSE)
}

admin_pit_fixture_repo <- function(block_col = FALSE, backup_overlap = FALSE, minwage = c("new", "canonical", "missing", "changed_overlap", "missing_history", "missing_required", "ambiguous")) {
  minwage <- match.arg(minwage)
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
    list(id = "chl-pit-total", family = "admin_tax", country = "CHL", method = "manual", canonical = "input_data/admin_data/CHL/PUB_Total_*.xlsx", inbox = "input_data/_new/admin/CHL/PUB_Total_*.xlsx", destination = "input_data/admin_data/CHL/{basename}", notes = "Chile PIT fixture."),
    list(id = "bra-pit-total", family = "admin_tax", country = "BRA", method = "manual", canonical = "input_data/admin_data/BRA/gn-irpf-ac*.xlsx", inbox = "input_data/_new/admin/BRA/gn-irpf-ac*.xlsx", destination = "input_data/admin_data/BRA/{basename}", notes = "Brazil PIT fixture."),
    list(id = "col-pit-total", family = "admin_tax", country = "COL", method = "manual", canonical = "input_data/admin_data/COL/1_Cuantiles_Ingreso_Bruto_Naturales_2014-*", inbox = "input_data/_new/admin/COL/1_Cuantiles_Ingreso_Bruto_Naturales_2014-*", destination = "input_data/admin_data/COL/{basename}", notes = "Colombia PIT fixture."),
    list(id = "bra-minwage", family = "admin_tax_aux", country = "BRA", method = "manual", canonical = "input_data/admin_data/BRA/downloads/wiki_minwage.csv", inbox = "input_data/_new/admin/BRA/wiki_minwage*.csv", destination = "input_data/admin_data/BRA/downloads/{basename}", notes = "Brazil min wage fixture."),
    list(id = "chl-uta", family = "admin_tax_aux", country = "CHL", method = "manual", canonical = "input_data/admin_data/CHL/uta_december.csv", inbox = "input_data/_new/admin/CHL/chl_uta_december.csv", destination = "input_data/admin_data/CHL/uta_december.csv", notes = "Chile UTA fixture."),
    list(id = "mex-admin-microdata", family = "admin_microdata", country = "MEX", method = "manual", canonical = "input_data/admin_data/MEX", notes = "Unsupported admin microdata fixture.")
  )), file.path(root, "config", "sources.yml"))

  admin_pit_write_workbook(file.path(root, "input_data", "admin_data", "CHL", "PUB_Total_2022.xlsx"), "Datos", "old-2022", years = 2005:2022)
  if (isTRUE(backup_overlap)) {
    admin_pit_write_workbook(file.path(root, "input_data", "admin_data", "CHL", "PUB_Total_2024.xlsx"), "Datos", "old-2024", years = 2005:2024)
  }
  admin_pit_write_workbook(file.path(root, "input_data", "_new", "admin", "CHL", "PUB_Total_2024.xlsx"), "Datos", "new-2024", years = 2005:2024)
  admin_pit_write_workbook(file.path(root, "input_data", "admin_data", "BRA", "gn-irpf-ac2023.xlsx"), "Tab8", "old-2023")
  admin_pit_write_workbook(file.path(root, "input_data", "_new", "admin", "BRA", "gn-irpf-ac2024.xlsx"), "Tab8", "new-2024")
  if (identical(minwage, "new")) {
    admin_pit_write_minwage(file.path(root, "input_data", "admin_data", "BRA", "downloads", "wiki_minwage.csv"), 2007:2023)
    admin_pit_write_minwage(file.path(root, "input_data", "_new", "admin", "BRA", "wiki_minwage.csv"), 2007:2024)
  } else if (identical(minwage, "canonical")) {
    admin_pit_write_minwage(file.path(root, "input_data", "admin_data", "BRA", "downloads", "wiki_minwage.csv"), 2007:2024)
  } else if (identical(minwage, "changed_overlap")) {
    admin_pit_write_minwage(file.path(root, "input_data", "admin_data", "BRA", "downloads", "wiki_minwage.csv"), 2007:2023)
    admin_pit_write_minwage(file.path(root, "input_data", "_new", "admin", "BRA", "wiki_minwage.csv"), 2007:2024, changed_year = 2023L)
  } else if (identical(minwage, "missing_history")) {
    admin_pit_write_minwage(file.path(root, "input_data", "admin_data", "BRA", "downloads", "wiki_minwage.csv"), 2000:2023)
    admin_pit_write_minwage(file.path(root, "input_data", "_new", "admin", "BRA", "wiki_minwage.csv"), 2007:2024)
  } else if (identical(minwage, "missing_required")) {
    admin_pit_write_minwage(file.path(root, "input_data", "admin_data", "BRA", "downloads", "wiki_minwage.csv"), 2007:2023)
    admin_pit_write_minwage(file.path(root, "input_data", "_new", "admin", "BRA", "wiki_minwage.csv"), 2007:2023)
  } else if (identical(minwage, "ambiguous")) {
    admin_pit_write_minwage(file.path(root, "input_data", "admin_data", "BRA", "downloads", "wiki_minwage.csv"), 2007:2023)
    admin_pit_write_minwage(file.path(root, "input_data", "_new", "admin", "BRA", "wiki_minwage.csv"), 2007:2024)
    admin_pit_write_minwage(file.path(root, "input_data", "_new", "admin", "BRA", "wiki_minwage_alt.csv"), 2007:2024)
  }
  writeLines("year,uta\n2024,75981", file.path(root, "input_data", "_new", "admin", "CHL", "chl_uta_december.csv"))
  dir.create(file.path(root, "intermediary_data", "population"), recursive = TRUE)
  dir.create(file.path(root, "input_data", "population"), recursive = TRUE)
  writeLines("latam-pop-fixture", file.path(root, "input_data", "population", "PopulationLatAm.dta"))
  writeLines("survey-pop-fixture", file.path(root, "intermediary_data", "population", "SurveyPop.dta"))
  for (file in c("tab_gc_1991_2000.xls", "tab_gc_1963_1981.xlsx", "tab_gc_1998_2009.xlsx", "tab_gc_wage_1998_2009.xlsx", "Data_1998-2009sinKG.xlsx")) {
    writeLines("static fixture", file.path(root, "input_data", "admin_data", "CHL", file))
  }
  admin_pit_write_workbook(file.path(root, "input_data", "admin_data", "BRA", "ptot_2000.xlsx"), "Sheet1", "static-2000")
  old_col <- file.path(root, "input_data", "admin_data", "COL", "1_Cuantiles_Ingreso_Bruto_Naturales_2014-2022")
  new_col <- file.path(root, "input_data", "_new", "admin", "COL", "1_Cuantiles_Ingreso_Bruto_Naturales_2014-2023")
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

test_that("admin source registry uses public admin buckets only", {
  skip_if_not_installed("yaml")
  registry_text <- paste(readLines(file.path(repo_root_for_tests, "config", "sources.yml"), warn = FALSE), collapse = "\n")
  admin_pit_expect_false(grepl("input_data/_new/admin_tax", registry_text, fixed = TRUE))
  admin_pit_expect_false(grepl("input_data/_new/admin_tax_aux", registry_text, fixed = TRUE))
  admin_pit_expect_true(grepl("input_data/_new/admin/CHL/PUB_Total_", registry_text, fixed = TRUE))
  admin_pit_expect_true(grepl("input_data/_new/admin/BRA/gn-irpf-ac", registry_text, fixed = TRUE))
  admin_pit_expect_true(grepl("input_data/_new/admin/BRA/wiki_minwage", registry_text, fixed = TRUE))
  admin_pit_expect_true(grepl("input_data/_new/admin/COL/1_Cuantiles_Ingreso_Bruto_Naturales_2014-", registry_text, fixed = TRUE))

  sources <- yaml::read_yaml(file.path(repo_root_for_tests, "config", "sources.yml"))$sources
  minwage <- Filter(function(source) identical(source$id, "bra-minwage"), sources)[[1L]]
  expect_equal(minwage$fetch_target, "input_data/_new/admin/BRA/wiki_minwage.csv")
})

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

test_that("PIT admin explorer reports Brazil min-wage dependency actions", {
  skip_if_not_installed("openxlsx")
  missing <- run_admin_pit_explorer(root = admin_pit_fixture_repo(minwage = "missing"), write_outputs = FALSE)
  action <- missing$outputs$dependency_actions[missing$outputs$dependency_actions$dependency_id == "bra-minwage", , drop = FALSE]
  expect_equal(action$status, "blocked_missing_aux")
  expect_equal(action$next_command, "dina sources fetch bra-minwage")
  missing_cmp <- missing$outputs$aux_comparison_summary[missing$outputs$aux_comparison_summary$dependency_id == "bra-minwage", , drop = FALSE]
  expect_equal(missing_cmp$status, "blocked_missing_aux")
  expect_equal(missing_cmp$next_command, "dina sources fetch bra-minwage")

  canonical <- run_admin_pit_explorer(root = admin_pit_fixture_repo(minwage = "canonical"), write_outputs = FALSE)
  dep <- canonical$outputs$aux_dependency_summary[canonical$outputs$aux_dependency_summary$dependency_id == "bra-minwage", , drop = FALSE]
  expect_equal(dep$status, "carried_forward_aux")
  expect_equal(dep$severity, "info")
  canonical_cmp <- canonical$outputs$aux_comparison_summary[canonical$outputs$aux_comparison_summary$dependency_id == "bra-minwage", , drop = FALSE]
  expect_equal(canonical_cmp$status, "carried_forward_aux")
  expect_equal(canonical_cmp$current_years, paste(2007:2024, collapse = ","))

  candidate <- run_admin_pit_explorer(root = admin_pit_fixture_repo(minwage = "new"), write_outputs = FALSE)
  dep <- candidate$outputs$aux_dependency_summary[candidate$outputs$aux_dependency_summary$dependency_id == "bra-minwage", , drop = FALSE]
  expect_equal(dep$status, "aux_candidate")
  cmp <- candidate$outputs$aux_comparison_summary[candidate$outputs$aux_comparison_summary$dependency_id == "bra-minwage", , drop = FALSE]
  expect_equal(cmp$status, "aux_validated_append_only")
  expect_equal(cmp$current_years, paste(2007:2023, collapse = ","))
  expect_equal(cmp$incoming_years, paste(2007:2024, collapse = ","))
  expect_equal(cmp$extension_years, "2024")
  expect_equal(cmp$overlap_years, paste(2007:2023, collapse = ","))
  detail <- candidate$outputs$aux_comparison_detail[candidate$outputs$aux_comparison_detail$dependency_id == "bra-minwage", , drop = FALSE]
  expect_equal(detail$value_status[detail$year == 2024L], "extension")
})

test_that("PIT admin explorer blocks invalid Brazil min-wage comparison states", {
  skip_if_not_installed("openxlsx")
  changed <- run_admin_pit_explorer(root = admin_pit_fixture_repo(minwage = "changed_overlap"), write_outputs = FALSE)
  changed_cmp <- changed$outputs$aux_comparison_summary[changed$outputs$aux_comparison_summary$dependency_id == "bra-minwage", , drop = FALSE]
  expect_equal(changed_cmp$status, "blocked_aux_overlap_changed")
  expect_equal(changed_cmp$changed_overlap_years, "2023")
  expect_equal(changed$manifest$value[changed$manifest$key == "status"], "blocked")

  history <- run_admin_pit_explorer(root = admin_pit_fixture_repo(minwage = "missing_history"), write_outputs = FALSE)
  history_cmp <- history$outputs$aux_comparison_summary[history$outputs$aux_comparison_summary$dependency_id == "bra-minwage", , drop = FALSE]
  expect_equal(history_cmp$status, "blocked_aux_missing_canonical_years")
  expect_equal(history_cmp$dropped_current_years, paste(2000:2006, collapse = ","))

  missing_required <- run_admin_pit_explorer(root = admin_pit_fixture_repo(minwage = "missing_required"), write_outputs = FALSE)
  required_cmp <- missing_required$outputs$aux_comparison_summary[missing_required$outputs$aux_comparison_summary$dependency_id == "bra-minwage", , drop = FALSE]
  expect_equal(required_cmp$status, "blocked_aux_missing_required_years")
  expect_equal(required_cmp$missing_required_years, "2024")
  expect_equal(required_cmp$next_command, "dina sources fetch bra-minwage")

  ambiguous <- run_admin_pit_explorer(root = admin_pit_fixture_repo(minwage = "ambiguous"), write_outputs = FALSE)
  ambiguous_cmp <- ambiguous$outputs$aux_comparison_summary[ambiguous$outputs$aux_comparison_summary$dependency_id == "bra-minwage", , drop = FALSE]
  expect_equal(ambiguous_cmp$status, "blocked_aux_ambiguous_candidates")
  expect_match(ambiguous_cmp$detail, "More than one incoming")
})

test_that("PIT admin explorer ignores old admin_tax inbox paths", {
  skip_if_not_installed("openxlsx")
  root <- admin_pit_fixture_repo()
  unlink(file.path(root, "input_data", "_new", "admin"), recursive = TRUE)
  admin_pit_write_workbook(file.path(root, "input_data", "_new", "admin_tax", "PUB_Total_2024.xlsx"), "Datos", "old-bucket", years = 2005:2024)
  admin_pit_write_workbook(file.path(root, "input_data", "_new", "admin_tax", "gn-irpf-ac2024.xlsx"), "Tab8", "old-bucket")
  admin_pit_col_files(file.path(root, "input_data", "_new", "admin_tax", "1_Cuantiles_Ingreso_Bruto_Naturales_2014-2023"), 2014:2023, nested = TRUE)

  result <- run_admin_pit_explorer(root = root, write_outputs = FALSE)
  new_rows <- result$outputs$source_inventory[result$outputs$source_inventory$source_set == "new", , drop = FALSE]
  admin_pit_expect_true(all(new_rows$status == "no_file"))
  admin_pit_expect_false(any(grepl("input_data/_new/admin_tax", new_rows$rel, fixed = TRUE)))
  admin_pit_expect_true(all(result$outputs$extension_summary$status == "blocked_missing_new"))
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
  minwage_path <- file.path(root, "input_data", "admin_data", "BRA", "downloads", "wiki_minwage.csv")
  admin_pit_expect_true(2024L %in% utils::read.csv(minwage_path)$year)
  chl_2024 <- file.path(root, "input_data", "admin_data", "CHL", "PUB_Total_2024.xlsx")
  expect_equal(openxlsx::read.xlsx(chl_2024, sheet = "Datos", colNames = FALSE)[1, 1], "new-2024")

  restore <- admin_pit_include_restore_sources(root = root, confirm_run = confirm$paths$root)
  expect_equal(openxlsx::read.xlsx(chl_2024, sheet = "Datos", colNames = FALSE)[1, 1], "old-2024")
  admin_pit_expect_false(file.exists(file.path(root, "input_data", "admin_data", "BRA", "gn-irpf-ac2024.xlsx")))
  admin_pit_expect_false(file.exists(file.path(root, "input_data", "admin_data", "COL", "1_Cuantiles_Ingreso_Bruto_Naturales_2014-2023")))
  admin_pit_expect_false(file.exists(file.path(root, "input_data", "admin_data", "CHL", "_clean", "total-pre-CHL.xlsx")))
  admin_pit_expect_false(2024L %in% utils::read.csv(minwage_path)$year)
  admin_pit_expect_true(any(restore$outputs$restore_report$restore_status == "removed_promoted_destination"))
  expect_equal(admin_pit_hashes(root), before)
})

test_that("PIT admin include validates Brazil minimum-wage aux before cleaners", {
  skip_if_not_installed("openxlsx")

  missing_root <- admin_pit_fixture_repo(minwage = "missing")
  missing_explore <- run_admin_pit_explorer(root = missing_root, write_outputs = TRUE)
  missing_include <- run_admin_pit_include(root = missing_root, exploration_run = missing_explore$paths$root, write_outputs = TRUE, cleaner_mode = "mock")
  expect_equal(missing_include$manifest$value[missing_include$manifest$key == "status"], "blocked")
  bra <- missing_include$outputs$include_summary[missing_include$outputs$include_summary$source_id == "bra-pit-total", , drop = FALSE]
  chl <- missing_include$outputs$include_summary[missing_include$outputs$include_summary$source_id == "chl-pit-total", , drop = FALSE]
  col <- missing_include$outputs$include_summary[missing_include$outputs$include_summary$source_id == "col-pit-total", , drop = FALSE]
  expect_equal(bra$status, "blocked")
  expect_equal(chl$status, "all_good")
  expect_equal(col$status, "all_good")
  expect_equal(missing_include$outputs$aux_validation_report$status[missing_include$outputs$aux_validation_report$dependency_id == "bra-minwage"], "blocked_missing_aux")
  expect_equal(missing_include$outputs$aux_validation_report$next_command[missing_include$outputs$aux_validation_report$dependency_id == "bra-minwage"], "dina sources fetch bra-minwage")

  canonical_root <- admin_pit_fixture_repo(minwage = "canonical")
  canonical_explore <- run_admin_pit_explorer(root = canonical_root, write_outputs = TRUE)
  canonical_include <- run_admin_pit_include(root = canonical_root, exploration_run = canonical_explore$paths$root, write_outputs = FALSE, cleaner_mode = "mock")
  expect_equal(canonical_include$manifest$value[canonical_include$manifest$key == "status"], "all_good")
  expect_equal(canonical_include$outputs$aux_validation_report$status[canonical_include$outputs$aux_validation_report$dependency_id == "bra-minwage"], "aux_carried_forward_valid")
  admin_pit_expect_false(any(canonical_include$outputs$promotion_plan$artifact_type == "aux_source" & canonical_include$outputs$promotion_plan$source_id == "bra-minwage"))

  changed_root <- admin_pit_fixture_repo(minwage = "changed_overlap")
  changed_explore <- run_admin_pit_explorer(root = changed_root, write_outputs = TRUE)
  changed_include <- run_admin_pit_include(root = changed_root, exploration_run = changed_explore$paths$root, write_outputs = FALSE, cleaner_mode = "mock")
  expect_equal(changed_include$outputs$aux_validation_report$status[changed_include$outputs$aux_validation_report$dependency_id == "bra-minwage"], "blocked_aux_overlap_changed")

  history_root <- admin_pit_fixture_repo(minwage = "missing_history")
  history_explore <- run_admin_pit_explorer(root = history_root, write_outputs = TRUE)
  history_include <- run_admin_pit_include(root = history_root, exploration_run = history_explore$paths$root, write_outputs = FALSE, cleaner_mode = "mock")
  expect_equal(history_include$outputs$aux_validation_report$status[history_include$outputs$aux_validation_report$dependency_id == "bra-minwage"], "blocked_aux_missing_canonical_years")
})

test_that("isolated PIT admin include reports aux sources without configured files instead of crashing", {
  skip_if_not_installed("yaml")
  root <- admin_pit_fixture_repo()
  sources_path <- file.path(root, "config", "sources.yml")
  sources <- yaml::read_yaml(sources_path)
  sources$sources <- lapply(sources$sources, function(source) {
    if (identical(source$id, "chl-uta")) {
      source$inbox <- NULL
      source$canonical <- NULL
    }
    source
  })
  dina_write_yaml(sources, sources_path)

  paths <- list(staged_repo = file.path(root, "stage"))
  aux <- admin_pit_include_stage_one_aux(root, paths, "chl-pit-total", "chl-uta")
  expect_equal(aux$status, "legacy_live_aux")
  expect_equal(aux$severity, "warning")
})

test_that("Brazil minimum-wage fetcher parses generic historical table columns", {
  skip_if_not_installed("rvest")
  html <- tempfile("bra-minwage-", fileext = ".html")
  target <- tempfile("wiki-minwage-", fileext = ".csv")
  writeLines(c(
    "<html><body>",
    "<table>",
    "<tr><th>Country</th><th>Salario minimo mensual</th><th>Inicio de vigencia</th></tr>",
    "<tr><td>Brasil</td><td>R$ 1.412,00</td><td>1 de enero de 2024</td></tr>",
    "</table>",
    "<table>",
    "<tr><td>Entrada en vigencia</td><td>Valor nominal</td><td>Other</td></tr>",
    "<tr><td>1 de enero de 2023</td><td>R$ 1.100,00</td><td>-</td></tr>",
    "<tr><td>1 de enero de 2024</td><td>R$ 1.212,00</td><td>-</td></tr>",
    "<tr><td>1 de enero de 2025</td><td>R$ 1.320,00</td><td>-</td></tr>",
    "</table>",
    "</body></html>"
  ), html)
  fetcher <- file.path(repo_root_for_tests, "code", "R", "manual-downloaders", "fetch_bra_minwage.R")
  status <- system2(
    file.path(R.home("bin"), "Rscript"),
    args = c(fetcher, "--target", target),
    env = paste0("DINA_FETCH_BRA_MINWAGE_URL=", shQuote(html)),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_null(attr(status, "status"))
  out <- utils::read.csv(target)
  expect_equal(out$year, c(2023L, 2024L, 2025L))
  expect_equal(out$minwage, c(1100, 1212, 1320))
})

test_that("isolated PIT admin confirm refuses changed incoming source fingerprints", {
  skip_if_not_installed("digest")
  skip_if_not_installed("openxlsx")
  root <- admin_pit_fixture_repo()
  explore <- run_admin_pit_explorer(root = root, write_outputs = TRUE)
  include <- run_admin_pit_include(root = root, exploration_run = explore$paths$root, write_outputs = TRUE, cleaner_mode = "mock")

  admin_pit_write_workbook(file.path(root, "input_data", "_new", "admin", "CHL", "PUB_Total_2024.xlsx"), "Datos", "changed-after-dry-run")

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

test_that("main dina CLI dispatches admin PIT explore and table to isolated modules", {
  skip_if_not_installed("openxlsx")
  root <- admin_pit_fixture_repo()
  explore <- run_dina_cli(c("sources", "explore", "admin"), root = root)
  expect_equal(explore$status, 0L)
  expect_match(explore$output, "PIT Admin Explore")
  expect_match(explore$output, "chl-pit-total")
  expect_match(explore$output, "2005-2022 \\(18y\\)")
  expect_match(explore$output, "Aux source coverage:")
  expect_match(explore$output, "bra-minwage")
  expect_match(explore$output, "aux validated append only")
  expect_match(explore$output, "dina sources table admin")
  admin_pit_expect_false(grepl("Experimental isolated source workflow", explore$output, fixed = TRUE))
  admin_pit_expect_false(grepl("review: structure=", explore$output, fixed = TRUE))
  admin_pit_expect_false(grepl("2005,2006,2007,2008", explore$output, fixed = TRUE))

  tables <- run_dina_cli(c("sources", "table", "admin", "--limit", "2"), root = root)
  expect_equal(tables$status, 0L)
  expect_match(tables$output, "PIT Admin Tables")
  expect_match(tables$output, "PIT Admin Table: extension_summary")
  expect_match(tables$output, "PIT Admin Table: aux_comparison_summary")
  expect_match(tables$output, "PIT Admin Table: aux_comparison_detail")
  expect_match(tables$output, "PIT Admin Table: source_inventory")

  aux_table <- run_dina_cli(c("sources", "table", "admin", "aux_comparison_summary"), root = root)
  expect_equal(aux_table$status, 0L)
  expect_match(aux_table$output, "aux_validated_append_only")

  table <- run_dina_cli(c("sources", "table", "admin", "year_expectations", "--country", "CHL"), root = root)
  expect_equal(table$status, 0L)
  expect_match(table$output, "PIT Admin Table")
  expect_match(table$output, "chl-pit-total")

  unlink(file.path(root, "intermediary_data", "population", "SurveyPop.dta"))
  include <- run_dina_cli(c("sources", "include", "admin"), root = root)
  expect_equal(include$status, 0L)
  expect_match(include$output, "Overall status: blocked")
  expect_match(include$output, "Blocking items:")
  expect_match(include$output, "missing static input")
  expect_match(include$output, "SurveyPop.dta")
  expect_match(include$output, "dina sources explore surveys")
  admin_pit_expect_false(grepl("Experimental isolated source workflow", include$output, fixed = TRUE))
})
