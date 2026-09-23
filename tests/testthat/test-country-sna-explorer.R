source(file.path(repo_root_for_tests, "code", "R", "source-diagnostics", "country_sna_explorer.R"))

Sys.unsetenv("LC_ALL")
country_sna_explorer_state_condition <- get("testthat_state_condition", asNamespace("testthat"))
assignInNamespace("testthat_state_condition", function(before, after, call = NULL) NULL, ns = "testthat")
if (requireNamespace("withr", quietly = TRUE)) {
  withr::defer(
    assignInNamespace("testthat_state_condition", country_sna_explorer_state_condition, ns = "testthat"),
    envir = testthat:::teardown_env()
  )
}

country_sna_explorer_quiet_locale <- function() {
  if (requireNamespace("withr", quietly = TRUE)) {
    withr::local_envvar(c(LC_ALL = NA_character_))
  }
}

test_that("B.5 zero handling is an explicit account-level exception", {
  contract <- country_sna_explorer_read_contract(repo_root_for_tests)
  expect_true(country_sna_explorer_zero_as_missing(contract, "B.5"))
  expect_false(country_sna_explorer_zero_as_missing(contract, "D.4"))
})

country_sna_explorer_fixture_contract <- function(root) {
  list(
    version = 1L,
    output_root = "output/experiments/country_sna_explore",
    years = list(first = 2024L, last = 2024L),
    economic_contract = list(
      countries = c("AAA", "MEX", "URY", "ECU"),
      zero_as_missing = TRUE,
      code_aliases = list(
        "D.4" = c("D.4", "D4"),
        "D.43" = c("D.43", "D43"),
        "D.44" = c("D.44", "D44"),
        "B.5" = c("B.5", "B5", "B.5b", "B5b")
      ),
      roles = list(
        households_r = list(sector = "households", direction = "resources"),
        households_u = list(sector = "households", direction = "uses"),
        NFC_r = list(sector = "NFC", direction = "resources")
      ),
      variables = list(
        primitives = list(
          list(name = "D4_cei", account = "D.4", role = "households_r"),
          list(name = "D43_cei", account = "D.43", role = "households_u"),
          list(name = "D44_cei", account = "D.44", role = "households_r"),
          list(name = "B5g_cei", account = "B.5", role = "households_r"),
          list(name = "NFC_r_D4_cei", account = "D.4", role = "NFC_r")
        ),
        derived = list()
      ),
      units = list(AAA = 10, MEX = 1, URY = 1, ECU = 1)
    ),
    source_discovery = list(
      countries = list(
        AAA = list(
          adapter_family = "rectangular_workbook",
          old = list(type = "single_stem", stem = "input_data/sna_country_data/AAA/fixture"),
          new = list(type = "single_pattern", pattern = "input_data/_new/sna/AAA/*.xlsx")
        ),
        MEX = list(
          adapter_family = "rectangular_workbook",
          old = list(
            type = "indexed_year_file",
            folder = "input_data/sna_country_data/MEX",
            index_file = "input_data/sna_country_data/MEX/indice_archivos.txt",
            title_contains = "Cuentas institucionales",
            unit_contains = "Precios corrientes",
            fallback_pattern = "input_data/sna_country_data/MEX/CSI_{index}.xlsx",
            fallback_index_offset = 2000L
          ),
          new = list(type = "single_pattern", pattern = "input_data/_new/sna/MEX/*.xlsx")
        ),
        URY = list(
          adapter_family = "sector_file_bundle",
          old = list(
            type = "sector_file_bundle",
            folder = "input_data/sna_country_data/URY",
            pattern = "CSI_3.{year}_SC_*.xlsx"
          ),
          new = list(type = "sector_file_bundle", folder = "input_data/_new/sna/URY", pattern = "CSI_3.{year}_SC_*.xlsx"),
          sector_codes = list(households = "S.1402", GG = "S.1301", ROW = "S.2000")
        ),
        ECU = list(
          adapter_family = "account_sheet_workbook",
          old = list(type = "single_pattern", pattern = "input_data/sna_country_data/ECU/mcs_cei_*.xlsx"),
          new = list(type = "single_pattern", pattern = "input_data/_new/sna/ECU/bam_cei_*.xlsx")
        )
      )
    ),
    layout_detection = list(
      max_rows = 80L,
      max_cols = 20L,
      header_rows_before_table = 4L,
      role_score_high = 0.70,
      role_score_low = 0.40,
      table_score_high = 0.70,
      table_score_low = 0.40,
      sheet_keywords = c("CEI", "cuentas corrientes", "tabulado"),
      account_sheet_keywords = c("produccion", "ahorro"),
      sector_labels = list(
        households = c("hogares", "familias"),
        NFC = c("sociedades no financieras", "no financieras"),
        GG = c("gobierno"),
        TOT = c("total economia"),
        ROW = c("resto del mundo")
      ),
      direction_labels = list(
        resources = c("recursos", "por cobrar"),
        uses = c("empleos", "por pagar")
      )
    )
  )
}

country_sna_explorer_write_rectangular_fixture <- function(path) {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "CEI_2024p")
  cells <- data.frame(
    X1 = c("", "", "", "", "", "", ""),
    X2 = c("", "codigo", "", "D.4", "D.43", "D.44", "B.5b"),
    X3 = c("", "descripcion", "", "property", "reinvested", "dividends", "balance"),
    X4 = c("", "Hogares Recursos", "", 100, NA, 25, 200),
    X5 = c("", "Hogares Empleos", "", NA, 20, NA, NA),
    X6 = c("", "Sociedades no financieras Recursos", "", 500, 30, 35, 800),
    stringsAsFactors = FALSE
  )
  openxlsx::writeData(wb, "CEI_2024p", cells, colNames = FALSE)
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
}

test_that("adaptive normalizers and sheet scoring tolerate common variants", {
  country_sna_explorer_quiet_locale()
  expect_equal(country_sna_explorer_normalize_code(c(" D4 ", "D.43", "B5b")), c("D.4", "D.43", "B.5B"))
  expect_equal(country_sna_explorer_normalize_code("D.4 - Property income"), "D.4")
  expect_match(country_sna_explorer_text("Hogares y ISFLSH"), "hogares")

  contract <- country_sna_explorer_fixture_contract(tempdir())
  score <- country_sna_explorer_sheet_score("CEI_2024p", 2024L, contract, "rectangular_workbook")
  expect_gt(score$score, 0.70)
  expect_match(score$reason, "sheet_year_matches")
})

test_that("Mexico configured layout reads code-in-label cells and known role columns", {
  country_sna_explorer_quiet_locale()
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("readxl")

  root <- tempfile("country-sna-mex-configured-")
  old_dir <- file.path(root, "input_data", "sna_country_data", "MEX")
  new_dir <- file.path(root, "input_data", "_new", "sna", "MEX")
  dir.create(old_dir, recursive = TRUE)
  dir.create(new_dir, recursive = TRUE)
  writeLines("CSI_24.xlsx|Cuentas institucionales|Precios corrientes|2024", file.path(old_dir, "indice_archivos.txt"))
  writeLines("CSI_24.xlsx|Cuentas institucionales|Precios corrientes|2024", file.path(new_dir, "indice_archivos.txt"))
  write_fixture <- function(path, d4) {
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "Tabulado")
    row <- as.data.frame(as.list(rep(NA, 19L)), stringsAsFactors = FALSE)
    row[[1L]] <- "D.4 - Renta de la propiedad"
    row[[3L]] <- d4 + 20
    row[[8L]] <- d4 + 10
    row[[9L]] <- d4
    openxlsx::writeData(wb, "Tabulado", row, startRow = 68L, colNames = FALSE)
    openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  }
  write_fixture(file.path(old_dir, "CSI_24.xlsx"), 100)
  write_fixture(file.path(new_dir, "CSI_24.xlsx"), 110)

  contract <- country_sna_explorer_fixture_contract(root)
  contract$source_discovery$countries$MEX$new <- list(
    type = "indexed_year_file", folder = "input_data/_new/sna/MEX", index_file = "input_data/_new/sna/MEX/indice_archivos.txt",
    title_contains = "Cuentas institucionales", unit_contains = "Precios corrientes",
    fallback_pattern = "input_data/_new/sna/MEX/CSI_{index}.xlsx", fallback_index_offset = 2000L
  )
  include_contract <- list(
    variables = contract$economic_contract$variables,
    country_rules = list(MEX = list(layouts = list(list(
      years = list(min = 2000L), sheet = list(type = "fixed", value = "Tabulado"), range = "A68:S153",
      columns = list(code = "A", code_long = "A", households_r = "I", households_u = "H", NFC_r = "C")
    ))))
  )
  result <- country_sna_explorer_audit(contract, root, years = 2024L, countries = "MEX", include_contract = include_contract)
  candidate <- result$value_candidates[
    result$value_candidates$source_set == "new" & result$value_candidates$variable == "D4_cei",
    , drop = FALSE
  ]
  expect_equal(candidate$status, "accepted_high_confidence")
  expect_equal(candidate$value_standardized, 110)
  expect_true(any(result$table_candidates$evidence == "configured_layout=A68:S153;code_column=A"))

  labels <- c("r p", "u a")
  expect_equal(country_sna_explorer_label_score(country_sna_explorer_text("R/P"), labels[[1L]]), 1)
  expect_equal(country_sna_explorer_label_score(country_sna_explorer_text("U/A"), labels[[2L]]), 1)
})

test_that("explorer imports default countries from project config", {
  country_sna_explorer_quiet_locale()
  root <- tempfile("country-sna-explorer-countries-")
  dir.create(file.path(root, "config"), recursive = TRUE)
  yaml::write_yaml(list(countries = c("MEX", "AAA", "ARG"), years = list(first = 2024L, last = 2024L)), file.path(root, "config", "dina.yml"))
  contract <- country_sna_explorer_fixture_contract(root)
  contract$economic_contract$countries <- c("ECU", "URY")

  expect_equal(country_sna_explorer_project_countries(contract, root), c("MEX", "AAA"))
})

test_that("adaptive table and role detection finds shifted rectangular layouts", {
  country_sna_explorer_quiet_locale()
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("readxl")

  root <- tempfile("country-sna-adaptive-rect-")
  dir.create(file.path(root, "input_data", "sna_country_data", "AAA"), recursive = TRUE)
  dir.create(file.path(root, "input_data", "_new", "sna", "AAA"), recursive = TRUE)
  path <- file.path(root, "input_data", "sna_country_data", "AAA", "fixture.xlsx")
  country_sna_explorer_write_rectangular_fixture(path)
  country_sna_explorer_write_rectangular_fixture(file.path(root, "input_data", "_new", "sna", "AAA", "fixture_new.xlsx"))
  contract <- country_sna_explorer_fixture_contract(root)
  result <- run_country_sna_explorer(root = root, contract_path = {
    p <- file.path(root, "contract.yml")
    yaml::write_yaml(contract, p)
    p
  }, years = 2024L, countries = "AAA", write_outputs = FALSE)

  tables <- result$outputs$table_candidates
  expect_true(any(tables$code_col_letter == "B"))
  expect_true(any(tables$status == "accepted_high_confidence"))

  roles <- result$outputs$role_candidates
  expect_true(any(roles$role == "households_r" & roles$role_col_letter == "D" & roles$status == "accepted_high_confidence"))
  expect_true(any(roles$role == "households_u" & roles$role_col_letter == "E" & roles$status == "accepted_high_confidence"))

  values <- result$outputs$value_candidates
  d43 <- values[values$variable == "D43_cei" & values$source_set == "old", , drop = FALSE]
  expect_equal(d43$status, "accepted_high_confidence")
  expect_equal(d43$status_group, "accepted")
  expect_equal(d43$resolution_stage, "accepted")
  expect_equal(d43$value_standardized, 200)
  expect_equal(d43$row_evidence, 5L)
  expect_equal(d43$column_evidence, 5L)

  expect_true(any(result$outputs$available_years$country == "AAA" & result$outputs$available_years$source_set == "new"))
  expect_true(any(grepl("sheet_likely_sheet", result$outputs$available_years$year_evidence, fixed = TRUE)))
  expect_true(any(result$outputs$source_match_summary$country == "AAA" & result$outputs$source_match_summary$matched_files > 0L))
  expect_true(any(result$outputs$extension_summary$country == "AAA" & result$outputs$extension_summary$status == "overlap_only"))
  expect_true(any(result$outputs$overlap_revision_detail$variable == "D4_cei" & result$outputs$overlap_revision_detail$status == "unchanged"))
  expect_true("no_old_candidate" %in% names(result$outputs$overlap_revision_summary))
  expect_true(any(result$outputs$value_status_summary$status == "accepted_high_confidence"))
  expect_true("resolution_stage" %in% names(result$outputs$value_status_summary))
  expect_true(any(result$outputs$structure_summary$country == "AAA"))
  expect_true(any(result$outputs$year_expectations$country == "AAA" & result$outputs$year_expectations$year_role == "overlap"))
  expect_true(any(result$outputs$variable_expectations$country == "AAA" & result$outputs$variable_expectations$variable == "D4_cei"))
  expect_equal(result$outputs$review_actions$action[result$outputs$review_actions$country == "AAA"], "include_dry_run")
})

test_that("explorer ignores retired country_sna inbox paths", {
  country_sna_explorer_quiet_locale()
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("readxl")

  root <- tempfile("country-sna-retired-inbox-")
  dir.create(file.path(root, "input_data", "sna_country_data", "AAA"), recursive = TRUE)
  dir.create(file.path(root, "input_data", "_new", "country_sna", "AAA"), recursive = TRUE)
  country_sna_explorer_write_rectangular_fixture(file.path(root, "input_data", "sna_country_data", "AAA", "fixture.xlsx"))
  country_sna_explorer_write_rectangular_fixture(file.path(root, "input_data", "_new", "country_sna", "AAA", "fixture_new.xlsx"))
  contract <- country_sna_explorer_fixture_contract(root)
  result <- run_country_sna_explorer(root = root, contract_path = {
    p <- file.path(root, "contract.yml")
    yaml::write_yaml(contract, p)
    p
  }, years = 2024L, countries = "AAA", write_outputs = FALSE)

  new_rows <- result$outputs$source_inventory[result$outputs$source_inventory$country == "AAA" & result$outputs$source_inventory$source_set == "new", , drop = FALSE]
  expect_true(nrow(new_rows) > 0L)
  expect_true(all(new_rows$status == "no_file"))
  expect_false(any(grepl("input_data/_new/country_sna", new_rows$file, fixed = TRUE)))
})

test_that("adaptive value candidates flag duplicate conflicts", {
  country_sna_explorer_quiet_locale()
  contract <- country_sna_explorer_fixture_contract(tempdir())
  grid <- data.frame(
    code = c("D.4", "D.4"),
    label = c("one", "two"),
    hh_r = c(1, 2),
    stringsAsFactors = FALSE
  )
  table <- data.frame(
    country = "AAA", source_set = "old", adapter_family = "rectangular_workbook",
    year = 2024L, file = "fixture.xlsx", sheet = "CEI_2024p", table_id = "fixture",
    code_col = 1L, code_col_letter = "A", row_start = 1L, row_end = 2L,
    account_hit_count = 2L, account_diversity = 1L, table_score = 1,
    status = "accepted_high_confidence", evidence = "fixture",
    stringsAsFactors = FALSE
  )
  roles <- data.frame(
    role = "households_r", role_col = 3L, role_score = 1,
    status = "accepted_high_confidence", table_id = "fixture",
    stringsAsFactors = FALSE
  )
  values <- country_sna_explorer_value_candidates(grid, table, roles, contract)
  expect_equal(values$status[values$variable == "D4_cei"], "duplicate_conflict")
  expect_equal(values$resolution_stage[values$variable == "D4_cei"], "value")
  expect_match(values$reason[values$variable == "D4_cei"], "duplicate_account_values_conflict")
})

test_that("B.5 duplicate zero is excluded while other account zeros remain evidence", {
  country_sna_explorer_quiet_locale()
  contract <- country_sna_explorer_fixture_contract(tempdir())
  contract$economic_contract$zero_as_missing <- FALSE
  contract$economic_contract$zero_as_missing_accounts <- "B.5"
  grid <- data.frame(code = c("B.5", "B.5"), label = c("uses", "resources"), hh_r = c(0, 200), stringsAsFactors = FALSE)
  table <- data.frame(
    country = "AAA", source_set = "old", adapter_family = "rectangular_workbook",
    year = 2024L, file = "fixture.xlsx", sheet = "CEI_2024p", table_id = "fixture",
    code_col = 1L, code_col_letter = "A", row_start = 1L, row_end = 2L,
    account_hit_count = 2L, account_diversity = 1L, table_score = 1,
    status = "accepted_high_confidence", evidence = "fixture", stringsAsFactors = FALSE
  )
  roles <- data.frame(
    role = "households_r", role_col = 3L, role_score = 1,
    status = "accepted_high_confidence", table_id = "fixture", stringsAsFactors = FALSE
  )
  values <- country_sna_explorer_value_candidates(grid, table, roles, contract)
  b5 <- values[values$variable == "B5g_cei", , drop = FALSE]
  expect_match(b5$status, "accepted")
  expect_equal(b5$value_raw, 200)
})

test_that("adaptive available-year summary expands generic filename spans", {
  country_sna_explorer_quiet_locale()
  source_inventory <- data.frame(
    country = "AAA",
    source_set = "new",
    adapter_family = "rectangular_workbook",
    selector = "single_pattern",
    year = NA_integer_,
    file = file.path(tempdir(), "cei_2022_2024p.xlsx"),
    files = NA_character_,
    status = "matched",
    matched_by = "single_pattern",
    expected_index = NA_integer_,
    index_consistency = NA_character_,
    notes = NA_character_,
    stringsAsFactors = FALSE
  )
  contract <- country_sna_explorer_fixture_contract(tempdir())
  years <- country_sna_explorer_available_years(source_inventory, data.frame(stringsAsFactors = FALSE), contract)
  expect_equal(years$years, "2022,2023,2024")
  expect_equal(years$year_count, 3L)
  expect_match(years$year_evidence, "source_filename_span")
  summary <- country_sna_explorer_source_match_summary(source_inventory)
  expect_equal(summary$first_matched_year, 2022L)
  expect_equal(summary$last_matched_year, 2024L)
})

test_that("missing incoming files are classified as source gaps, never confirmed missing data", {
  inventory <- data.frame(
    country = c("AAA", "AAA", "BBB", "BBB"),
    source_set = c("old", "new", "old", "new"),
    year = c(2024L, 2024L, 2024L, 2024L),
    status = c("matched", "no_file", "no_file", "no_file"),
    stringsAsFactors = FALSE
  )
  extension <- data.frame(country = c("AAA", "BBB"), old_years = c("2024", "2024"), stringsAsFactors = FALSE)
  gaps <- country_sna_explorer_incoming_source_gaps(extension, inventory)
  expect_equal(gaps$country, "AAA")
  expect_equal(gaps$status, "incoming_source_not_supplied")
  expect_equal(nrow(country_sna_explorer_confirmed_missing_years(inventory)), 0L)
})

test_that("explorer performs a fresh inspection on every normal run", {
  skip_if_not_installed("openxlsx")
  root <- tempfile("country-sna-cache-")
  dir.create(file.path(root, "input_data", "sna_country_data", "AAA"), recursive = TRUE)
  dir.create(file.path(root, "input_data", "_new", "sna", "AAA"), recursive = TRUE)
  country_sna_explorer_write_rectangular_fixture(file.path(root, "input_data", "sna_country_data", "AAA", "fixture.xlsx"))
  country_sna_explorer_write_rectangular_fixture(file.path(root, "input_data", "_new", "sna", "AAA", "incoming_2024.xlsx"))
  contract <- country_sna_explorer_fixture_contract(root)
  contract$economic_contract$countries <- "AAA"
  contract$source_discovery$countries <- contract$source_discovery$countries["AAA"]
  contract_path <- file.path(root, "contract.yml")
  yaml::write_yaml(contract, contract_path)
  output_dir <- "output/cache-test"
  progress <- character()
  first <- run_country_sna_explorer(root = root, contract_path = contract_path, output_dir = output_dir,
    years = 2024L, countries = "AAA", write_outputs = TRUE, scope = list(config_source = "fixture", years = 2024L),
    progress = function(message) progress <<- c(progress, message))
  expect_equal(first$outputs$cache_status$status, "computed")
  expect_true(any(grepl("Preparing a fresh", progress)))
  expect_true(any(grepl("Discovering current and incoming", progress)))
  expect_true(any(grepl("Reading changed workbooks", progress)))
  expect_true(any(grepl("Analyzing evidence", progress)))
  expect_true(any(grepl("Writing review report", progress)))
  second <- run_country_sna_explorer(root = root, contract_path = contract_path, output_dir = output_dir,
    years = 2024L, countries = "AAA", write_outputs = FALSE, scope = list(config_source = "fixture", years = 2024L))
  expect_equal(second$outputs$cache_status$status, "computed")
  path <- file.path(root, "input_data", "_new", "sna", "AAA", "incoming_2024.xlsx")
  Sys.setFileTime(path, Sys.time() + 2)
  third <- run_country_sna_explorer(root = root, contract_path = contract_path, output_dir = output_dir,
    years = 2024L, countries = "AAA", write_outputs = FALSE, scope = list(config_source = "fixture", years = 2024L))
  expect_equal(third$outputs$cache_status$status, "computed")
  expect_equal(third$outputs$cache_status$evidence_reused, 0L)
  expect_equal(third$outputs$cache_status$evidence_analyzed, 0L)
})

test_that("explorer identities use their supplied root outside the repository", {
  root <- tempfile("country-sna-explicit-root-")
  outside <- tempfile("country-sna-outside-")
  dir.create(root, recursive = TRUE)
  dir.create(outside, recursive = TRUE)
  contract <- file.path(root, "contract.yml")
  include <- file.path(root, "include.yml")
  file.create(contract, include)
  scope <- list(config_source = "fixture", years = 2024L, effective_config_hash = "fixture")
  original <- getwd()
  on.exit(setwd(original), add = TRUE)
  setwd(outside)
  first <- country_sna_explorer_scope_identity(scope, root, contract, include)
  scoped_countries <- scope
  scoped_countries$countries <- "AAA"
  expect_false(identical(first, country_sna_explorer_scope_identity(scoped_countries, root, contract, include)))
  inventory <- data.frame(
    country = "AAA", source_set = "new", selector = "fixture", year = 2024L, status = "matched",
    file = contract, matched_by = "fixture", index_consistency = "ok", stringsAsFactors = FALSE
  )
  first_key <- country_sna_explorer_cache_key("AAA", inventory, first, contract, include, root)
  setwd(tempdir())
  second <- country_sna_explorer_scope_identity(scope, root, contract, include)
  second_key <- country_sna_explorer_cache_key("AAA", inventory, second, contract, include, root)
  expect_equal(first, second)
  expect_equal(first_key, second_key)
})

test_that("explorer review actions stay broad and expectation-oriented", {
  country_sna_explorer_quiet_locale()
  extension_summary <- data.frame(
    country = c("LAY", "NOO", "ROL", "RDY"),
    old_years = c("2024", "2023", "2024", "2024"),
    new_years = c("2024", "2024", "2024", "2024"),
    overlap_years = c("2024", "", "2024", "2024"),
    extension_years = c("", "2024", "", ""),
    missing_in_new_years = c("", "2023", "", ""),
    status = c("overlap_only", "no_overlap", "overlap_only", "overlap_only"),
    stringsAsFactors = FALSE
  )
  structure_summary <- data.frame(
    country = c("LAY", "NOO", "ROL", "RDY"),
    extension_status = c("overlap_only", "no_overlap", "overlap_only", "overlap_only"),
    structure_status = c("layout_adapter_required", "structure_evidence_available", "structure_review_needed", "structure_evidence_available"),
    matched_new_files = c(1L, 1L, 1L, 1L),
    likely_new_sheets = c(1L, 1L, 0L, 1L),
    possible_new_sheets = c(0L, 0L, 0L, 0L),
    developer_accepted_values = c(0L, 1L, 0L, 1L),
    developer_unresolved_values = c(1L, 0L, 1L, 0L),
    note = "fixture",
    stringsAsFactors = FALSE
  )
  actions <- country_sna_explorer_review_actions(extension_summary, structure_summary)
  expect_equal(actions$action[actions$country == "LAY"], "layout_adapter_required")
  expect_equal(actions$action[actions$country == "NOO"], "include_dry_run_no_overlap")
  expect_equal(actions$action[actions$country == "ROL"], "structure_review_needed")
  expect_equal(actions$action[actions$country == "RDY"], "include_dry_run")
  expect_false("unresolved_role" %in% names(actions))
})

test_that("adaptive Mexico source matching uses index metadata before year-minus-2000 fallback", {
  country_sna_explorer_quiet_locale()
  root <- tempfile("country-sna-adaptive-mex-")
  dir.create(file.path(root, "input_data", "sna_country_data", "MEX"), recursive = TRUE)
  writeLines(
    "CSI_99.xlsx|Cuentas institucionales de la economia, por sector|Precios corrientes en millones de pesos|2024",
    file.path(root, "input_data", "sna_country_data", "MEX", "indice_archivos.txt")
  )
  writeLines("placeholder", file.path(root, "input_data", "sna_country_data", "MEX", "CSI_99.xlsx"))
  writeLines("placeholder", file.path(root, "input_data", "sna_country_data", "MEX", "CSI_24.xlsx"))
  contract <- country_sna_explorer_fixture_contract(root)
  spec <- country_sna_explorer_country_rules(contract)$MEX$old
  resolved <- country_sna_explorer_resolve_indexed_year_file(spec, 2024L, root)
  expect_equal(basename(resolved$file), "CSI_99.xlsx")
  expect_equal(resolved$matched_by, "index_metadata")
  expect_equal(resolved$index_consistency, "differs_from_year_minus_2000")
})

test_that("adaptive audit handles Uruguay sector bundles without writing cei.xlsx", {
  country_sna_explorer_quiet_locale()
  skip_if_not_installed("openxlsx")

  root <- tempfile("country-sna-adaptive-ury-")
  dir.create(file.path(root, "input_data", "sna_country_data", "URY"), recursive = TRUE)
  path <- file.path(root, "input_data", "sna_country_data", "URY", "CSI_3.2024_SC_S.1402.xlsx")
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Sheet1")
  openxlsx::writeData(
    wb,
    "Sheet1",
    data.frame(code = c("D.4", "D.44"), label = c("property", "dividends"), value = c(100, 25)),
    colNames = FALSE
  )
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)

  contract <- country_sna_explorer_fixture_contract(root)
  contract_path <- file.path(root, "contract.yml")
  yaml::write_yaml(contract, contract_path)
  result <- run_country_sna_explorer(root = root, contract_path = contract_path, years = 2024L, countries = "URY", write_outputs = TRUE)

  expect_true(any(result$outputs$source_inventory$adapter_family == "sector_file_bundle"))
  expect_true(any(result$outputs$value_candidates$status == "accepted_low_confidence"))
  expect_false(file.exists(file.path(root, "input_data", "sna_country_data", "URY", "cei.xlsx")))
  expect_true(startsWith(result$paths$root, file.path(root, "output", "experiments", "country_sna_explore")))
})

test_that("adaptive audit inventories Ecuador account-sheet workbooks as unhandled layout family", {
  country_sna_explorer_quiet_locale()
  skip_if_not_installed("openxlsx")

  root <- tempfile("country-sna-adaptive-ecu-")
  dir.create(file.path(root, "input_data", "_new", "sna", "ECU"), recursive = TRUE)
  path <- file.path(root, "input_data", "_new", "sna", "ECU", "bam_cei_2018_2024p.xlsx")
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Produccion")
  openxlsx::writeData(wb, "Produccion", data.frame(x = 1))
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)

  contract <- country_sna_explorer_fixture_contract(root)
  contract_path <- file.path(root, "contract.yml")
  yaml::write_yaml(contract, contract_path)
  result <- run_country_sna_explorer(root = root, contract_path = contract_path, years = 2024L, countries = "ECU", write_outputs = FALSE)

  expect_true(any(result$outputs$sheet_inventory$status == "likely_sheet"))
  expect_true(any(result$outputs$value_candidates$status == "layout_family_unhandled"))
})

test_that("SNA explore/include are visible while diagnose is retired", {
  country_sna_explorer_quiet_locale()
  source_cli_for_tests()
  expect_true(grepl("sources explore FAMILY", dina_help_text(), fixed = TRUE))
  expect_true(grepl("sources include FAMILY", dina_help_text(), fixed = TRUE))
  expect_true(grepl("sources table FAMILY", dina_help_text(), fixed = TRUE))
  expect_true(grepl("dina sources explore FAMILY", dina_help_text("sources"), fixed = TRUE))
  expect_true(grepl("dina sources include FAMILY", dina_help_text("sources"), fixed = TRUE))
  expect_true(grepl("dina sources table FAMILY", dina_help_text("sources"), fixed = TRUE))
  expect_false(grepl("sources explore country-sna", dina_help_text(), fixed = TRUE))
  expect_false(grepl("dina sources explore country-sna", dina_help_text("sources"), fixed = TRUE))
  expect_false(grepl("sources diagnose country-sna", dina_help_text(), fixed = TRUE))
  expect_false(grepl("dina sources diagnose country-sna", dina_help_text("sources"), fixed = TRUE))
  catalog <- paste(dina_command_catalog_lines(dina_command_catalog()), collapse = "\n")
  expect_true(grepl("Explore source family", catalog, fixed = TRUE))
  expect_true(grepl("Include reviewed family", catalog, fixed = TRUE))
  expect_true(grepl("Review family details", catalog, fixed = TRUE))
  expect_true(grepl("Confirm source type", catalog, fixed = TRUE))
  expect_true(grepl("Restore source type", catalog, fixed = TRUE))
  expect_false(grepl("Country-SNA diagnostic", catalog, fixed = TRUE))
})

test_that("explore printer shows current years and no-new keep-current action", {
  country_sna_explorer_quiet_locale()
  source_cli_for_tests()
  result <- list(
    outputs = list(
      extension_summary = data.frame(
        country = c("AAA", "ECU"),
        old_years = c("2020,2021", "2023"),
        new_years = c("", "2023,2024"),
        overlap_years = c("", "2023"),
        extension_years = c("", "2024"),
        status = c("no_new_years_detected", "extension_found"),
        stringsAsFactors = FALSE
      ),
      structure_summary = data.frame(
        country = c("AAA", "ECU"),
        structure_status = c("no_new_years", "layout_adapter_required"),
        stringsAsFactors = FALSE
      ),
      review_actions = data.frame(
        country = c("AAA", "ECU"),
        action = c("keep_current", "layout_adapter_required"),
        next_command = c("", ""),
        stringsAsFactors = FALSE
      )
    ),
    paths = list(root = tempdir(), tables = tempdir())
  )
  output <- paste(capture.output(dina_print_country_sna_explore(result, dry_run = TRUE, is_terminal = FALSE)), collapse = "\n")
  expect_match(output, "current")
  expect_match(output, "2020,2021")
  expect_match(output, "keep current")
  expect_match(output, "adapter needed")
  expect_match(output, "Status notes")
})
