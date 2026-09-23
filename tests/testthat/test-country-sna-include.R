source(file.path(repo_root_for_tests, "code", "R", "source-diagnostics", "country_sna_include.R"))

Sys.unsetenv("LC_ALL")
country_sna_include_state_condition <- get("testthat_state_condition", asNamespace("testthat"))
assignInNamespace("testthat_state_condition", function(before, after, call = NULL) NULL, ns = "testthat")
if (requireNamespace("withr", quietly = TRUE)) {
  withr::defer(
    assignInNamespace("testthat_state_condition", country_sna_include_state_condition, ns = "testthat"),
    envir = testthat:::teardown_env()
  )
}

country_sna_include_fixture_contract <- function(root) {
  list(
    version = 1L,
    output_root = "output/experiments/country_sna_include",
    base_dataset = "intermediary_data/national_accounts/UNDATA-WID-Merged.dta",
    years = list(first = 2020L, last = 2020L),
    countries = "AAA",
    thresholds = list(anecdotal_pct = 1L, zero_as_missing = TRUE),
    code_aliases = list(
      "D.4" = c("D.4", "D4"),
      "D.43" = c("D.43", "D43"),
      "D.44" = c("D.44", "D44"),
      "B.5" = c("B.5", "B5")
    ),
    variables = list(
      primitives = list(
        list(name = "D4_cei", account = "D.4", role = "households_r"),
        list(name = "D43_cei", account = "D.43", role = "households_u"),
        list(name = "D44_cei", account = "D.44", role = "households_r"),
        list(name = "B5g_cei", account = "B.5", role = "households_r")
      ),
      derived = list(
        list(name = "ratio_d43_d4", op = "ratio", numerator = "D43_cei", denominator = "D4_cei"),
        list(name = "ratio_d44_d4", op = "ratio", numerator = "D44_cei", denominator = "D4_cei"),
        list(name = "ratio_d43d44", op = "sum", inputs = c("ratio_d43_d4", "ratio_d44_d4"))
      )
    ),
    country_rules = list(
      AAA = list(
        adapter = "workbook_table",
        magnitude = 10,
        source = list(type = "single_stem", stem = "input_data/sna_country_data/AAA/fixture"),
        layouts = list(list(
          years = list(min = 2020L),
          sheet = list(type = "fixed", value = "2020"),
          range = "A1:D4",
          columns = list(code = "A", code_long = "B", households_r = "C", households_u = "D")
        ))
      )
    )
  )
}

test_that("country SNA include contract loads literal Excel column letters", {
  contract <- country_sna_include_read_contract(repo_root_for_tests)
  expect_equal(contract$output_root, "output/experiments/country_sna_include")
  expect_equal(length(contract$countries), 9L)
  expect_false(isTRUE(contract$thresholds$zero_as_missing))
  expect_true(country_sna_include_zero_as_missing(contract, "B.5"))
  expect_false(country_sna_include_zero_as_missing(contract, "D.4"))

  flattened <- unlist(contract$country_rules, recursive = TRUE, use.names = FALSE)
  if (any(vapply(flattened, identical, logical(1), FALSE))) {
    fail("Contract should preserve literal Excel column letters instead of YAML booleans.")
  }
  if (!("N" %in% flattened)) {
    fail("Contract should include literal Excel column N.")
  }
})

test_that("country SNA include retains a published zero when policy requires it", {
  rows <- data.frame(
    role = "households_r", code = "D.4", value_raw = "0", value = 0,
    stringsAsFactors = FALSE
  )
  picked <- country_sna_include_pick_from_group(rows, "households_r", "D.4", zero_as_missing = FALSE)
  expect_equal(picked$status, "matched")
  expect_equal(picked$value, 0)
})

test_that("country SNA include imports default countries from project config", {
  root <- tempfile("country-sna-include-countries-")
  dir.create(file.path(root, "config"), recursive = TRUE)
  yaml::write_yaml(list(countries = c("MEX", "AAA", "ARG"), years = list(first = 2020L, last = 2020L)), file.path(root, "config", "dina.yml"))

  contract <- country_sna_include_fixture_contract(root)
  contract$countries <- c("AAA")
  contract$country_rules$MEX <- contract$country_rules$AAA
  contract_path <- file.path(root, "contract.yml")
  yaml::write_yaml(contract, contract_path)

  loaded <- country_sna_include_read_contract(root, contract_path)
  expect_equal(loaded$countries, c("MEX", "AAA"))
})

test_that("country SNA include stages every documented sector-bundle member", {
  root <- tempfile("country-sna-stage-bundle-")
  dir.create(root, recursive = TRUE)
  members <- file.path(root, c("CSI_3.2025_SC_S.1301.xlsx", "CSI_3.2025_SC_S.1402.xlsx", "CSI_3.2025_SC_S.2000.xlsx"))
  file.create(members)
  inventory <- data.frame(
    country = "URY", source_set = "new", selector = "sector_file_bundle",
    file = members[[1L]], files = paste(members, collapse = "|"), stringsAsFactors = FALSE
  )
  expanded <- country_sna_include_stage_candidate_rows(inventory)
  expect_equal(expanded$file, members)

  non_bundle <- inventory
  non_bundle$selector <- "year_pattern"
  expanded_non_bundle <- country_sna_include_stage_candidate_rows(non_bundle)
  expect_equal(expanded_non_bundle$file, members[[1L]])
})

test_that("Peru's include contract resolves its documented yearly releases", {
  contract <- country_sna_include_read_contract(repo_root_for_tests)
  source <- contract$country_rules$PER$source
  expect_equal(source$type, "year_pattern")
  expect_match(source$pattern, "\\{year\\}")
})

test_that("country SNA include Mexico resolver prefers index metadata", {
  root <- tempfile("country-sna-mex-index-")
  dir.create(file.path(root, "input_data", "sna_country_data", "MEX"), recursive = TRUE)
  writeLines(
    c(
      "Indice",
      "CSI_24.xlsx|Cuentas institucionales de la economia, por sector|Precios corrientes en millones de pesos|2024",
      "CSI_99.xlsx|Cuentas institucionales de la economia, por sector|Porcentaje|2024"
    ),
    file.path(root, "input_data", "sna_country_data", "MEX", "indice_archivos.txt")
  )
  writeLines("placeholder", file.path(root, "input_data", "sna_country_data", "MEX", "CSI_24.xlsx"))
  writeLines("placeholder", file.path(root, "input_data", "sna_country_data", "MEX", "CSI_99.xlsx"))

  spec <- list(
    type = "indexed_year_file",
    folder = "input_data/sna_country_data/MEX",
    index_file = "input_data/sna_country_data/MEX/indice_archivos.txt",
    title_contains = "Cuentas institucionales de la economia, por sector",
    unit_contains = "Precios corrientes en millones de pesos",
    fallback_pattern = "input_data/sna_country_data/MEX/CSI_{index}.xlsx",
    fallback_index_offset = 2000L
  )
  resolved <- country_sna_include_resolve_indexed_year_file(spec, 2024L, root)
  expect_equal(basename(resolved$file), "CSI_24.xlsx")
  expect_equal(resolved$selector, "indexed_year_file")
})

test_that("country SNA include sheet resolver accepts configured variants", {
  skip_if_not_installed("openxlsx")
  root <- tempfile("country-sna-exp-sheet-")
  dir.create(root, recursive = TRUE)
  path <- file.path(root, "fixture.xlsx")
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "CEI_2024p")
  openxlsx::writeData(wb, "CEI_2024p", data.frame(x = 1))
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)

  sheet_rule <- list(type = "year_variants", variants = c("CEI_{year}", "CEI_{year}p"))
  resolved <- country_sna_include_resolve_sheet(path, 2024L, sheet_rule)
  expect_equal(resolved$status, "matched")
  expect_equal(resolved$sheet, "CEI_2024p")
  expect_equal(resolved$match_kind, "declared_variant")

  openxlsx::addWorksheet(wb, "CEI_2023")
  openxlsx::writeData(wb, "CEI_2023", data.frame(x = 1))
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)

  resolved <- country_sna_include_resolve_sheet(path, 2023L, sheet_rule)
  expect_equal(resolved$status, "matched")
  expect_equal(resolved$sheet, "CEI_2023")
  expect_equal(resolved$match_kind, "exact")
})

test_that("country SNA include rejects unsupported or underspecified sheet rules", {
  contract <- country_sna_include_fixture_contract(tempdir())
  contract$country_rules$AAA$layouts[[1L]]$sheet <- list(type = "fuzzy")
  expect_error(country_sna_include_validate_contract(contract), "Unsupported sheet rule")
  contract$country_rules$AAA$layouts[[1L]]$sheet <- list(type = "template", value = "CEI")
  expect_error(country_sna_include_validate_contract(contract), "must contain")
})

test_that("Ecuador's current layout treats provisional sheet suffixes as variants", {
  contract <- country_sna_include_read_contract(repo_root_for_tests)
  layout <- country_sna_include_layout_for_year(contract$country_rules$ECU, 2023L)
  expect_equal(layout$sheet$type, "year_variants")
  expect_equal(layout$sheet$variants, c("CEI_{year}", "CEI_{year}p"))
  expect_true("CEI_2023" %in% country_sna_include_sheet_candidates(layout$sheet, 2023L))
  expect_true("CEI_2023p" %in% country_sna_include_sheet_candidates(layout$sheet, 2023L))
})

test_that("country SNA include rejects exploration from a different effective scope", {
  root <- tempfile("country-sna-scope-")
  dir.create(file.path(root, "explore", "tables"), recursive = TRUE)
  utils::write.csv(data.frame(
    config_source = "benchmark config/dina.yml", years = "2000,2001,2002,2003", effective_config_hash = "old", scope_identity = "old-scope",
    stringsAsFactors = FALSE
  ), file.path(root, "explore", "tables", "scope.csv"), row.names = FALSE)
  expect_error(
    country_sna_include_assert_exploration_scope(file.path(root, "explore"), list(
      config_source = "active update example (effective configuration)", years = 2000:2024,
      effective_config_hash = "new", identity = "new-scope"
    )),
    "different effective configuration"
  )
})

test_that("whole-year absence requires a recognized source and successful extraction setup", {
  detail <- data.frame(
    country = c("AAA", "AAA"), year = c(2024L, 2024L), variable = c("D4_cei", "B5g_cei"),
    expected_status = "expected_value", status = "warning_missing_expected_value",
    extract_status = c("no_code_match", "zero_treated_missing"),
    source_file = "/tmp/incoming.xlsx", sheet = "CEI_2024", stringsAsFactors = FALSE
  )
  confirmed <- country_sna_include_confirmed_missing_years(detail)
  expect_equal(confirmed$status, "whole_year_missing_confirmed")
  detail$extract_status[[1L]] <- "missing_sheet"
  expect_equal(nrow(country_sna_include_confirmed_missing_years(detail)), 0L)
})

test_that("country SNA include single-stem resolver supports extension priority", {
  root <- tempfile("country-sna-exp-stem-")
  dir.create(file.path(root, "input_data"), recursive = TRUE)
  file.create(file.path(root, "input_data", "source.xls"))
  file.create(file.path(root, "input_data", "source.xlsx"))

  resolved <- country_sna_include_resolve_single(
    list(
      type = "single_stem",
      stem = "input_data/source",
      extension_priority = c(".xls", ".xlsx")
    ),
    root
  )

  expect_equal(basename(resolved$file), "source.xls")
  expect_equal(length(resolved$candidates), 2L)
  expect_match(resolved$warning, "Multiple candidate workbooks")
})

test_that("country SNA include extractor keeps outputs disposable and formulas compatible", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("yaml")
  skip_if_not_installed("haven")

  root <- tempfile("country-sna-exp-run-")
  dir.create(file.path(root, "input_data", "sna_country_data", "AAA"), recursive = TRUE)
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "2020")
  openxlsx::writeData(
    wb,
    "2020",
    data.frame(
      code = c("D.4", "D.43", "D.44", "B.5"),
      label = c("property", "reinvested", "other property", "balance"),
      households_r = c(100, NA, 25, 200),
      households_u = c(NA, 0, NA, NA)
    ),
    colNames = FALSE
  )
  openxlsx::saveWorkbook(wb, file.path(root, "input_data", "sna_country_data", "AAA", "fixture.xlsx"), overwrite = TRUE)

  contract <- country_sna_include_fixture_contract(root)
  contract_path <- file.path(root, "contract.yml")
  yaml::write_yaml(contract, contract_path)

  result <- run_country_sna_include(root = root, contract_path = contract_path, write_outputs = TRUE)
  wide <- result$outputs$values_wide
  expect_equal(wide$D4_cei, 1000)
  expect_true(is.na(wide$D43_cei))
  expect_equal(wide$ratio_d43d44, 0.25)

  expect_true(file.exists(file.path(result$paths$data, "country_sna_candidate_patch.csv")))
  expect_true(file.exists(file.path(result$paths$data, "country_sna_candidate_patch.dta")))
  expect_true(startsWith(result$paths$root, file.path(root, "output", "experiments", "country_sna_include")))
  expect_false(dir.exists(file.path(root, "intermediary_data")))
})

test_that("country SNA include dry-run consumes explorer expectations", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("yaml")

  root <- tempfile("country-sna-include-expect-")
  dir.create(file.path(root, "input_data", "sna_country_data", "AAA"), recursive = TRUE)
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "2020")
  openxlsx::writeData(
    wb,
    "2020",
    data.frame(
      code = c("D.4", "D.43", "D.44", "B.5"),
      label = c("property", "reinvested", "other property", "balance"),
      households_r = c(100, NA, 25, 200),
      households_u = c(NA, 0, NA, NA)
    ),
    colNames = FALSE
  )
  openxlsx::saveWorkbook(wb, file.path(root, "input_data", "sna_country_data", "AAA", "fixture.xlsx"), overwrite = TRUE)

  contract <- country_sna_include_fixture_contract(root)
  contract_path <- file.path(root, "contract.yml")
  yaml::write_yaml(contract, contract_path)

  exploration_root <- file.path(root, "output", "experiments", "country_sna_explore")
  dir.create(file.path(exploration_root, "tables"), recursive = TRUE)
  utils::write.csv(
    data.frame(
      country = "AAA",
      year = 2020L,
      year_role = "overlap",
      extension_status = "overlap_only",
      structure_status = "structure_evidence_available",
      expected_action = "overlap_confirm",
      include_scope = TRUE,
      stringsAsFactors = FALSE
    ),
    file.path(exploration_root, "tables", "year_expectations.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    data.frame(
      country = "AAA",
      year = 2020L,
      year_role = "overlap",
      variable = c("D4_cei", "D43_cei"),
      variable_type = "primitive",
      account_code = c("D.4", "D.43"),
      sector_role = c("households_r", "households_u"),
      expected_action = "overlap_confirm",
      structure_status = "structure_evidence_available",
      expected_status = "expected_value",
      stringsAsFactors = FALSE
    ),
    file.path(exploration_root, "tables", "variable_expectations.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    data.frame(country = "AAA", structure_status = "structure_evidence_available", stringsAsFactors = FALSE),
    file.path(exploration_root, "tables", "structure_summary.csv"),
    row.names = FALSE
  )

  result <- run_country_sna_include(
    root = root,
    contract_path = contract_path,
    exploration_run = exploration_root,
    write_outputs = TRUE
  )
  detail <- result$outputs$include_detail
  expect_equal(detail$status[detail$variable == "D4_cei"], "ok_value_found")
  expect_equal(detail$status[detail$variable == "D43_cei"], "warning_missing_expected_value")
  expect_equal(result$outputs$include_summary$status, "check_following")
  expect_equal(country_sna_include_manifest_value(result$outputs$include_manifest, "dry_run"), "TRUE")
  expect_true(file.exists(file.path(result$paths$logs, "include_manifest.csv")))
})

test_that("country SNA include apply is retired", {
  skip_if_not_installed("yaml")

  root <- tempfile("country-sna-include-apply-guard-")
  dir.create(root, recursive = TRUE)
  contract <- country_sna_include_fixture_contract(root)
  contract_path <- file.path(root, "contract.yml")
  yaml::write_yaml(contract, contract_path)

  expect_error(
    run_country_sna_include(root = root, contract_path = contract_path, write_outputs = FALSE, apply = TRUE),
    "Use `--confirm`"
  )
})

test_that("country SNA include confirm backs up promoted sources and restore reverts them", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("yaml")

  root <- tempfile("country-sna-include-confirm-")
  dir.create(file.path(root, "config"), recursive = TRUE)
  dir.create(file.path(root, "input_data", "sna_country_data", "AAA"), recursive = TRUE)
  dir.create(file.path(root, "input_data", "_new", "sna", "AAA"), recursive = TRUE)
  dina_sources <- list(sources = list(list(
    id = "country-sna-aaa",
    family = "country_sna",
    country = "AAA",
    destination = "input_data/sna_country_data/AAA/{basename}"
  )))
  yaml::write_yaml(dina_sources, file.path(root, "config", "sources.yml"))

  write_fixture <- function(path, d4) {
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "2020")
    openxlsx::writeData(
      wb,
      "2020",
      data.frame(code = c("D.4", "D.43", "D.44", "B.5"), label = "x", households_r = c(d4, NA, 25, 200), households_u = c(NA, 20, NA, NA)),
      colNames = FALSE
    )
    openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  }
  canonical <- file.path(root, "input_data", "sna_country_data", "AAA", "fixture.xlsx")
  incoming <- file.path(root, "input_data", "_new", "sna", "AAA", "fixture.xlsx")
  write_fixture(canonical, 100)
  write_fixture(incoming, 110)

  contract <- country_sna_include_fixture_contract(root)
  contract_path <- file.path(root, "contract.yml")
  yaml::write_yaml(contract, contract_path)

  exploration_root <- file.path(root, "output", "experiments", "country_sna_explore")
  dir.create(file.path(exploration_root, "tables"), recursive = TRUE)
  utils::write.csv(
    data.frame(country = "AAA", source_set = "new", status = "matched", file = incoming, stringsAsFactors = FALSE),
    file.path(exploration_root, "tables", "source_inventory.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    data.frame(
      country = "AAA", year = 2020L, year_role = "overlap", variable = "D4_cei",
      variable_type = "primitive", account_code = "D.4", sector_role = "households_r",
      expected_action = "overlap_confirm", structure_status = "structure_evidence_available",
      expected_status = "expected_value", stringsAsFactors = FALSE
    ),
    file.path(exploration_root, "tables", "variable_expectations.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    data.frame(country = "AAA", structure_status = "structure_evidence_available", stringsAsFactors = FALSE),
    file.path(exploration_root, "tables", "structure_summary.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    data.frame(country = "AAA", year = 2020L, include_scope = TRUE, stringsAsFactors = FALSE),
    file.path(exploration_root, "tables", "year_expectations.csv"),
    row.names = FALSE
  )

  dry <- run_country_sna_include(root = root, contract_path = contract_path, exploration_run = exploration_root, write_outputs = TRUE)
  expect_equal(country_sna_include_manifest_value(dry$outputs$include_manifest, "status"), "all_good")
  expect_true(file.exists(file.path(dry$paths$tables, "staged_source_mappings.csv")))
  expect_true(startsWith(dry$paths$root, file.path(root, "output", "experiments", "country_sna_include", "runs")))
  expect_equal(openxlsx::read.xlsx(canonical, sheet = "2020", colNames = FALSE)[1, 3], 100)

  confirmed <- country_sna_include_confirm_sources(root, dry$paths$root)
  expect_equal(openxlsx::read.xlsx(canonical, sheet = "2020", colNames = FALSE)[1, 3], 110)
  expect_true(file.exists(file.path(confirmed$paths$logs, "backup_manifest.csv")))

  restored <- country_sna_include_restore_sources(root, confirmed$paths$root)
  expect_true(any(restored$outputs$restore_report$action == "restored"))
  expect_equal(openxlsx::read.xlsx(canonical, sheet = "2020", colNames = FALSE)[1, 3], 100)
})

test_that("country SNA include stages a release-named workbook at an explicit canonical destination", {
  skip_if_not_installed("yaml")
  root <- tempfile("country-sna-explicit-destination-")
  dir.create(file.path(root, "config"), recursive = TRUE)
  root <- normalizePath(root, mustWork = TRUE)
  incoming <- file.path(root, "input_data", "_new", "sna", "CHL", "CEI_anuario_2013-2024.xls")
  dir.create(dirname(incoming), recursive = TRUE)
  writeLines("release workbook", incoming)
  yaml::write_yaml(list(sources = list(list(
    id = "country-sna-chl", family = "country_sna", country = "CHL",
    destination = "input_data/sna_country_data/CHL/CEI_merged.xls"
  ))), file.path(root, "config", "sources.yml"))
  expectations <- list(root = file.path(root, "explore"))
  dir.create(file.path(expectations$root, "tables"), recursive = TRUE)
  utils::write.csv(data.frame(
    country = "CHL", source_set = "new", status = "matched", file = incoming,
    stringsAsFactors = FALSE
  ), file.path(expectations$root, "tables", "source_inventory.csv"), row.names = FALSE)

  staged <- country_sna_include_prepare_staged_sources(root, list(staged_repo = file.path(root, "stage")), expectations)
  expected <- file.path(root, "input_data", "sna_country_data", "CHL", "CEI_merged.xls")
  expect_equal(staged$mappings$status, "staged_new_source")
  expect_equal(staged$mappings$destination, expected)
  expect_true(file.exists(file.path(root, "stage", "input_data", "sna_country_data", "CHL", "CEI_merged.xls")))
})

test_that("country SNA include Brazil household override uses Familias year columns", {
  skip_if_not_installed("openxlsx")

  root <- tempfile("country-sna-exp-bra-")
  dir.create(file.path(root, "input_data", "sna_country_data", "BRA", "single_year"), recursive = TRUE)

  main <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(main, "CEI")
  openxlsx::writeData(
    main,
    "CEI",
    data.frame(
      code = c("D.4", "D.43", "D.44", "D.7", "D.75"),
      label = c("property", "reinvested", "other property", "transfers", "other current"),
      households_r = c(999, NA, 999, 999, 777),
      households_u = c(NA, 999, NA, NA, NA)
    ),
    colNames = FALSE
  )
  openxlsx::saveWorkbook(
    main,
    file.path(root, "input_data", "sna_country_data", "BRA", "single_year", "CEI2020.xlsx"),
    overwrite = TRUE
  )

  familias_codes <- c(
    "B.2", "B.3", "D.1", "D.11", "D.12", "D.4", "D.41", "D.42", "D.43", "D.44", "D.45",
    "USOS", "D.4", "D.41", "D.45", "B.5", NA, "II.2", "RECURSOS", "B.5", "D.62", "D.7",
    "USOS", "D.5", "D.61", "D.7", "B.6"
  )
  familias_values <- c(
    10, 20, 300, 250, 50, 100, NA, NA, 0, 25, NA,
    NA, 999, NA, NA, 200, NA, NA, NA, 200, 30, 50,
    NA, 8, 9, 80, 400
  )
  familias <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(familias, "Familias")
  openxlsx::writeData(
    familias,
    "Familias",
    data.frame(code = familias_codes, label = familias_codes, y2020 = familias_values),
    startRow = 30,
    startCol = 1,
    colNames = FALSE
  )
  openxlsx::saveWorkbook(
    familias,
    file.path(root, "input_data", "sna_country_data", "BRA", "contas_economicas_a_precos_correntes_2000a2021.xlsx"),
    overwrite = TRUE
  )

  contract <- country_sna_include_fixture_contract(root)
  contract$countries <- "BRA"
  contract$variables$primitives <- list(
    list(name = "D4_cei", account = "D.4", role = "households_r"),
    list(name = "D43_cei", account = "D.43", role = "households_u"),
    list(name = "D44_cei", account = "D.44", role = "households_r"),
    list(name = "D7_cei", account = "D.7", role = "households_r"),
    list(name = "D75_cei", account = "D.75", role = "households_r"),
    list(name = "B5g_cei", account = "B.5", role = "households_r")
  )
  contract$variables$derived <- list(
    list(name = "ratio_d44_d4", op = "ratio", numerator = "D44_cei", denominator = "D4_cei"),
    list(name = "ratio_d75_d7", op = "ratio", numerator = "D75_cei", denominator = "D7_cei")
  )
  contract$country_rules <- list(
    BRA = list(
      adapter = "workbook_table",
      magnitude = 10,
      source = list(type = "year_pattern", pattern = "input_data/sna_country_data/BRA/single_year/CEI{year}.xlsx"),
      primitive_overrides = list(list(
        id = "household_familias",
        type = "year_column_table",
        role = "households_r",
        variables = c("D4_cei", "D43_cei", "D44_cei", "D7_cei", "D75_cei", "B5g_cei"),
        source = list(type = "single_stem", stem = "input_data/sna_country_data/BRA/contas_economicas_a_precos_correntes_2000a2021"),
        sheet = list(type = "fixed", value = "Familias"),
        range = "A30:C56",
        columns = list(code = "A", code_long = "B"),
        year_columns = list(first_year = 2020L, last_year = 2020L, first_col = "C"),
        row_edits = list(drop_rows = 13L, recode_rows = list(list(excel_row = 55L, code = "D.7u")))
      )),
      layouts = list(list(
        years = list(min = 2020L),
        sheet = list(type = "fixed", value = "CEI"),
        range = "A1:D5",
        columns = list(code = "A", code_long = "B", households_r = "C", households_u = "D")
      ))
    )
  )

  extracted <- country_sna_include_extract_all(contract, root, years = 2020L)
  wide <- country_sna_include_wide(extracted$values_long, contract, years = 2020L)

  expect_equal(wide$D4_cei, 1000)
  expect_true(is.na(wide$D43_cei))
  expect_equal(wide$D44_cei, 250)
  expect_equal(wide$D7_cei, 500)
  expect_true(is.na(wide$D75_cei))
  expect_equal(wide$B5g_cei, 2000)
  expect_equal(wide$ratio_d44_d4, 0.25)
  expect_equal(wide$ratio_d75_d7, 0)
})

test_that("country SNA include Uruguay bundle preserves blank Excel columns without writing cei", {
  skip_if_not_installed("openxlsx")

  root <- tempfile("country-sna-exp-ury-")
  dir.create(file.path(root, "input_data", "sna_country_data", "URY"), recursive = TRUE)
  path <- file.path(root, "input_data", "sna_country_data", "URY", "CSI_3.2020_SC_S.1402.xlsx")

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Sheet1")
  openxlsx::writeData(
    wb,
    "Sheet1",
    data.frame(
      code = c("D.4", "B.5"),
      label = c("income por cobrar", "balance"),
      value = c(100, 250)
    ),
    startCol = 5,
    startRow = 1,
    colNames = FALSE
  )
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)

  contract <- country_sna_include_fixture_contract(root)
  contract$countries <- "URY"
  contract$variables$primitives <- list(
    list(name = "D4_cei", account = "D.4", role = "households_r"),
    list(name = "B5g_cei", account = "B.5", role = "households_r")
  )
  contract$variables$derived <- list()
  contract$country_rules <- list(
    URY = list(
      adapter = "sector_file_bundle",
      magnitude = 10,
      source = list(
        type = "sector_file_bundle",
        folder = "input_data/sna_country_data/URY",
        filename_template = "CSI_3.{year}_SC_{sector_code}.xlsx",
        sectors = list(households = "S.1402"),
        columns = list(code = "E", code_long = "F", value = "G"),
        directions = list(r = "por cobrar", u = "por pagar")
      )
    )
  )

  extracted <- country_sna_include_extract_all(contract, root, years = 2020L)
  wide <- country_sna_include_wide(extracted$values_long, contract, years = 2020L)

  expect_equal(wide$D4_cei, 1000)
  expect_equal(wide$B5g_cei, 2500)
  expect_false(file.exists(file.path(root, "input_data", "sna_country_data", "URY", "cei.xlsx")))
})

test_that("country SNA include exclusions can preserve Brazil pre-2010 benchmark missingness", {
  contract <- country_sna_include_fixture_contract(tempdir())
  contract$post_merge_exclusions <- list(list(
    country = "BRA",
    years = list(max = 2009L),
    variables = c("B2g_cei", "B3g_cei"),
    reason = "fixture exclusion"
  ))
  values <- data.frame(
    country = c("BRA", "BRA", "BRA"),
    year = c(2009L, 2009L, 2010L),
    variable = c("B2g_cei", "B3g_cei", "B2g_cei"),
    value_standardized = c(1, 2, 3),
    extract_status = "matched",
    contract_status = "contract_ok",
    warning = NA_character_,
    stringsAsFactors = FALSE
  )

  out <- country_sna_include_apply_exclusions(values, contract)
  expect_true(all(is.na(out$value_standardized[out$year == 2009L])))
  expect_equal(out$value_standardized[out$year == 2010L], 3)
  expect_true(all(out$contract_status[out$year == 2009L] == "contract_exclusion"))
})

test_that("country SNA include exclusions can apply without year bounds", {
  contract <- country_sna_include_fixture_contract(tempdir())
  contract$post_merge_exclusions <- list(list(
    country = "URY",
    variables = "GG_B5g_cei",
    reason = "fixture all-year exclusion"
  ))
  values <- data.frame(
    country = c("URY", "URY", "CRI"),
    year = c(2012L, 2021L, 2012L),
    variable = "GG_B5g_cei",
    value_standardized = c(1, 2, 3),
    extract_status = "matched",
    contract_status = "contract_ok",
    warning = NA_character_,
    stringsAsFactors = FALSE
  )

  out <- country_sna_include_apply_exclusions(values, contract)
  expect_true(all(is.na(out$value_standardized[out$country == "URY"])))
  expect_equal(out$value_standardized[out$country == "CRI"], 3)
})

test_that("country SNA include parity report compares candidate cells with benchmark DTA", {
  skip_if_not_installed("haven")

  root <- tempfile("country-sna-exp-parity-")
  dir.create(file.path(root, "intermediary_data", "national_accounts"), recursive = TRUE)
  benchmark_path <- file.path(root, "intermediary_data", "national_accounts", "UNDATA-WID-Merged.dta")
  haven::write_dta(
    data.frame(
      country = c("AAA", "AAA", "AAA"),
      year = c(2020L, 2020L, 2021L),
      D4_cei = c(100, 100, 100),
      D43_cei = c(NA_real_, NA_real_, NA_real_),
      ratio_d43d44 = c(0.5, 0.5, 0.7)
    ),
    benchmark_path
  )

  contract <- country_sna_include_fixture_contract(root)
  wide <- data.frame(
    country = c("AAA", "AAA"),
    year = c(2020L, 2021L),
    D4_cei = c(100, NA),
    D43_cei = c(NA, 1),
    ratio_d43d44 = c(0.5, 0.6),
    stringsAsFactors = FALSE
  )

  report <- country_sna_include_parity_report(wide, contract, root = root)
  expect_equal(report$parity_summary$status, "parity_failed")
  expect_equal(report$parity_summary$cells, 6L)
  expect_equal(report$parity_summary$same_within_tolerance, 2L)
  expect_equal(report$parity_summary$both_missing, 1L)
  expect_equal(report$parity_summary$numeric_differences, 1L)
  expect_equal(report$parity_summary$candidate_missing_benchmark_value, 1L)
  expect_equal(report$parity_summary$candidate_value_benchmark_missing, 1L)
  expect_true(any(report$parity_cell_detail$status == "numeric_difference"))
})

test_that("country SNA include duplicate code conflicts are not guessed", {
  rows <- data.frame(
    role = "households_r",
    code = c("D.4", "D.4"),
    code_long = c("one", "two"),
    value_raw = c("1", "2"),
    value = c(1, 2),
    row_in_range = 1:2,
    excel_row = 1:2,
    excel_col = "C",
    stringsAsFactors = FALSE
  )
  picked <- country_sna_include_pick_from_group(rows, "households_r", c("D.4"), zero_as_missing = TRUE)
  expect_equal(picked$status, "duplicate_conflict")

  rows$value <- c(1, 1)
  picked <- country_sna_include_pick_from_group(rows, "households_r", c("D.4"), zero_as_missing = TRUE)
  expect_equal(picked$status, "duplicate_identical")

  rows$value_raw <- c("0", "2")
  rows$value <- c(0, 2)
  picked <- country_sna_include_pick_from_group(rows, "households_r", c("D.4"), zero_as_missing = TRUE)
  expect_equal(picked$status, "matched")
  expect_equal(picked$value, 2)
})

test_that("family SNA review compares extracted values and includes a frozen candidate", {
  skip_if_not_installed("openxlsx")
  source_cli_for_tests()
  root <- mini_repo()
  file.copy(file.path(repo_root_for_tests, "code", "R", "source-diagnostics", c("country_sna_include.R", "country_sna_explorer.R")),
    file.path(root, "code", "R", "source-diagnostics"))
  dir.create(file.path(root, "code", "Stata"), recursive = TRUE, showWarnings = FALSE)
  file.copy(
    file.path(repo_root_for_tests, "code", "Stata", "05b-rescale-and-impute.do"),
    file.path(root, "code", "Stata", "05b-rescale-and-impute.do")
  )
  dina_write_yaml(list(countries = "AAA", years = list(first = 2020L, last = 2020L)), file.path(root, "config", "dina.yml"))
  dina_write_yaml(list(sources = list(list(id = "country-sna-aaa", family = "country_sna", country = "AAA",
    canonical = "input_data/sna_country_data/AAA/fixture.xlsx", inbox = "input_data/_new/sna/AAA/*.xlsx",
    destination = "input_data/sna_country_data/AAA/{basename}"))), file.path(root, "config", "sources.yml"))
  contract <- country_sna_include_fixture_contract(root)
  contract$country_rules$AAA$layouts[[1]]$range <- "A3:D6"
  dina_write_yaml(contract, file.path(root, "config", "country_sna_include.yml"))
  exploration <- dina_read_yaml(file.path(repo_root_for_tests, "config", "country_sna_explorer.yml"))
  exploration$economic_contract$countries <- "AAA"
  exploration$economic_contract$variables <- contract$variables
  exploration$economic_contract$code_aliases <- contract$code_aliases
  exploration$source_discovery$country_rules <- NULL
  exploration$source_discovery$countries <- list(AAA = list(adapter_family = "rectangular_workbook",
    old = list(type = "single_stem", stem = "input_data/sna_country_data/AAA/fixture"),
    new = list(type = "single_pattern", pattern = "input_data/_new/sna/AAA/*.xlsx")))
  dina_write_yaml(exploration, file.path(root, "config", "country_sna_explorer.yml"))
  write_book <- function(path, d4, broken = FALSE) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "2020")
    openxlsx::writeData(wb, "2020", data.frame(code = "codigo", label = "descripcion", r = "Hogares Recursos", u = "Hogares Empleos"), colNames = FALSE)
    openxlsx::writeData(wb, "2020", data.frame(code = if (broken) rep("unknown", 4) else c("D.4", "D.43", "D.44", "B.5"),
      label = "x", r = c(d4, NA, 25, 200), u = c(NA, 20, NA, NA)), startRow = 3, colNames = FALSE)
    openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  }
  current <- file.path(root, "input_data", "sna_country_data", "AAA", "fixture.xlsx")
  incoming <- file.path(root, "input_data", "_new", "sna", "AAA", "fixture.xlsx")
  write_book(current, 100)
  write_book(incoming, 110)
  original <- dina_hash_file(current)
  result <- run_dina_cli(c("sources", "explore", "sna"), root)
  expect_equal(result$status, 0L, info = result$output)
  record <- dina_review_read(root, "sna")
  expect_equal(record$status, "all_good", info = result$output)
  rows <- dina_review_table(record, "review_values")
  d4 <- rows[rows$measure == "D4_cei", ]
  expect_equal(d4$old_value, 1000)
  expect_equal(d4$new_value, 1100)
  expect_equal(d4$result, "revised")
  audit <- dina_review_table(record, "value-changes-audit")
  expect_true(nrow(audit) > 0L)
  expect_true(all(audit$result != "unchanged"))
  expect_true(all(c("source_file", "sheet", "extract_status") %in% names(audit)))
  expect_true(file.exists(file.path(record$run, "tables", "value_changes_audit.csv")))
  expect_equal(run_dina_cli(c("sources", "table", "sna", "value-changes-audit"), root)$status, 0L)
  expect_equal(dina_hash_file(current), original)
  plan <- dina_review_table(record, "review_files")
  expect_true(all(grepl("reviewed_sources", plan$from_rel, fixed = TRUE)))
  expect_equal(run_dina_cli(c("sources", "include", "sna", "--confirm"), root)$status, 0L)
  expect_equal(dina_hash_file(current), dina_hash_file(incoming))
  backup <- dina_review_read(root, "sna")$confirmation
  expect_equal(run_dina_cli(c("sources", "include", "sna", "--restore", backup), root)$status, 0L)
  expect_equal(dina_hash_file(current), original)
  write_book(incoming, 110, broken = TRUE)
  broken <- run_dina_cli(c("sources", "explore", "sna"), root)
  expect_equal(broken$status, 0L, info = broken$output)
  # This fixture only covers D4-family values, which are intentionally outside
  # the direct 05b review focus. They remain in the audit, not an inclusion block.
  expect_equal(dina_review_read(root, "sna")$status, "all_good")
  expect_match(broken$output, "3. Problems", fixed = TRUE)
  expect_equal(run_dina_cli(c("sources", "include", "sna", "--confirm"), root)$status, 0L)
})
