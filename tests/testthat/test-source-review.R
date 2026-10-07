review_fixture_surveys <- function(root) {
  dina_write_yaml(list(sources = list(list(id = "surveys-cepal", family = "surveys", country = "ARG", method = "manual",
    canonical = "input_data/surveys_CEPAL/*/*.dta", inbox = "input_data/_new/surveys/*"))), file.path(root, "config", "sources.yml"))
  current <- file.path(root, "input_data", "surveys_CEPAL", "ARG", "ARG_2000N.dta")
  incoming <- file.path(root, "input_data", "_new", "surveys", "ARG_2000N.dta")
  dir.create(dirname(current), recursive = TRUE)
  dir.create(dirname(incoming), recursive = TRUE)
  data <- data.frame(`_fep` = c(10, 20), edad = c(30, 10), id_hogar = c(1, 2), check.names = FALSE)
  haven::write_dta(data, current)
  data[["_fep"]] <- c(20, 30)
  haven::write_dta(data, incoming)
  population <- file.path(root, "input_data", "wid", "population_total_adult_npopul.dta")
  dir.create(dirname(population), recursive = TRUE)
  haven::write_dta(data.frame(country = "Argentina", year = 2000L, totalpop = 1000, adultpop = 800), population)
  list(current = current, incoming = incoming)
}

test_that("keyed comparisons detect offsetting revisions and preserve observation identity", {
  source_cli_for_tests()
  old <- data.frame(country = "ARG", year = 2000:2001, variable = "income", value = c(100, 200))
  new <- old
  new$value <- c(110, 190)
  rows <- dina_review_compare(old, new, c("country", "year", "variable"), "value")
  expect_equal(rows$result, c("revised", "revised"))
  expect_equal(rows$difference, c(10, -10))
  expect_equal(rows$variable, rep("income", 2))
  expect_equal(rows$measure, rep("value", 2))
  expect_error(dina_review_compare(old, rbind(new, new[1, ]), c("country", "year"), "value"), "duplicated")
  new$year[1] <- NA
  expect_error(dina_review_compare(old, new, c("country", "year"), "value"), "missing")
})

test_that("comparison distinguishes new observations, missing values and invalid numbers", {
  source_cli_for_tests()
  old <- data.frame(year = 2000:2002, value = c(0, 200, 300))
  new <- data.frame(year = 2000:2003, value = c(10, NA, Inf, 400))
  rows <- dina_review_compare(old, new, "year", "value")
  expect_equal(rows$result, c("revised", "value missing", "nonfinite value", "new observation"))
  expect_true(is.na(rows$percent[1]))
  expect_equal(dina_review_compare(old, old, "year", "value")$result, rep("unchanged", 3))
  expect_error(dina_review_compare(old, data.frame(year = 2000L, value = "bad"), "year", "value"), "nonnumeric")
})

test_that("SNA comparison retains an accepted year even when stale discovery calls it an extension", {
  source_cli_for_tests()
  canonical <- data.frame(country = "PER", year = 2024L, x_cei = 10)
  long <- data.frame(country = "PER", year = 2024L, variable = "x_cei", value_standardized = 10,
    extract_status = "matched", source_file = "canonical.xlsx", sheet = "CEI_2024")
  engine <- list(
    country_sna_include_extract_all = function(...) list(
      values_long = long,
      source_matches = data.frame(country = "PER", year = 2024L, source_status = "matched")
    ),
    country_sna_include_wide = function(...) canonical,
    country_sna_include_float_tolerance = function(...) 0
  )
  prepared <- list(
    contract = list(variables = list(primitives = list(list(name = "x_cei")), derived = list())), years = 2024L,
    outputs = list(values_wide = canonical, include_detail = data.frame(), values_long = long)
  )
  # This reproduces a stale discovery rule claiming the year is new.  It must
  # not override a successful canonical extraction.
  explored <- list(outputs = list(extension_summary = data.frame(country = "PER", extension_years = "2024")))
  compared <- dina_review_values(tempdir(), "sna", engine, explored, prepared)$rows
  expect_equal(compared$result, "unchanged")
})

test_that("survey direct-input overview is a compact horizontal table", {
  source_cli_for_tests()
  summary <- data.frame(
    country = c("ARG", "BOL"), years = c("2000-2014,2016-2024", "2000-2024"),
    files = c(24L, 25L), inputs_checked = c(22L, 20L),
    missing = c(0L, 2L), unresolved = c(0L, 0L), stringsAsFactors = FALSE
  )
  table <- dina_review_survey_input_table(summary, expected_inputs = 22L)
  expect_equal(names(table), c("country", "years", "files", "inputs", "status"))
  expect_equal(table$inputs, c("22/22", "20/22"))
  expect_equal(table$status, c("ready", "2 missing"))
  shown <- paste(capture.output(dina_print_data_frame_compact(table, limit = nrow(table))), collapse = "\n")
  expect_match(shown, "country")
  expect_match(shown, "ARG")
  expect_false(grepl("country:", shown, fixed = TRUE))
})

test_that("survey review distinguishes file-level changes from input availability", {
  source_cli_for_tests()
  comparison <- data.frame(
    country = c("ARG", "ARG", "BRA"), year = c(2022L, 2023L, 2023L),
    comparison_status = c("retro_overlap_changed", "retro_overlap_same", "retro_overlap_changed"),
    sum_fep_diff = c(-10, 0, 25), canonical_sum_fep = c(1000, 1000, 500),
    adult_share_diff = c(0.25, 0, -0.1),
    incoming_row_count = c(99L, 100L, 50L), canonical_row_count = c(100L, 100L, 50L),
    added_columns = c("new_field", "", ""), dropped_columns = c("", "", "old_field"),
    stringsAsFactors = FALSE
  )
  table <- dina_review_survey_source_change_table(comparison, list(countries = c("ARG", "BRA"), years = 2022:2023))
  expect_equal(table$compared, c(2L, 1L))
  expect_equal(table$changed, c(1L, 1L))
  expect_equal(table$`row changes`, c(1L, 0L))
  expect_equal(table$`layout changes`, c(1L, 1L))
  expect_equal(table$`weight Δ`, c("-1.0%", "+5.0%"))
  expect_equal(table$`adult share Δ`, c("+0.2 pp", "-0.1 pp"))
})

test_that("survey population dependency is one actionable ARG blocker", {
  source_cli_for_tests()
  problems <- data.frame(
    country = c("ARG", "ARG", "ARG"), year = c(2000L, 2001L, NA_integer_),
    severity = "Blocker",
    status = c("missing_arg_population_target", "missing_arg_population_target", "blocked_no_observed_surveys"),
    reason = c("missing arg population target", "missing arg population target", "blocked no observed surveys"),
    detail = c(
      "Argentina totalpop target is missing from input_data/wid/population_total_adult_npopul.dta.",
      "Argentina totalpop target is missing from input_data/wid/population_total_adult_npopul.dta.",
      ""
    ),
    stringsAsFactors = FALSE
  )
  triage <- dina_review_survey_problem_triage(problems)
  expect_equal(nrow(triage), 1L)
  expect_equal(triage$issue, "Population benchmark is not accepted")
  expect_equal(triage$period, "2000–2001 (2 years)")
  expect_equal(triage$checks, 2L)
  output <- paste(capture.output(dina_review_print_problem_triage(problems, "surveys", triage = triage, root = tempdir())), collapse = "\n")
  expect_match(output, "survey files and direct inputs are valid")
  expect_match(output, "Population benchmark")
})

test_that("source family navigation follows the advisory dependency order", {
  source_cli_for_tests()
  expect_equal(dina_review_source_family_order(), c("wid", "surveys", "admin", "sna"))
  expect_equal(dina_review_source_prerequisites("surveys"), c(wid = "WID population"))
  expect_equal(dina_review_source_prerequisites("admin"), c(surveys = "Household surveys"))
  expect_length(dina_review_source_prerequisites("sna"), 0L)
  root <- mini_repo()
  rows <- dina_review_source_prerequisite_rows(root, "surveys")
  expect_equal(rows$required, "WID population")
  expect_true(nzchar(rows$status))
  dina_review_save(list(
    family = "wid", status = "included", run = tempdir(), watch = dina_review_watch(character()), included_at = "2026-09-21"
  ), root)
  expect_match(dina_review_source_prerequisite_rows(root, "surveys")$status, "Included")
  output <- paste(capture.output(dina_review_print_source_prerequisites(root, "surveys")), collapse = "\n")
  expect_match(output, "Before including Household surveys")
  expect_match(output, "You can explore now")
})

test_that("source-family prerequisite status remains visible inside its menu", {
  source_cli_for_tests()
  root <- mini_repo()
  input <- textConnection("q\n")
  on.exit(close(input), add = TRUE)
  output <- paste(capture.output(dina_review_family_menu(root, "surveys", input, TRUE)), collapse = "\n")
  expect_match(output, "Before including Household surveys")
  expect_match(output, "WID population")
  expect_match(output, "Household surveys")
})

test_that("artifact source reviews collapse whole-year additions and retain a row-level audit", {
  source_cli_for_tests()
  values <- data.frame(
    country = rep("HND", 6L), year = rep(2013:2015, each = 2L),
    source_id = "population", measure = rep(c("totalpop", "adultpop"), 3L),
    result = "new observation", percent = NA_real_, stringsAsFactors = FALSE
  )
  whole <- dina_review_whole_year_additions(values)
  compact <- dina_review_compact_whole_year_additions(whole)
  expect_equal(nrow(compact), 1L)
  expect_equal(compact$years, "2013-2015 (3y)")
  expect_equal(compact[["new values"]], 6L)
  output <- paste(capture.output(dina_review_print_artifact_value_changes(
    list(run = tempdir()), values, "wid"
  )), collapse = "\n")
  expect_match(output, "Whole-year additions are already summarized in Coverage")
  expect_false(grepl("country: HND", output, fixed = TRUE))
  expect_match(output, "Complete row-level audit")
})

test_that("WID reviewer coverage is configured-scope and country-level", {
  source_cli_for_tests()
  coverage <- data.frame(
    country = c("ARG", "ARG", "HND"), source_id = c("population", "wid-macro", "population"),
    old_years = "", new_years = c("1990,2000,2001", "2000,2001", "2000,2001"),
    extension_years = c("1990,2000,2001", "2000,2001", "2000,2001"),
    missing_in_new_years = "", stringsAsFactors = FALSE
  )
  scoped <- dina_review_filter_display_scope(coverage, list(countries = "ARG", years = 2000:2024))
  summary <- dina_review_wid_compact_coverage(scoped, list(years = 2000:2024))
  expect_equal(nrow(summary), 1L)
  expect_equal(summary$country, "ARG")
  expect_equal(summary$change, "+2000-2001 (2y)")
  expect_equal(summary$artifacts, "2 benchmark checks")
})

test_that("SNA review presentation is limited to configured countries and years", {
  source_cli_for_tests()
  scope <- list(countries = c("COL", "ARG"), years = 2023:2024)
  values <- data.frame(
    country = c("ARG", "ARG", "COL", "CHL"), year = c(2022L, 2024L, 2023L, 2024L),
    measure = "FC_B5g_cei", result = "revised", stringsAsFactors = FALSE
  )
  filtered <- dina_review_sna_filter_scope(values, scope)
  expect_equal(filtered$country, c("ARG", "COL"))
  expect_equal(filtered$year, c(2024L, 2023L))
  problems <- data.frame(
    country = c("ARG", "CHL", NA_character_), year = c(2024L, 2024L, NA_integer_),
    severity = "Blocker", status = "blocked", stringsAsFactors = FALSE
  )
  filtered_problems <- dina_review_sna_filter_scope(problems, scope)
  expect_equal(filtered_problems$country, c("ARG", NA_character_))
  coverage <- dina_review_sna_scope_coverage(data.frame(
    country = "ARG", old_years = "2022,2023,2024", new_years = "2022,2023,2024",
    new_country = FALSE, extension_years = "", overlap_years = "2022,2023,2024",
    missing_in_new_years = "", stringsAsFactors = FALSE
  ), scope)
  expect_equal(coverage$country, c("COL", "ARG"))
  expect_true(coverage$scope_empty[[1L]])
  compact <- dina_review_compact_coverage(coverage)
  expect_equal(compact$coverage[[1L]], "not assessed")
  expect_equal(compact$change[[1L]], "no SNA evidence")
})

test_that("SNA contract exclusions are expected absences and do not block the focused review", {
  source_cli_for_tests()
  rows <- data.frame(
    country = "URY", year = 2020L, measure = "NFC_B5g_cei",
    old_value = NA_real_, new_value = NA_real_, result = "absence unexplained",
    stringsAsFactors = FALSE
  )
  extracted <- data.frame(
    country = "URY", year = 2020L, variable = "NFC_B5g_cei",
    extract_status = "excluded_by_contract", source_file = "fixture.xlsx", sheet = "CEI_2020",
    stringsAsFactors = FALSE
  )
  classified <- dina_review_sna_absences(rows, old = extracted, new = extracted)
  expect_equal(classified$result, "expected absence")
  expect_match(classified$absence_reason, "Excluded on both sides")
  expect_equal(dina_review_sna_focus_status(classified, data.frame(), "check_following"), "all_good")
  classified$result <- "expected value missing"
  expect_equal(dina_review_sna_focus_status(classified, data.frame(), "all_good"), "check_following")
})

test_that("SNA review display summarizes changes and stores the raw audit separately", {
  source_cli_for_tests()
  rows <- data.frame(
    country = rep("COL", 7L), year = c(2023L, 2021L, 2022L, 2020L, 2018L, 2024L, 2019L),
    measure = c("D11_cei", "FC_r_D4_cei", "FC_r_D4_cei", "TOT_B5g_cei", "D0_cei", "D99_cei", "D44_cei"),
    old_value = c(4.39238e14, 1.35243e14, 100, 200, 0, NA, 10),
    new_value = c(4.38299e14, 1.28062e14, 101, 196, 1, 25, NA),
    difference = c(-9.39e11, -7.181e12, 1, -4, 1, NA, NA),
    percent = c(-0.213779, -5.3097, 1, -2, NA, NA, NA),
    result = c("revised", "revised", "revised", "revised", "revised", "value added", "value missing"),
    stringsAsFactors = FALSE
  )
  shown <- paste(capture.output(dina_review_show_rows(rows)), collapse = "\n")
  expect_match(shown, "439,238B")
  expect_match(shown, "-0.21%")
  expect_false(grepl("e\\+", shown))

  coverage <- dina_review_compact_coverage(data.frame(
    country = "COL", old_years = "2000,2001,2002,2003", new_years = "2000,2001,2002,2003,2004",
    extension_years = "2004", missing_in_new_years = "", stringsAsFactors = FALSE
  ))
  expect_equal(names(coverage), c("country", "coverage", "change"))
  expect_false(grepl("→", coverage$coverage, fixed = TRUE))
  expect_equal(coverage$change, "+2004")

  record <- list(family = "sna", status = "all_good", comparison_note = "fixture", watch = list(), run = tempdir())
  old_table <- dina_review_table
  on.exit(assign("dina_review_table", old_table, envir = environment(dina_review_print)), add = TRUE)
  assign("dina_review_table", function(record, table) {
    if (identical(table, "review_values")) return(rows)
    if (identical(table, "review_coverage")) return(data.frame())
    data.frame()
  }, envir = environment(dina_review_print))
  output <- paste(capture.output(dina_review_print(record)), collapse = "\n")
  expect_match(output, "Direct pipeline inputs \\(05b-rescale-and-impute\\)")
  expect_match(output, "Revision tables show \\|mean change\\| ≥1%")
  expect_match(output, "Revisions")
  expect_match(output, "Property-income receipts")
  expect_match(output, "National income")
  expect_match(output, "category")
  expect_match(output, "mean change")
  expect_match(output, "range")
  expect_match(output, "FC_r_D4")
  expect_match(output, "-5.31%")
  expect_match(output, "TOT_B5g")
  expect_false(grepl("FC_r_D4_cei", output, fixed = TRUE))
  expect_false(grepl("D11_cei", output, fixed = TRUE))
  variable_summary <- dina_review_sna_variable_summary(data.frame(
    measure = c("D4_cei", "D4_cei"), year = c(2021L, 2023L), percent = c(-5, 1)
  ), revisions = TRUE)
  expect_equal(variable_summary$years, "2021–2023 (2 years)")
  expect_equal(variable_summary$`mean change`, "-2%")
  expect_equal(variable_summary$`max change`, "1%")
  focus <- dina_review_sna_review_focus(repo_root_for_tests)
  expect_setequal(focus$entries$measure, dina_review_sna_pipeline_inputs(file.path(repo_root_for_tests, "code", "Stata", "05b-rescale-and-impute.do")))
  expect_equal(dina_review_sna_focus_rows(data.frame(measure = c("D11_cei", "FC_B5g_cei")), focus)$measure, "FC_B5g_cei")
  filtered_problems <- dina_review_sna_focus_problems(data.frame(
    variable = c("D11_cei", "FC_B5g_cei", NA_character_), severity = c("Warning", "Warning", "Blocker")
  ), focus)
  expect_equal(filtered_problems$variable, c("FC_B5g_cei", NA_character_))
  compact <- dina_review_sna_focused_revision_summary(data.frame(
    measure = c("FC_r_D4_cei", "FC_r_D4_cei", "TOT_B5g_cei"), year = c(2021L, 2022L, 2023L), percent = c(2, 2, -3)
  ), focus)
  expect_equal(compact$range, c("", ""))
  expect_equal(compact$variable, c("FC_r_D4", "TOT_B5g"))
  compact_categories <- dina_review_sna_compact_category_cells(dina_review_sna_focused_revision_summary(data.frame(
    measure = c("NFC_B5g_cei", "FC_B5g_cei", "FC_r_D4_cei"), year = 2023L, percent = c(2, 3, -2)
  ), focus))
  expect_equal(compact_categories$category, c("Corporate profits", "", "Property-income receipts"))
  compact_revisions <- dina_review_sna_compact_revision_cells(dina_review_sna_revision_summary(data.frame(
    country = c("COL", "COL", "ECU"), measure = c("NFC_B5g_cei", "FC_B5g_cei", "FC_r_D4_cei"),
    year = 2023L, percent = c(2, 3, -2)
  ), focus))
  expect_equal(compact_revisions$country, c("COL", "", "ECU"))
  expect_equal(compact_revisions$category, c("Corporate profits", "", "Property-income receipts"))
  unresolved <- dina_review_sna_unresolved_additions(data.frame(
    country = c("CHL", "CHL", "CHL", "CHL", "ECU", "ECU", "ECU"),
    year = c(2023L, 2023L, 2024L, 2024L, 2024L, 2024L, 2024L),
    variable = c("D11_cei", "B3g_cei", "D11_cei", "B3g_cei", "B2g_cei", "B3g_cei", "ratio_d44_d4"),
    expected_status = "expected_value",
    extract_status = c("no_code_match", "non_numeric", "no_code_match", "non_numeric", "duplicate_conflict", "duplicate_conflict", "ratio_missing_input"),
    stringsAsFactors = FALSE
  ), data.frame(country = c("CHL", "CHL", "ECU"), year = c(2023L, 2024L, 2024L)))
  expect_equal(unresolved$years[unresolved$country == "CHL" & unresolved$issue == "Account not published separately"], "2023–2024 (2 years)")
  expect_equal(unresolved$values[unresolved$country == "CHL" & unresolved$issue == "Source cell is blank"], 2L)
  expect_equal(unresolved$values[unresolved$country == "ECU"], 2L)
  expect_false(any(grepl("ratio", unresolved$inputs, fixed = TRUE)))
  expect_equal(dina_review_sna_compact_country_column(unresolved)$country, c("CHL", "", "ECU"))
  material <- dina_review_sna_material_revision_variables(data.frame(
    measure = c("quiet", "outlier", "outlier", "mean_change"),
    percent = c(0.5, 6, -5, 1.2)
  ))
  expect_equal(sort(material), c("mean_change", "outlier"))
  whole_added <- dina_review_whole_year_additions(data.frame(
    country = c("COL", "COL", "COL"), year = c(2024L, 2024L, 2023L),
    result = c("new observation", "new observation", "unchanged")
  ))
  expect_equal(whole_added$country, "COL")
  expect_equal(whole_added$year, 2024L)
  expect_equal(whole_added$change, "whole year added")
  expect_equal(whole_added$observations, 2L)
  expect_match(output, "Complete audit")
  expect_match(output, "value_changes_audit.csv")
  expect_match(output, "value_changes_audit")
  expect_false(grepl("Accepted: 439,238B", output, fixed = TRUE))
  expect_false(grepl("D4_cei", output, fixed = TRUE))
  audit <- dina_review_sna_audit_rows(rows)
  expect_equal(nrow(audit), nrow(rows))
  expect_true(all(audit$result != "unchanged"))
  if (requireNamespace("cli", quietly = TRUE)) expect_equal(cli::ansi_strip(dina_cli_emphasis("Heading")), "Heading")
})

test_that("SNA inclusion rejects a review saved for another effective scope", {
  source_cli_for_tests()
  root <- mini_repo()
  record <- list(
    family = "sna",
    scope = list(
      config_source = "active update fixture (effective configuration)",
      years = 2000:2024,
      effective_config_hash = "different"
    )
  )
  expect_error(dina_review_assert_sna_scope(record, root), "saved for")
})

test_that("problem presentation aggregates checks into reviewer triage", {
  source_cli_for_tests()
  problems <- data.frame(
    country = c("MEX", "MEX", "CHL", "CHL", "CHL"),
    year = c(NA, NA, 2023L, 2024L, 2024L),
    variable = c(NA, NA, "D11_cei", "D752_cei", "FC_r_D43_cei"),
    severity = c("Blocker", "Blocker", "Warning", "Warning", "Warning"),
    status = c("ambiguous_destination", "ambiguous_destination", rep("warning_missing_expected_value", 3L)),
    extract_status = c(NA, NA, "no_code_match", "no_code_match", "zero_treated_missing"),
    stringsAsFactors = FALSE
  )
  triage <- dina_review_problem_triage(problems)
  expect_equal(nrow(triage), 3L)
  expect_equal(triage$checks[triage$country == "MEX"], 2L)
  expect_equal(triage$period[triage$country == "CHL" & triage$issue == "Account not published separately"], "2023–2024 (2 years)")
  output <- paste(capture.output(dina_review_print_problem_triage(problems, "sna")), collapse = "\n")
  expect_match(output, "Blocks inclusion")
  expect_match(output, "Needs a decision")
  expect_match(output, "Recorded source limitations")
  expect_match(output, "dina sources table sna problems", fixed = TRUE)
  expect_false(grepl("D11_cei|D752_cei|FC_r_D43_cei", output))
})

test_that("Admin trust configuration is not presented as a failed source review", {
  source_cli_for_tests()
  problems <- data.frame(
    country = "Family", year = NA_integer_, severity = "Blocker",
    status = "trust_region_confirmation_required",
    stringsAsFactors = FALSE
  )
  triage <- dina_review_problem_triage(problems)
  expect_equal(triage$issue, "Complete Admin configuration proposal required")
  expect_match(triage$next_step, "Review and apply the complete proposal")
  expect_false(grepl("could not complete", triage$issue, ignore.case = TRUE))
})

test_that("admin dependency blocks use the shared review language and preserve their next steps", {
  source_cli_for_tests()
  problems <- data.frame(
    country = c("CHL", "CHL", "BRA", "COL"),
    year = NA_integer_, severity = "Blocker",
    status = c("missing_static_dependency", "missing_static_dependency", "blocked", "blocked"),
    reason = c("missing static dependency", "missing static dependency", "source_checks_blocked", "stata_not_configured"),
    next_command = c("dina sources explore surveys", "dina sources explore wid", "", ""),
    stringsAsFactors = FALSE
  )
  triage <- dina_review_problem_triage(problems)
  expect_true(all(triage$section == "Blocks inclusion"))
  expect_true(all(c("Required source data is not accepted", "Required checks have not run", "Stata is not configured") %in% triage$issue))
  expect_true(any(grepl("dina sources explore surveys", triage$next_step, fixed = TRUE)))
  expect_true(any(grepl("dina sources explore wid", triage$next_step, fixed = TRUE)))
})

test_that("admin review exposes non-PIT inputs separately from harmonized PIT values", {
  source_cli_for_tests()
  contract <- list(cleaners = list(review_other_inputs = list(
    `bra-minwage` = list(label = "Minimum wage"),
    `bra-admin-thresholds` = list(label = "Tax and social-security thresholds")
  )))
  summary <- data.frame(
    country = c("BRA", "BRA"), dependency_id = c("bra-minwage", "bra-admin-thresholds"),
    incoming_rel = c("input_data/_new/admin/BRA/wiki_minwage.csv", "input_data/_new/admin/BRA/admin_thresholds.csv"),
    current_rel = c("input_data/admin_data/BRA/downloads/wiki_minwage.csv", "input_data/admin_data/BRA/downloads/admin_thresholds.csv"),
    required_years = c("2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023,2024", "2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023,2024"),
    changed_overlap_years = c("", "2023"), severity = c("info", "blocked"),
    status = c("aux_validated_append_only", "blocked_aux_overlap_changed"), stringsAsFactors = FALSE
  )
  inputs <- dina_review_admin_other_inputs(summary, contract)
  expect_equal(names(inputs), c("country", "input", "incoming", "coverage", "changes", "status"))
  expect_equal(inputs$incoming, c("provided", "provided"))
  expect_equal(inputs$changes, c("unchanged in overlap", "changed 2023"))
  expect_equal(inputs$status, c("ready", "blocked"))
  root <- tempfile("admin-other-inputs-")
  dir.create(file.path(root, "tables"), recursive = TRUE)
  utils::write.csv(inputs, file.path(root, "tables", "other_admin_inputs.csv"), row.names = FALSE)
  output <- paste(capture.output(dina_review_print_admin_other_inputs(list(run = root))), collapse = "\n")
  expect_match(output, "Other administrative inputs")
  expect_match(output, "Minimum wage")
  expect_match(output, "changed 2023", fixed = TRUE)
})

test_that("a fresh admin explore resets a prior inclusion to its original backup once", {
  source_cli_for_tests()
  root <- tempfile("admin-review-reset-")
  confirm <- file.path(root, "output", "experiments", "admin_pit_include", "confirms", "confirm-a")
  dir.create(file.path(confirm, "logs"), recursive = TRUE)
  utils::write.csv(data.frame(key = "status", value = "confirmed"), file.path(confirm, "logs", "confirm_manifest.csv"), row.names = FALSE)
  called <- NULL
  engine <- list(admin_pit_include_restore_sources = function(root, confirm_run) {
    called <<- confirm_run
    list(
      paths = list(restore_report = file.path(confirm_run, "tables", "restore_report.csv")),
      outputs = list(restore_report = data.frame(restore_status = c("staged", "removed_promoted_destination")))
    )
  })
  dina_review_reset_before_explore(root, "admin", engine)
  expect_equal(called, confirm)
  dir.create(file.path(confirm, "tables"), recursive = TRUE)
  utils::write.csv(data.frame(restore_status = "staged"), file.path(confirm, "tables", "restore_report.csv"), row.names = FALSE)
  expect_null(dina_review_family_confirmation_to_reset(root, "admin"))
})

test_that("explore is nonmutating and include accepts and restores the exact survey family", {
  skip_if_not_installed("haven")
  source_cli_for_tests()
  root <- mini_repo()
  files <- review_fixture_surveys(root)
  original <- dina_hash_file(files$current)
  result <- run_dina_cli(c("sources", "explore", "surveys"), root)
  expect_equal(result$status, 0L)
  expect_match(result$output, "1\\. Coverage")
  expect_match(result$output, "2\\. Value changes")
  expect_match(result$output, "3\\. Problems")
  expect_match(result$output, "Survey-file changes")
  expect_false(grepl("dry-run", result$output, fixed = TRUE))
  expect_equal(dina_hash_file(files$current), original)
  expect_false(dir.exists(file.path(root, "output", "run_logs")))
  expect_equal(dina_review_read(root, "surveys")$status, "all_good")
  before <- dina_review_recommendation(root)
  expect_equal(before$proposal$command, "dina sources include surveys")
  declined <- run_dina_cli(c("sources", "include", "surveys"), root)
  expect_equal(declined$status, 0L)
  expect_match(declined$output, "No files changed")
  expect_equal(dina_hash_file(files$current), original)
  accepted <- run_dina_cli(c("sources", "include", "surveys", "--confirm"), root)
  expect_equal(accepted$status, 0L)
  expect_match(accepted$output, "Family included")
  expect_equal(dina_hash_file(files$current), dina_hash_file(files$incoming))
  expect_equal(dina_review_read(root, "surveys")$status, "included")
  expect_true(is.null(dina_review_recommendation(root)))
  confirmation <- dina_review_read(root, "surveys")$confirmation
  restored <- run_dina_cli(c("sources", "include", "surveys", "--restore", confirmation), root)
  expect_equal(restored$status, 0L)
  expect_equal(dina_hash_file(files$current), original)
  expect_equal(dina_review_read(root, "surveys")$status, "restored")
})

test_that("changed baseline, configuration or candidate invalidates a saved review", {
  skip_if_not_installed("haven")
  source_cli_for_tests()
  root <- mini_repo()
  files <- review_fixture_surveys(root)
  expect_equal(run_dina_cli(c("sources", "explore", "surveys"), root)$status, 0L)
  record <- dina_review_read(root, "surveys")
  original <- readBin(files$current, "raw", n = file.info(files$current)$size)
  writeLines("changed baseline", files$current)
  expect_error(dina_review_include(root, "surveys", list(confirm = TRUE)), "changed")
  writeBin(original, files$current)
  config <- file.path(root, "config", "dina.yml")
  original_config <- readLines(config)
  writeLines(c(original_config, "# changed"), config)
  expect_error(dina_review_include(root, "surveys", list(confirm = TRUE)), "changed")
  writeLines(original_config, config)
  plan <- dina_review_table(record, "review_files")
  unlink(plan$from_rel[[1]])
  expect_error(dina_review_include(root, "surveys", list(confirm = TRUE)), "changed")
  expect_equal(dina_hash_file(files$current), digest::digest(original, algo = "sha256", serialize = FALSE))
})

test_that("failed exploration invalidates an earlier usable review", {
  skip_if_not_installed("haven")
  source_cli_for_tests()
  root <- mini_repo()
  files <- review_fixture_surveys(root)
  expect_equal(run_dina_cli(c("sources", "explore", "surveys"), root)$status, 0L)
  unlink(file.path(root, "config", "survey_population_include.yml"))
  expect_error(dina_review_explore(root, "surveys"))
  expect_equal(dina_review_read(root, "surveys")$status, "incomplete")
  expect_error(dina_review_include(root, "surveys", list(confirm = TRUE)), "Explore this family first")
})

test_that("menu chooses a family explicitly and includes only after one confirmation", {
  source_cli_for_tests()
  entries <- dina_command_flatten(dina_command_catalog())
  entry <- Filter(function(x) identical(x$key, "sources-explore-type"), entries)[[1]]
  input <- textConnection("3\n")
  on.exit(close(input), add = TRUE)
  args <- dina_command_collect_args(entry, input = input, is_terminal = TRUE)
  expect_equal(args$args, c("sources", "explore", "surveys"))
  expect_error(dina_review_choose_family(is_terminal = FALSE), "Choose a family")
  skip_if_not_installed("haven")
  root <- mini_repo()
  files <- review_fixture_surveys(root)
  expect_equal(run_dina_cli(c("sources", "explore", "surveys"), root)$status, 0L)
  input2 <- textConnection("1\n")
  on.exit(close(input2), add = TRUE)
  # Yes is the first numbered confirmation action; the default remains No.
  output <- capture.output(dina_review_include(root, "surveys", input = input2, is_terminal = TRUE))
  expect_equal(sum(grepl("Accept this reviewed family", output, fixed = TRUE)), 1L)
  expect_equal(dina_review_read(root, "surveys")$status, "included")
})

test_that("family menu and typed Explore save the same review and cancellation preserves sources", {
  source_cli_for_tests()
  root <- mini_repo()
  files <- review_fixture_surveys(root)
  original <- dina_hash_file(files$current)
  expect_equal(run_dina_cli(c("sources", "explore", "surveys"), root)$status, 0L)
  typed <- dina_review_read(root, "surveys")
  rows <- dina_review_table(typed, "review_values")
  input <- textConnection("2\n\n4\n2\n\n5\n")
  on.exit(close(input), add = TRUE)
  output <- capture.output(result <- dina_review_family_menu(root, "surveys", input, TRUE))
  menu <- dina_review_read(root, "surveys")
  expect_equal(dina_review_table(menu, "review_values"), rows)
  expect_equal(menu$status, typed$status)
  expect_equal(result, "back")
  expect_equal(dina_hash_file(files$current), original)
  expect_match(paste(output, collapse = "\n"), "dina sources explore surveys", fixed = TRUE)
  expect_match(paste(output, collapse = "\n"), "No files changed", fixed = TRUE)
  expect_match(paste(output, collapse = "\n"), "Press Enter to return to", fixed = TRUE)
  expect_error(dina_cmd_sources(root, c("include", "surveys", "--confirm", "--include-run", typed$run)), "superseded")
})

test_that("failed backup prevents replacement and a partial inclusion is reported accurately", {
  source_cli_for_tests()
  root <- mini_repo()
  files <- review_fixture_surveys(root)
  original <- dina_hash_file(files$current)
  second <- file.path(root, "input_data", "_new", "surveys", "COL_2000N.dta")
  file.copy(files$incoming, second)
  expect_equal(run_dina_cli(c("sources", "explore", "surveys"), root)$status, 0L)
  engine <- dina_review_engine(root, "surveys")
  copy <- engine$survey_pop_copy_path
  engine$survey_pop_copy_path <- function(from, to) {
    if (grepl("/snapshots/original/", to, fixed = TRUE)) "copy_failed" else copy(from, to)
  }
  env <- environment(dina_review_include)
  original_engine <- get("dina_review_engine", env)
  on.exit(assign("dina_review_engine", original_engine, env), add = TRUE)
  assign("dina_review_engine", function(...) engine, env)
  expect_error(dina_review_include(root, "surveys", list(confirm = TRUE)), "Inclusion did not complete")
  record <- dina_review_read(root, "surveys")
  expect_equal(record$status, "inclusion_failed")
  expect_equal(dina_hash_file(files$current), original)
  report <- dina_read_csv_table_or_empty(file.path(record$confirmation, "tables", "promote_report.csv"))
  expect_true("skipped_backup_failed" %in% report$promote_status)
  expect_true("staged" %in% report$promote_status)
  expect_equal(dina_review_recommendation(root)$proposal$command, "dina sources table surveys")
  expect_error(dina_review_include(root, "surveys", list(confirm = TRUE)), "unresolved checks")
})

test_that("unavailable evidence and failed exploration cannot look like a successful review", {
  source_cli_for_tests()
  empty <- data.frame(year = 2000L, value = NA_real_)
  expect_equal(dina_review_compare(empty, empty, "year", "value")$result, "absence unexplained")
  root <- mini_repo()
  files <- review_fixture_surveys(root)
  expect_equal(run_dina_cli(c("sources", "explore", "surveys"), root)$status, 0L)
  dina_review_save(list(family = "surveys", status = "incomplete"), root)
  expect_error(dina_cmd_sources(root, c("table", "surveys")), "latest exploration did not complete")
})

test_that("coverage distinguishes extracted extensions, lost years and retained survey history", {
  source_cli_for_tests()
  old <- data.frame(country = "AAA", year = 2000:2002, value = c(1, 2, 3))
  new <- data.frame(country = "AAA", year = c(2000L, 2001L, 2003L), value = c(1, NA, 4))
  values <- dina_review_compare(old, new, c("country", "year"), "value")
  coverage <- dina_review_coverage(list(), values, "sna")
  expect_equal(coverage$extension_years, "2003")
  expect_equal(coverage$overlap_years, "2000")
  expect_equal(coverage$missing_in_new_years, "2001,2002")
  expect_true("removed observation" %in% values$result)
  survey <- list(outputs = list(year_coverage = data.frame(country = "AAA", canonical_discovered_years = "2000-2002",
    incoming_discovered_years = "2002-2003", selected_years = "2000-2003", blocked_years = "", canonical_discovered_count = 3L, incoming_discovered_count = 2L)))
  coverage <- dina_review_coverage(survey, data.frame(), "surveys")
  expect_equal(coverage$extension_years, "2003")
  expect_equal(coverage$overlap_years, "2002")
  expect_equal(coverage$missing_in_new_years, "")
})

test_that("compact registry preserves complete IDs and includes artifacts owned by WID", {
  source_cli_for_tests()
  root <- mini_repo()
  id <- "a-complete-source-id-longer-than-the-old-column-width"
  dina_write_yaml(list(sources = list(list(id = id, family = "prices", country = "MULTI", method = "wid", integration = "sources_explore_include_wid"),
    list(id = "wid-main", family = "wid", country = "ARG", method = "wid"))), file.path(root, "config", "sources.yml"))
  registry <- dina_source_registry(root, dina_source_resolve_family_filter("wid", root))
  expect_equal(length(registry), 2L)
  output <- capture.output(dina_print_source_registry_view(registry, root))
  expect_match(paste(output, collapse = "\n"), id, fixed = TRUE)
  expect_equal(dina_source_public_family(registry[[1]]), "wid")
})

test_that("dashboard and session guidance agree on each family and unfinished reviews", {
  source_cli_for_tests()
  root <- mini_repo()
  session <- list(task_runs = list())
  for (family in c("sna", "admin", "surveys", "wid")) {
    incoming <- file.path(root, "input_data", "_new", family)
    dir.create(incoming, recursive = TRUE, showWarnings = FALSE)
    writeLines("candidate", file.path(incoming, "source.txt"))
    expect_equal(dina_dashboard_state_fast(session, root)$proposal$command, paste("dina sources explore", family))
    record <- list(family = family, run = "fixture", status = "all_good", reviewed_at = dina_now(), watch = dina_review_watch(incoming))
    dina_review_save(record, root)
    expect_equal(dina_session_state(session, root)$proposal$command, paste("dina sources include", family))
    expect_equal(dina_dashboard_state_fast(session, root)$proposal$command, paste("dina sources include", family))
    record$status <- "blocked"
    dina_review_save(record, root)
    expect_equal(dina_dashboard_state_fast(session, root)$proposal$command, paste("dina sources table", family))
    record$status <- "included"
    dina_review_save(record, root)
    expect_null(dina_review_recommendation(root))
  }
})

test_that("an empty survey workspace explains why no review can be prepared", {
  source_cli_for_tests()
  root <- mini_repo()
  result <- run_dina_cli(c("sources", "explore", "surveys"), root)
  expect_equal(result$status, 1L)
  expect_match(result$output, "No survey files are available to prepare", fixed = TRUE)
  expect_match(result$output, "not checked", fixed = TRUE)
  expect_equal(dina_review_read(root, "surveys")$status, "incomplete")
  expect_error(dina_review_include(root, "surveys", list(confirm = TRUE)), "Explore this family first")
})
