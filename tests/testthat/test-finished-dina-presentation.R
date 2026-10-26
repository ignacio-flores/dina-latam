finished_presentation_record <- function(root) {
  run <- file.path(root, "output", "experiments", "finished-review-fixture")
  dir.create(file.path(run, "tables"), recursive = TRUE)
  issues <- data.frame(
    check_id = c("bracket_bounds", "bracket_bounds", "share_identity",
                 "metadata", "export_grid"),
    status = c("review", "review", "review", "warning", "warning"),
    country = c("AAA", "AAA", "AAA", "", "AAA"),
    year = c(1991, 2022, 2022, NA, 1992),
    step = c("nat", "nat", "nat", "", "nat"),
    unit = c("ind", "esn", "esn", "", "esn"),
    percentile = c("p10p11", "p20p21", "p99.99p99.991", "", ""),
    row = c(1L, 2L, 3L, NA, NA),
    field = c("avg", "avg", "bckt_sh", "bracket_semantics", "bracket count"),
    observed = c("100.05", "102", "0.000362107330566", "", "126"),
    expected = c("100", "100", "0.000362110342566", "confirmed definition", "127"),
    difference = c(0.05, 2, -0.000000003012, NA, -1),
    detail = c(
      "Small threshold discrepancy.", "Larger threshold discrepancy.",
      "Small share discrepancy.", "Confirm the supplied bracket definitions.",
      "Complete population coverage; wider brackets require export alignment."
    ),
    source_id = "aaa-finished-fixture", file = "source.dta",
    stringsAsFactors = FALSE
  )
  checks <- data.frame(
    check_id = c("schema", "bracket_bounds", "share_identity", "metadata", "export_grid"),
    status = c("pass", "review", "review", "warning", "warning"),
    affected_rows = c(0L, 2L, 1L, 0L, 0L),
    affected_cases = c(0L, 2L, 1L, 0L, 1L),
    detail = c("Schema is valid.", "Review thresholds.", "Review shares.",
               "Confirm definitions.", "Agree export alignment."),
    source_id = "aaa-finished-fixture", file = "source.dta",
    stringsAsFactors = FALSE
  )
  coverage <- data.frame(
    country = "AAA", step = "nat", unit = c("esn", "ind"),
    first_year = 1991, last_year = 2022, n_years = 32L, n_groups = 32L,
    rows = 4063L, min_brackets = 126L, max_brackets = 127L,
    publication = c(TRUE, FALSE), source_id = "aaa-finished-fixture",
    file = "source.dta", stringsAsFactors = FALSE
  )
  files <- data.frame(
    country = "AAA", version = "v2", publication = "nat/esn",
    citation = "Researcher. 2026. Finished series.",
    doi = "https://doi.org/10.1234/fixture", currency = "unconfirmed",
    price_basis = "unconfirmed", population = "unconfirmed",
    status = "needs_review", source_id = "aaa-finished-fixture",
    file = "source.dta", stringsAsFactors = FALSE
  )
  tables <- list(checks = checks, review_problems = issues,
                 review_coverage = coverage, review_files = files)
  for (name in names(tables)) utils::write.csv(
    tables[[name]], file.path(run, "tables", paste0(name, ".csv")),
    row.names = FALSE, na = ""
  )
  list(family = "dina", status = "needs_review", run = run,
       reviewed_at = "2026-10-20T12:00:00+0200", review_only = TRUE)
}

test_that("finished-series checks use a readable comparison table", {
  source_cli_for_tests()
  root <- mini_repo()
  record <- finished_presentation_record(root)
  output <- paste(capture.output(dina_finished_show_table(record, "checks")), collapse = "\n")
  expect_match(output, "Small\\s+Larger\\s+Typical\\s+Largest")
  expect_match(output, "Mean within thresholds", fixed = TRUE)
  expect_match(output, "Bracket share identity", fixed = TRUE)
  expect_match(output, "Definition", fixed = TRUE)
  expect_match(output, "Format", fixed = TRUE)
  expect_match(output, "does not approve a correction", fixed = TRUE)
  expect_match(output, "pp = percentage points", fixed = TRUE)
  expect_false(grepl("check[ _]id\\s*:", output, ignore.case = TRUE))
})

test_that("consistency sections keep country findings and definition notes separate", {
  source_cli_for_tests()
  root <- mini_repo(); record <- finished_presentation_record(root)
  for (table in c("checks", "review_problems", "review_files", "review_coverage")) {
    path <- file.path(record$run, "tables", paste0(table, ".csv"))
    a <- read.csv(path, stringsAsFactors = FALSE)
    b <- a; b$source_id <- "bbb-finished"; b$file <- "BBB.dta"
    if ("country" %in% names(b)) b$country <- "BBB"
    if (table == "review_problems") {
      b$check_id <- "metadata"; b$status <- "warning"
      b$observed <- ""; b$expected <- ""; b$difference <- NA_real_
      b$country <- ""; b$year <- NA_real_
      b$detail <- "BBB definition only"
    }
    if (table == "checks") {b$check_id <- "metadata"; b$status <- "warning"}
    write.csv(rbind(a, b), path, row.names = FALSE, na = "")
  }
  output <- paste(capture.output(dina_finished_print(record, root = root)), collapse = "\n")
  expect_match(output, "Consistency review — AAA", fixed = TRUE)
  expect_match(output, "Consistency review — BBB", fixed = TRUE)
  sections <- strsplit(output, "Consistency review — BBB", fixed = TRUE)[[1L]]
  expect_match(sections[1L], "Bracket share identity", fixed = TRUE)
  expect_false(grepl("Bracket share identity", sections[2L], fixed = TRUE))
  filtered <- paste(capture.output(dina_finished_show_table(record, "checks", country = "AAA")), collapse = "\n")
  expect_match(filtered, "Consistency review — AAA", fixed = TRUE)
  expect_false(grepl("Consistency review — BBB|unassigned inputs", filtered))
})

test_that("finished-series findings separate small larger and decisions", {
  source_cli_for_tests()
  root <- mini_repo()
  record <- finished_presentation_record(root)
  small_output <- paste(capture.output(
    small <- dina_finished_show_table(record, "small")
  ), collapse = "\n")
  larger_output <- paste(capture.output(
    larger <- dina_finished_show_table(record, "larger")
  ), collapse = "\n")
  decisions_output <- paste(capture.output(
    decisions <- dina_finished_show_table(record, "decisions")
  ), collapse = "\n")
  expect_equal(nrow(small), 2L)
  expect_true(all(small$category == "small"))
  expect_equal(nrow(larger), 1L)
  expect_true(all(larger$category == "larger"))
  expect_setequal(decisions$category, c("definition", "format"))
  expect_match(small_output, "Supplied\\s+Reference\\s+Gap\\s+Size")
  expect_match(larger_output, "2022", fixed = TRUE)
  expect_match(decisions_output, "Complete population coverage", fixed = TRUE)
  expect_false(grepl("check[ _]id\\s*:", paste(
    small_output, larger_output, decisions_output
  ), ignore.case = TRUE))
})

test_that("finished-series filters select the intended cases and retain source definitions", {
  source_cli_for_tests()
  root <- mini_repo()
  record <- finished_presentation_record(root)
  output <- paste(capture.output(
    rows <- dina_finished_show_table(record, "problems", filters = list(
      year = "2022", step = "nat", unit = "esn"
    ))
  ), collapse = "\n")
  expect_equal(nrow(rows), 3L)
  expect_setequal(rows$check_id, c("bracket_bounds", "share_identity", "metadata"))
  expect_true(all(rows$year[!is.na(rows$year)] == 2022))
  expect_true(all(rows$unit[nzchar(rows$unit)] == "esn"))
  expect_false(grepl("1991|1992|nat/ind", output))
  invisible(capture.output(selected <- dina_finished_show_table(
    record, "problems", filters = list(check = "bracket_bounds", size = "small")
  )))
  expect_equal(nrow(selected), 1L)
  expect_equal(selected$year, 1991)
  expect_error(dina_finished_show_table(record, "problems",
    filters = list(size = "tiny")), "--size")
})

test_that("exact finished-series view preserves tiny fraction differences", {
  source_cli_for_tests()
  root <- mini_repo()
  record <- finished_presentation_record(root)
  output <- paste(capture.output(dina_finished_show_table(
    record, "problems", filters = list(check = "share_identity", exact = TRUE)
  )), collapse = "\n")
  expect_match(output, "0.000362107330566", fixed = TRUE)
  expect_match(output, "0.000362110342566", fixed = TRUE)
  expect_match(output, "-3.012e-09", fixed = TRUE)
  expect_match(output, "Small share discrepancy.", fixed = TRUE)
})

test_that("summary and detail navigation leave saved review tables unchanged", {
  source_cli_for_tests()
  root <- mini_repo()
  record <- finished_presentation_record(root)
  paths <- list.files(file.path(record$run, "tables"), full.names = TRUE)
  before <- tools::md5sum(paths)
  output <- paste(capture.output(dina_finished_print(
    record, root = root, filters = list(step = "nat", unit = "esn")
  )), collapse = "\n")
  expect_match(output, "Small\\s+Larger\\s+Typical\\s+Largest")
  expect_match(output, "07d automatically integrates the source", fixed = TRUE)
  expect_match(output, "Researcher. 2026. Finished series.", fixed = TRUE)
  expect_false(grepl("nat/ind", output, fixed = TRUE))
  for (table in c("checks", "small", "larger", "decisions", "problems")) {
    invisible(capture.output(dina_finished_show_table(record, table)))
  }
  expect_identical(tools::md5sum(paths), before)
})
