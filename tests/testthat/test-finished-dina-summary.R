source(file.path(repo_root_for_tests, "code", "R", "cli", "finished_dina_summary.R"))

finished_summary_issues <- function(check_id = "bracket_bounds", observed = 100.1,
                                    expected = 100, difference = NA_real_,
                                    status = "review") {
  n <- max(length(check_id), length(observed), length(expected), length(difference), length(status))
  data.frame(
    check_id = rep_len(check_id, n), status = rep_len(status, n),
    country = "AAA", year = 2022, step = "nat", unit = "esn",
    percentile = "p0p50", row = seq_len(n), field = "avg",
    observed = as.character(rep_len(observed, n)),
    expected = as.character(rep_len(expected, n)),
    difference = rep_len(difference, n), detail = "Fixture discrepancy.",
    stringsAsFactors = FALSE
  )
}

testthat::test_that("size labels use display boundaries without changing validation status", {
  issues <- finished_summary_issues(observed = c(100.1, 100.10001, 99.9, 99.89999))
  original <- issues
  sizes <- finished_dina_issue_sizes(issues)
  testthat::expect_identical(sizes$category, c("small", "larger", "small", "larger"))
  testthat::expect_equal(sizes$relative_percent, c(0.1, 0.10001, 0.1, 0.10001))
  testthat::expect_identical(sizes$status, issues$status)
  testthat::expect_identical(issues, original)
  testthat::expect_identical(sizes[names(issues)], issues)
})

testthat::test_that("zero and nonnumeric references remain honest about relative size", {
  issues <- finished_summary_issues(
    observed = c("0", "0.00001", "1.1", "one", "2"),
    expected = c("0", "0", "[0,1]", "1", ">0"),
    difference = c(NA, NA, 0.1, NA, NA)
  )
  sizes <- finished_dina_issue_sizes(issues)
  testthat::expect_equal(sizes$relative_percent[1:2], c(0, Inf))
  testthat::expect_true(all(is.na(sizes$relative_percent[3:5])))
  testthat::expect_identical(sizes$category, c("small", "larger", "decision", "decision", "decision"))
  testthat::expect_identical(sizes$observed, issues$observed)
  testthat::expect_identical(sizes$expected, issues$expected)
})

testthat::test_that("share triage requires both relative and percentage-point limits", {
  issues <- finished_summary_issues(
    check_id = c("share_identity", "share_total", "top_share_identity", "share_identity", "bracket_bounds"),
    observed = c(0.1001, 1.0002, 0.10010001, 0.001002, 1.0002),
    expected = c(0.1, 1, 0.1, 0.001, 1)
  )
  sizes <- finished_dina_issue_sizes(issues)
  testthat::expect_identical(sizes$category, c("small", "larger", "larger", "larger", "small"))
  testthat::expect_equal(sizes$absolute_pp[1:4], c(0.01, 0.02, 0.010001, 0.0002))
  testthat::expect_true(is.na(sizes$absolute_pp[[5L]]))
  bounds <- finished_summary_issues(
    check_id = c("share_bounds", "gini_bounds"), observed = c(1.01, 1.1), expected = "[0,1]"
  )
  sizes <- finished_dina_issue_sizes(bounds)
  testthat::expect_true(all(is.na(sizes$absolute_pp)))
  testthat::expect_identical(sizes$category, c("decision", "decision"))
})

testthat::test_that("precise saved differences determine size without masking large errors", {
  issues <- finished_summary_issues(
    observed = c(100, 100, 100), expected = 100,
    difference = c(0.02, 90, -10)
  )
  sizes <- finished_dina_issue_sizes(issues)
  testthat::expect_equal(sizes$relative_percent, c(0.02, 90, 10))
  testthat::expect_identical(sizes$category, c("small", "larger", "larger"))
  testthat::expect_equal(sizes$difference, c(0.02, 90, -10))
  large <- finished_dina_issue_sizes(finished_summary_issues(
    observed = 1.1e308, expected = 1e308, difference = 1e307
  ))
  testthat::expect_equal(large$relative_percent, 10)
})

testthat::test_that("definition format and structural findings are not numerical tweaks", {
  issues <- finished_summary_issues(
    check_id = c("metadata", "source_metadata", "export_grid", "percentile_alignment", "export_grid"),
    observed = 1.00001, expected = 1,
    status = c("warning", "fail", "warning", "fail", "fail")
  )
  sizes <- finished_dina_issue_sizes(issues)
  testthat::expect_identical(sizes$category, c("definition", "structural", "format", "structural", "structural"))
  testthat::expect_identical(sizes$status, issues$status)
  testthat::expect_false(any(c("corrected", "approved", "repair") %in% names(sizes)))
})

testthat::test_that("summary separates finding counts from unique rows and cases", {
  issues <- finished_summary_issues(
    check_id = c("bracket_bounds", "bracket_bounds", "bracket_bounds", "metadata", "export_grid"),
    observed = c(100.1, 100.2, 101, 1, 126),
    expected = c(100, 100, 100, 1, 127),
    status = c("review", "review", "review", "warning", "warning")
  )
  issues$row <- c(1, 1, 2, NA, NA)
  issues$year[4] <- NA
  issues$country[4] <- ""
  checks <- data.frame(
    check_id = c("schema", "bracket_bounds", "metadata", "export_grid"),
    status = c("pass", "review", "warning", "warning"),
    affected_rows = c(0L, 2L, 0L, 0L), affected_cases = c(0L, 1L, 0L, 1L)
  )
  checks_before <- checks
  issues_before <- issues
  summary <- finished_dina_check_summary(checks, issues)
  bracket <- summary[summary$check_id == "bracket_bounds", ]
  testthat::expect_equal(bracket$small, 1L)
  testthat::expect_equal(bracket$larger, 2L)
  testthat::expect_equal(bracket$rows, 2L)
  testthat::expect_equal(bracket$cases, 1L)
  testthat::expect_equal(bracket$median_relative_percent, 0.2)
  testthat::expect_equal(bracket$max_relative_percent, 1)
  testthat::expect_identical(bracket$category, "larger")
  metadata <- summary[summary$check_id == "metadata", ]
  testthat::expect_equal(metadata$cases, 0L)
  testthat::expect_equal(metadata$rows, 0L)
  testthat::expect_equal(summary$cases[summary$check_id == "export_grid"], 1L)
  testthat::expect_identical(summary$category[summary$check_id == "schema"], "pass")
  testthat::expect_identical(summary$status, checks$status)
  testthat::expect_true(all(nzchar(summary$action)))
  testthat::expect_identical(checks, checks_before)
  testthat::expect_identical(issues, issues_before)
})

testthat::test_that("summary keeps source rows separate and preserves infinite sizes", {
  issues <- finished_summary_issues(observed = c(1, 2), expected = 0)
  issues$row <- 1L
  issues$source_id <- c("first", "second")
  issues$file <- c("first.dta", "second.dta")
  checks <- data.frame(check_id = "bracket_bounds", status = "review")
  summary <- finished_dina_check_summary(checks, issues)
  testthat::expect_equal(summary$rows, 2L)
  testthat::expect_equal(summary$cases, 2L)
  testthat::expect_equal(summary$max_relative_percent, Inf)
  testthat::expect_equal(summary$median_relative_percent, Inf)
})

testthat::test_that("empty reports and compact labels are supported", {
  empty <- data.frame()
  testthat::expect_equal(nrow(finished_dina_issue_sizes(empty)), 0L)
  summary <- finished_dina_check_summary(empty, empty)
  testthat::expect_equal(nrow(summary), 0L)
  testthat::expect_true(all(c("check_id", "label", "status", "category", "small",
    "larger", "cases", "rows", "median_relative_percent", "max_relative_percent",
    "median_absolute_pp", "max_absolute_pp", "action") %in% names(summary)))
  testthat::expect_true(all(nchar(finished_dina_check_labels()) <= 24L))
  check_only <- finished_dina_check_summary(data.frame(
    check_id = c("schema", "metadata", "export_grid"),
    status = c("fail", "warning", "warning")
  ), empty)
  testthat::expect_identical(check_only$category, c("structural", "definition", "format"))
  testthat::expect_true(all(check_only$cases == 0L & check_only$rows == 0L))
  blocked <- finished_dina_check_summary(data.frame(
    check_id = c("source_metadata", "export_grid"), status = c("fail", "fail")
  ), finished_summary_issues(
    check_id = c("source_metadata", "export_grid"), status = "warning"
  ))
  testthat::expect_identical(blocked$category, c("structural", "structural"))
  testthat::expect_identical(blocked$status, c("fail", "fail"))
})
