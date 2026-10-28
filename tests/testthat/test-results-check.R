results_check_fixture <- function() {
  p <- round(c(seq(0, .99, .01), seq(.991, .999, .001),
    seq(.9991, .9999, .0001), seq(.99991, .99999, .00001)), 5)
  end <- c(p[-1L], 1); mass <- end - p
  fmt <- function(x) vapply(x, format, character(1), scientific = FALSE, trim = TRUE, digits = 8)
  bracket <- paste0("p", fmt(p * 100), "p", fmt(end * 100))
  top <- paste0("p", fmt(p * 100), "p100")
  average <- seq_along(p); income <- average * mass; share <- income / sum(income)
  rows <- function(kind, labels, value) data.frame(country = "COL", year = 2024L,
    p = labels, widcode = paste0(kind, "ptinc992j"), value = value)
  # The last detailed bin is also its cumulative top; represent it once.
  top_only <- top != tail(bracket, 1L)
  bottom <- list(p0p20 = p < .2, p0p50 = p < .5, p0p90 = p < .9, p50p90 = p >= .5 & p < .9)
  rbind(rows("a", bracket, average), rows("s", bracket, share),
    rows("t", bracket, average - .5),
    rows("a", top[top_only], (rev(cumsum(rev(income))) / rev(cumsum(rev(mass))))[top_only]),
    rows("s", top[top_only], rev(cumsum(rev(share)))[top_only]),
    rows("s", names(bottom), vapply(bottom, function(ii) sum(share[ii]), numeric(1))),
    rows("a", names(bottom)[-1L], vapply(bottom[-1L], function(ii) sum(income[ii]) / sum(mass[ii]), numeric(1))))
}

test_that("standalone consistency checks select the recorded main export and leave it unchanged", {
  skip_if_not_installed("haven")
  source_cli_for_tests(); root <- mini_repo()
  file.copy(file.path(repo_root_for_tests, "code", "R", "consistency_checker.R"), file.path(root, "code", "R"))
  folder <- file.path(root, "output", "latest_wid_series"); dir.create(folder, recursive = TRUE)
  date <- "27Oct2026"; relative <- file.path("output", "latest_wid_series", paste0("dina_latam_", date, ".dta"))
  main <- file.path(root, relative); haven::write_dta(results_check_fixture(), main)
  old_relative <- file.path("output", "latest_wid_series", "dina_latam_26Oct2026.dta")
  writeLines("Older generation must not be selected", file.path(root, old_relative))
  amory <- file.path("output", "latest_wid_series", paste0("dina_latam_amory_", date, ".dta"))
  writeLines("Historical subset must not be selected", file.path(root, amory))
  report_path <- dina_results_consistency_paths(date)[["receipt"]]
  dina_write_json(list(expected_cases = list(list(country = "COL", year = 2024L, step = "nat"))),
    file.path(root, report_path))
  dina_results_publish_metadata(list(status = "complete", generated_at = "2026-10-26T10:00:00+0000",
    series = old_relative), dina_results_metadata_path(root, "26Oct2026"))
  dina_results_publish_metadata(list(status = "complete", generated_at = "2026-10-27T10:00:00+0000",
    series = c(relative, amory), consistency = list(reports = list(receipt = report_path))),
    dina_results_metadata_path(root, date))
  before <- dina_hash_file(main)
  output <- capture.output(receipt <- dina_cmd_results(root, "check"))
  expect_identical(receipt$status, "clean")
  expect_identical(normalizePath(receipt$export), normalizePath(main))
  expect_identical(dina_hash_file(main), before)
  expect_true(all(file.exists(unlist(receipt$reports))))
  expect_match(paste(output, collapse = "\n"), "completed, no unresolved issues")

  # An unrecorded copy can be audited, with unverified coverage reported rather
  # than silently certifying coverage inferred from the file itself.
  supplied <- file.path(root, "supplied.dta"); file.copy(main, supplied)
  supplied_before <- dina_hash_file(supplied)
  capture.output(review <- dina_cmd_results(root, c("check", "supplied.dta")))
  expect_identical(review$status, "review_needed")
  expect_identical(dina_hash_file(supplied), supplied_before)
  expect_match(paste(readLines(review$reports$summary), collapse = "\n"), "scope_unverified")
})

test_that("completion and notifications report consistency review without failing the export", {
  source_cli_for_tests(); root <- mini_repo(); date <- "27Oct2026"
  config <- dina_config(root, expand_env = FALSE)
  haven::write_dta(data.frame(year = 2023L, iso = "CO", p = "p90p100",
    widcode = "sptinc992j", value = .5), file.path(root, config$export_validation$previous_update_file))
  dina_results_begin(root, date, config)
  paths <- c(file.path("output", "figures", "updates", paste0("update-", date, "-sptinc992j-", names(dina_results_graph_names()), ".pdf")),
    file.path("output", "latest_wid_series", paste0("dina_latam", c("_wide", ""), "_", date, ".dta")))
  for (path in paths) {
    dir.create(dirname(file.path(root, path)), recursive = TRUE, showWarnings = FALSE)
    writeLines("artifact fixture", file.path(root, path))
  }
  expect_error(dina_results_complete(root, date, config), "audit did not finish")
  write_consistency_receipt_for_tests(root, date, "review_needed", 3L)
  metadata <- dina_results_complete(root, date, config)
  expect_identical(metadata$status, "complete")
  expect_identical(metadata$consistency$status, "review_needed")
  expect_length(metadata$series, 2L)
  expect_false(any(grepl("amory", unlist(metadata))))
  expect_true(all(grepl("consistency review needed", dina_results_inventory(root)$graphs$status)))
  output <- paste(capture.output(dina_results_report_latest_consistency(root)), collapse = "\n")
  expect_match(output, "completed, review needed")
  expect_match(output, "3 findings across 3 cases")
  message <- dina_run_notification_message(TRUE, list(list(task = "07d-export-results-to-wid",
    status = "succeeded", consistency = metadata$consistency)))
  expect_match(message, "DINA run finished")
  expect_match(message, "review needed: 3 findings across 3 cases")
  expect_match(message, "summary.txt", fixed = TRUE)
})
