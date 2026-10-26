# Exercise the production availability calculation in an isolated directory.
# Source files only need to exist: 07b does not read their contents.
test_that("export availability extends carry-forward without inventing observations", {
  skip_if_not_installed("haven")
  skip_if_not_installed("processx")
  stata <- Sys.getenv("DINA_STATA_CMD", unset = "/Applications/Stata/StataMP.app/Contents/MacOS/stata-mp")
  skip_if(!file.exists(stata), "Stata unavailable")
  root <- tempfile("dina-export-availability-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  for (country in c("TST", "RAW", "NON")) {
    dir.create(file.path(root, "input_data", "admin_data", country), recursive = TRUE)
    dir.create(file.path(root, "input_data", "surveys_CEPAL", country), recursive = TRUE)
  }
  dir.create(file.path(root, "output", "data_reports"), recursive = TRUE)
  files <- c(
    "input_data/surveys_CEPAL/TST/TST_2022N.dta",
    "input_data/surveys_CEPAL/TST/TST_2024N1.dta",
    "input_data/admin_data/TST/gpinter_TST_2024.xlsx"
  )
  # Deliberately present future survey/admin files must not make 2025 observed.
  for (country in c("TST", "RAW")) {
    files <- c(files,
      sprintf("input_data/surveys_CEPAL/%s/%s_2025N.dta", country, country),
      sprintf("input_data/admin_data/%s/gpinter_%s_2025.xlsx", country, country),
      sprintf("input_data/admin_data/%s/wage_%s_2025.xlsx", country, country),
      sprintf("input_data/admin_data/%s/diverse_%s_2025.xlsx", country, country))
  }
  stopifnot(all(file.create(file.path(root, files))))
  production <- readLines(file.path(repo_root_for_tests,
    "code/Stata/07b-report_data_availability.do"), warn = FALSE)
  start <- which(production == "*define first year ")
  expect_length(start, 1L)
  calculation <- production[seq.int(start, length(production))]
  writeLines(calculation, file.path(root, "availability.do"))
  exporter <- readLines(file.path(repo_root_for_tests,
    "code/Stata/07d-export-results-to-wid.do"), warn = FALSE)
  guard_start <- grep("^// Availability must cover the export horizon", exporter)
  guard_end <- which(exporter == "qui keep country year grade")
  expect_length(guard_start, 1L)
  expect_length(guard_end, 1L)
  writeLines(c("local ly = $export_last_y", exporter[seq.int(guard_start, guard_end - 1L)]),
    file.path(root, "export-coverage.do"))
  fixture <- c('set more off', 'global all_countries "TST RAW NON"',
    'global first_y 2022', 'global last_y 2024', 'global dina_output_root "."')
  for (endpoint in c(2025L, 2024L, 2023L)) {
    fixture <- c(fixture, "clear all", sprintf("global export_last_y %d", endpoint),
      'do "availability.do"', sprintf('save "availability-%d.dta", replace', endpoint))
  }
  fixture <- c(fixture, 'global export_last_y 2025',
    'use "availability-2024.dta", clear', 'capture noisily do "export-coverage.do"',
    'assert _rc == 459', 'use "availability-2025.dta", clear', 'do "export-coverage.do"')
  writeLines(fixture, file.path(root, "fixture.do"))
  writeLines(c('capture noisily do "fixture.do"', 'local result_rc = _rc',
    'file open result using "result-rc.txt", write text replace',
    'file write result "`result_rc\'"', 'file close result'), file.path(root, "driver.do"))
  processx::run(stata, c("-b", "do", "driver.do"), wd = root,
    timeout = 90000, error_on_status = FALSE)
  result_file <- file.path(root, "result-rc.txt")
  log <- paste(readLines(file.path(root, "driver.log"), warn = FALSE), collapse = "\n")
  expect_true(file.exists(result_file), info = log)
  expect_equal(as.integer(readLines(result_file, warn = FALSE)), 0L, info = log)
  expect_match(log, "Availability report is missing TST 2025. Run dina run 07b,07d.", fixed = TRUE)
  results <- lapply(c(2025L, 2024L, 2023L), function(endpoint) {
    result <- as.data.frame(haven::read_dta(file.path(root, sprintf("availability-%d.dta", endpoint))))
    result$grade <- as.numeric(result$grade)
    result[] <- lapply(result, as.vector)
    result
  })
  extended <- results[[1L]]
  expect_equal(range(extended$year), c(2022, 2025))
  expect_equal(nrow(extended), 12L)
  observed <- subset(extended, country == "TST")
  expect_equal(observed$grade, c(3, 2, 4, 1))
  expect_equal(observed$grade_label[observed$year == 2025], "1. Carry forward/backward")
  future <- subset(extended, year == 2025)
  expect_true(all(future$survey == "no" & future$has_tax == 0))
  expect_true(all(future$admin_total == "no" & future$admin_wage == "no" & future$admin_other == "no"))
  # Preserve the existing missing grade for countries with no observations;
  # 07d maps those to regional grade zero, rather than carry-forward grade one.
  expect_true(all(is.na(extended$grade[extended$country %in% c("RAW", "NON")])))
  existing <- subset(extended, year <= 2024)
  rownames(existing) <- NULL
  expect_equal(existing, results[[2L]])
  expect_equal(results[[2L]], results[[3L]])
})
