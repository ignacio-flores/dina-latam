test_that("Stata comparison graphs use available baseline years and publish provenance", {
  stata <- Sys.getenv("DINA_STATA_CMD", unset = "")
  if (!nzchar(stata) && file.exists("/Applications/Stata/StataMP.app/Contents/MacOS/stata-mp")) stata <- "/Applications/Stata/StataMP.app/Contents/MacOS/stata-mp"
  skip_if(!nzchar(stata) || !file.exists(stata), "Stata is not installed")
  skip_if_not_installed("processx"); skip_if_not_installed("haven")
  source_cli_for_tests(); root <- mini_repo()
  cfg <- dina_config(root, expand_env = FALSE)
  haven::write_dta(data.frame(year = 2020:2024, iso = "CO", p = "p90p100", widcode = "sptinc992j", value = .5), file.path(root, cfg$export_validation$previous_update_file))
  file.copy(file.path(repo_root_for_tests, "code", "R", "cli"), file.path(root, "code", "R"), recursive = TRUE)
  file.copy(file.path(repo_root_for_tests, "code", "R", "export-comparison-metadata.R"), file.path(root, "code", "R"))
  dir.create(file.path(root, "output", "latest_wid_series"), recursive = TRUE)
  production <- readLines(file.path(repo_root_for_tests, "code", "Stata", "07d-export-results-to-wid.do"))
  begin <- which(grepl("^// Capture the settings", production))
  begin_end <- which(grepl("^//compare with previous update", production))
  graph_start <- which(grepl('^foreach xxx in "sptinc992j"', production))
  graph_end <- which(seq_along(production) > graph_start & production == "preserve")[[1]] - 1L
  graph_block <- production[graph_start:graph_end]
  predicate_line <- graph_block[grepl("line old_value year if", graph_block)][[1]]
  predicate <- sub("^.*line old_value year if (.*), lcolor.*$", "\\1", predicate_line)
  expect_false(any(grepl("local ly = 2022", production, fixed = TRUE)))
  setup <- c("clear all", "set more off", dina_render_config_do(cfg), 'global graph_scheme ""',
    production[begin:(begin_end - 1L)], "set obs 30", "gen year = 2020 + mod(_n-1,5)",
    'gen str3 country = "COL"', 'gen str12 widcode = "sptinc992j"', 'gen str12 p = ""',
    "local i = 0", "foreach group in p99.99p100 p99.9p100 p99p100 p90p100 p50p90 p0p50 {",
    "local i = `i' + 1", 'replace p = "`group\'" if ceil(_n/5) == `i\'', "}",
    "gen new_value = .1 + (year-2020)*.01", "gen old_value = new_value-.005", "gen data_quality = 1", "local ly = $export_last_y",
    "foreach stop_year in 2023 2024 {", "replace old_value = new_value-.005", "replace old_value = . if year > `stop_year'",
    paste("quietly summarize year if !missing(old_value) &", predicate), "assert r(max) == `stop_year'", graph_block, "}")
  final <- c('save "output/latest_wid_series/dina_latam_`date\'.dta", replace',
    'save "output/latest_wid_series/dina_latam_wide_`date\'.dta", replace',
    'save "output/latest_wid_series/dina_latam_`date\'_amory.dta", replace',
    tail(production, 3), 'file open passed using "passed.txt", write replace', 'file write passed "passed"', "file close passed")
  writeLines(c(setup, final), file.path(root, "graph-fixture.do"))
  result <- processx::run(stata, c("-b", "do", "graph-fixture.do"), wd = root, timeout = 60000, error_on_status = FALSE)
  log <- readLines(file.path(root, "graph-fixture.log"), warn = FALSE)
  expect_true(file.exists(file.path(root, "passed.txt")), info = paste(tail(log, 25), collapse = "\n"))
  inventory <- dina_results_inventory(root)
  expect_equal(nrow(inventory$graphs), 6L)
  expect_true(all(inventory$graphs$status == "Matches recorded baseline and settings"), info = paste(unique(inventory$graphs$status), collapse = "; "))
})
