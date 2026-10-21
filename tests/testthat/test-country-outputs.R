source(file.path(repo_root_for_tests, "code", "R", "country_outputs.R"))

country_output_fixture <- function() {
  root <- tempfile("country-output-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE), envir = parent.frame())
  root
}
country_output_book <- function(path, sheets) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  openxlsx::write.xlsx(sheets, path, overwrite = TRUE)
}
country_output_publish_fixture <- function(root, stage, task = "07a-test", countries = "ARG", commit = function() NULL) {
  country_output_publish(stage, root, list(id = task, country_scope = "selected"), countries,
    c("ARG", "COL", "MEX"), file.path(root, "work", "publication.rds"), commit = commit)
}

test_that("merging a Stata workbook retains valid Excel display attributes and all cells", {
  stata <- Sys.getenv("DINA_STATA_CMD", unset = "")
  if (!nzchar(stata)) stata <- "/Applications/Stata/StataMP.app/Contents/MacOS/stata-mp"
  skip_if(!file.exists(stata), "Stata is not installed")
  root <- country_output_fixture()
  writeLines(c("clear all", "set more off", "set obs 2", "gen double p = _n / 100", "gen double mpoint = .93",
    'export excel using "old.xlsx", sheet("COL2021") firstrow(variables) replace',
    "replace mpoint = .95", 'export excel using "old.xlsx", sheet("ECU2011") firstrow(variables) sheetreplace'),
    file.path(root, "create.do"))
  processx::run(stata, c("-b", "do", "create.do"), wd = root, timeout = 30000)
  old <- file.path(root, "old.xlsx")
  expect_true(file.exists(old))
  unchanged <- openxlsx::read.xlsx(old, "COL2021")
  incoming <- file.path(root, "incoming.xlsx")
  selected <- data.frame(p = c(.01, .02), mpoint = c(.96, .96))
  country_output_book(incoming, list(ECU2011 = selected))
  destination <- file.path(root, "merged.xlsx")
  country_output_workbook(old, incoming, destination, "ECU", "03a-test")
  expect_equal(openxlsx::read.xlsx(destination, "COL2021"), unchanged, tolerance = 0)
  expect_equal(openxlsx::read.xlsx(destination, "ECU2011"), selected, tolerance = 0)
  xml_dir <- file.path(root, "xml")
  unzip(destination, exdir = xml_dir)
  xml <- paste(unlist(lapply(list.files(xml_dir, "\\.xml$", recursive = TRUE, full.names = TRUE), readLines, warn = FALSE)), collapse = "")
  expect_false(grepl('showGridLines="NA"|rgb="#[[:xdigit:]]{6}"', xml))
  writeLines(c("clear all", "set more off", 'import excel "merged.xlsx", sheet("COL2021") firstrow clear',
    "assert _N == 2", "assert p == _n / 100", "assert mpoint == .93",
    'import excel "merged.xlsx", sheet("ECU2011") firstrow clear', "assert _N == 2", "assert mpoint == .96",
    'file open passed using "passed.txt", write replace', 'file write passed "passed"', "file close passed"),
    file.path(root, "verify.do"))
  processx::run(stata, c("-b", "do", "verify.do"), wd = root, timeout = 30000)
  expect_true(file.exists(file.path(root, "passed.txt")), info = paste(readLines(file.path(root, "verify.log"), warn = FALSE), collapse = "\n"))
})

test_that("workbook publication preserves unselected rows and retires selected years", {
  root <- country_output_fixture(); stage <- file.path(root, "stage")
  relative <- "output/ineqstats/ineqstats_natinc_esn.xlsx"
  old <- list(ARG2000 = data.frame(p = 0, value = 1), ARG2001 = data.frame(p = 0, value = 2), COL2000 = data.frame(p = 0, value = 8))
  for (sheet in c("Summary", "Composition", "sginis", "gtheils")) old[[sheet]] <- data.frame(country = c("ARG", "COL"), year = 2000L, value = c(1, 8))
  new <- list(ARG2000 = data.frame(p = 0, value = 4))
  for (sheet in c("Summary", "Composition", "sginis", "gtheils")) new[[sheet]] <- data.frame(country = "ARG", year = 2000L, value = 4)
  country_output_book(file.path(root, relative), old)
  country_output_book(file.path(stage, relative), new)
  country_output_publish_fixture(root, stage, "06a-test")
  expect_setequal(openxlsx::getSheetNames(file.path(root, relative)), setdiff(names(old), "ARG2001"))
  expect_equal(openxlsx::read.xlsx(file.path(root, relative), "COL2000"), old$COL2000)
  for (sheet in c("Summary", "Composition", "sginis", "gtheils")) {
    table <- openxlsx::read.xlsx(file.path(root, relative), sheet)
    expect_equal(table$value[table$country == "COL"], 8)
    expect_equal(table$value[table$country == "ARG"], 4)
  }
})

test_that("all synthetic CSV partitions retain unselected countries", {
  root <- country_output_fixture(); stage <- file.path(root, "stage")
  for (name in c("detailed", "grouped", "efftax")) {
    relative <- paste0("output/synthetic_microfiles/_smicrofile_long_", name, ".csv")
    dir.create(dirname(file.path(root, relative)), recursive = TRUE, showWarnings = FALSE)
    dir.create(dirname(file.path(stage, relative)), recursive = TRUE, showWarnings = FALSE)
    write.csv(data.frame(country = c("ARG", "COL"), year = 2000, value = c(1, 8)), file.path(root, relative), row.names = FALSE)
    write.csv(data.frame(country = "ARG", year = 2024, value = 4), file.path(stage, relative), row.names = FALSE)
  }
  country_output_publish_fixture(root, stage)
  for (name in c("detailed", "grouped", "efftax")) {
    table <- read.csv(file.path(root, paste0("output/synthetic_microfiles/_smicrofile_long_", name, ".csv")))
    expect_equal(table$year[table$country == "ARG"], 2024)
    expect_equal(table$value[table$country == "COL"], 8)
    expect_equal(nrow(table), 2L)
  }
})

test_that("03e replaces only selected extrapolated merging sheets", {
  root <- country_output_fixture(); stage <- file.path(root, "stage")
  relative <- "output/bfm_summary/bfm_norep_pos/merging_points.xlsx"
  old <- list(ARG2000 = data.frame(mpoint = 93), ARG2000_extrap = data.frame(mpoint = 94), ARG1999_extrap = data.frame(mpoint = 94), COL2000_extrap = data.frame(mpoint = 96))
  country_output_book(file.path(root, relative), old)
  country_output_book(file.path(stage, relative), list(ARG2000_extrap = data.frame(mpoint = 95)))
  country_output_publish_fixture(root, stage, "03e-test")
  expect_equal(openxlsx::read.xlsx(file.path(root, relative), "ARG2000"), old$ARG2000)
  expect_equal(openxlsx::read.xlsx(file.path(root, relative), "COL2000_extrap"), old$COL2000_extrap)
  expect_false("ARG1999_extrap" %in% openxlsx::getSheetNames(file.path(root, relative)))
  country_output_book(file.path(stage, relative), list(ARG2000 = data.frame(mpoint = 80)))
  expect_error(country_output_publish_fixture(root, stage, "03e-test"), "observed merging-point")
  expect_equal(openxlsx::read.xlsx(file.path(root, relative), "ARG2000"), old$ARG2000)
})

test_that("country-specific rate workbooks can replace year-only sheets", {
  root <- country_output_fixture(); stage <- file.path(root, "stage")
  relative <- "intermediary_data/tax_rates/ARG/ARG_rates.xlsx"
  country_output_book(file.path(root, relative), list(`2020` = data.frame(rate = 1)))
  country_output_book(file.path(stage, relative), list(`2020` = data.frame(rate = 2)))
  country_output_publish_fixture(root, stage, "04a5-test")
  expect_equal(openxlsx::read.xlsx(file.path(root, relative), "2020")$rate, 2)
})

test_that("ISO-year weight adjusters are country-owned while the index stays shared", {
  root <- country_output_fixture(); stage <- file.path(root, "stage")
  relative <- "intermediary_data/weight_adjusters/CRI2000.xlsx"
  index <- "intermediary_data/weight_adjusters/index.xlsx"
  country_output_book(file.path(root, relative), list(brackets = data.frame(weight = 1)))
  country_output_book(file.path(stage, relative), list(brackets = data.frame(weight = 2)))
  country_output_book(file.path(root, index), list(country_years_03d = data.frame(country_year = c("CRI_2000", "MEX_2010"))))
  country_output_book(file.path(stage, index), list(country_years_03d = data.frame(country_year = "CRI_2001")))
  publish <- function() country_output_publish(stage, root, list(id = "03d-test", country_scope = "selected"),
    "CRI", c("CRI", "MEX"), file.path(root, "work/publication.rds"))
  publish()
  expect_equal(openxlsx::read.xlsx(file.path(root, relative), "brackets")$weight, 2)
  expect_setequal(openxlsx::read.xlsx(file.path(root, index), "country_years_03d")$country_year, c("CRI_2001", "MEX_2010"))
  expect_false(country_output_path_owned(index, "CRI"))
  expect_false(country_output_path_owned("intermediary_data/weight_adjusters/CRI20001.xlsx", "CRI"))
  unselected <- "intermediary_data/weight_adjusters/MEX2010.xlsx"
  country_output_book(file.path(root, unselected), list(brackets = data.frame(weight = 8)))
  country_output_book(file.path(stage, unselected), list(brackets = data.frame(weight = 9)))
  expect_error(publish(), "unselected country's files.*MEX2010")
  expect_equal(openxlsx::read.xlsx(file.path(root, unselected), "brackets")$weight, 8)
})

test_that("failed publication restores prior files and rejects unselected files", {
  root <- country_output_fixture(); stage <- file.path(root, "stage")
  targets <- file.path(root, c("one", "two")); sources <- file.path(root, c("new-one", "new-two"))
  for (i in seq_along(targets)) { writeLines(paste("old", i), targets[i]); writeLines(paste("new", i), sources[i]) }
  journal <- file.path(root, "publication.rds")
  expect_error(country_output_transaction(sources, targets, journal, commit = function() stop("simulated interruption")), "simulated interruption")
  expect_equal(readLines(targets[1]), "old 1"); expect_equal(readLines(targets[2]), "old 2")
  expect_false(file.exists(journal))
  dir.create(file.path(stage, "intermediary_data/microdata/bfm_norep_pre"), recursive = TRUE)
  writeLines("unselected", file.path(stage, "intermediary_data/microdata/bfm_norep_pre/COL_2024_bfm_norep_pre.dta"))
  expect_error(country_output_publish_fixture(root, stage, "05b-test"), "unselected country's files")
})

country_output_gpinter_fixture <- function(stage, country, groups) {
  grid <- c(seq(0, .99, by = .01), seq(.991, .999, by = .001), seq(.9991, .9999, by = .0001), seq(.99991, .99999, by = .00001))
  data <- data.frame(country = country, year = 2020L, step = "nat", unit = "esn", p = grid, thr = 1, avg = 2, bckt_sh = .01, topavg = 3, topsh = .1)
  dir.create(file.path(stage, "output/ineqstats"), recursive = TRUE, showWarnings = FALSE)
  write.csv(data, file.path(stage, "output/ineqstats/_gpinter_all_topcorrected.csv"), row.names = FALSE)
  write.csv(groups, file.path(stage, "_groups.csv"), row.names = FALSE)
  write.csv(data.frame(country = country, year = 2020L, step = "nat", unit = "esn", status = "success"), file.path(stage, "_coverage.csv"), row.names = FALSE)
  data
}

test_that("GPinter chunks wait for complete publication and remain reusable", {
  root <- country_output_fixture()
  groups <- data.frame(country = c("ARG", "COL", "MEX"), year = 2020L, step = "nat", unit = "esn")
  stages <- file.path(root, c("ARG", "COL")); assembly <- file.path(root, "assembled")
  country_output_gpinter_fixture(stages[1], "ARG", groups)
  baseline <- file.path(root, "output/ineqstats/_gpinter_all_topcorrected.csv")
  dir.create(dirname(baseline), recursive = TRUE)
  mex <- country_output_gpinter_fixture(file.path(root, "mex-fixture"), "MEX", groups)
  write.csv(mex, baseline, row.names = FALSE)
  expect_error(country_output_gpinter_assemble(stages[1], assembly, baseline, c("ARG", "COL")), "selected chunks are not complete")
  expect_equal(nrow(read.csv(baseline)), 127L)
  country_output_gpinter_fixture(stages[2], "COL", groups)
  saved <- readLines(file.path(stages[1], "output/ineqstats/_gpinter_all_topcorrected.csv"))
  expect_error(country_output_gpinter_assemble(stages, assembly, tempfile(), c("ARG", "COL")), "MEX/2020/nat/esn")
  expect_identical(readLines(file.path(stages[1], "output/ineqstats/_gpinter_all_topcorrected.csv")), saved)
  country_output_gpinter_assemble(stages, assembly, baseline, c("ARG", "COL"))
  country_output_publish_fixture(root, assembly, "07c-test", c("ARG", "COL"))
  published <- read.csv(baseline)
  expect_equal(nrow(published), 381L)
  expect_equal(published[published$country == "MEX", ], mex, ignore_attr = TRUE)
  broken <- published; broken$p[1] <- broken$p[2]
  expect_error(country_output_gpinter_validate(assembly, broken), "duplicated, or invalid fractiles")
})

test_that("zero-success statistics retire selected prior contributions", {
  root <- country_output_fixture(); stage <- file.path(root, "stage")
  relative <- "output/ineqstats/ineqstats_natinc_esn.xlsx"
  country_output_book(file.path(root, relative), list(ARG2000 = data.frame(p = 0, avg = 2), COL2000 = data.frame(p = 0, avg = 8),
    Summary = data.frame(country = c("ARG", "COL"), year = 2000L, avg = c(2, 8))))
  dir.create(stage)
  write.csv(data.frame(country = "ARG", year = 2000L, step = "natinc", unit = "esn", status = "excluded"), file.path(stage, "_coverage.csv"), row.names = FALSE)
  country_output_publish_fixture(root, stage, "06a-test")
  expect_false("ARG2000" %in% openxlsx::getSheetNames(file.path(root, relative)))
  expect_equal(openxlsx::read.xlsx(file.path(root, relative), "Summary")$country, "COL")
})

test_that("empty BFM placeholders retire selected index rows without inventing a case", {
  old <- data.frame(country_year = c("ARG_2000", "COL_2000"), extrap = c(1, 1), fail = c(0, 0))
  empty <- data.frame(country_year = "", extrap = NA_real_, fail = NA_real_)
  merged <- country_output_rows(old, empty, "ARG")
  expect_equal(merged$country_year, "COL_2000")
  expect_equal(nrow(merged), 1L)
  invalid <- empty; invalid$fail <- 1
  expect_error(country_output_rows(old, invalid, "ARG"), "data without a country")
})

test_that("publication locks retain live owners and recover dead-owner transactions", {
  root <- country_output_fixture()
  lock <- file.path(root, "output/run_logs/.country-publication-lock")
  dir.create(lock, recursive = TRUE)
  journal <- file.path(root, "output/run_logs/run/work/task--ARG/publication.rds")
  dir.create(dirname(journal), recursive = TRUE)
  target <- file.path(root, "published.txt"); backup <- paste0(target, ".backup")
  writeLines("new", target); writeLines("old", backup)
  saveRDS(data.frame(source = "unused", target = target, backup = backup, existed = TRUE), journal)
  owner <- list(pid = Sys.getpid(), journal = journal)
  saveRDS(owner, file.path(lock, "owner.rds"))
  expect_error(country_output_lock_recover(root), "Another publication is active")
  expect_equal(readLines(target), "new")
  owner$pid <- 2147483647L
  saveRDS(owner, file.path(lock, "owner.rds"))
  result <- country_output_lock_recover(root)
  expect_true(result$recovered)
  expect_false(result$complete)
  expect_equal(readLines(target), "old")
  expect_false(dir.exists(lock))

  dir.create(lock)
  writeLines("new", target); writeLines("old", backup)
  saveRDS(data.frame(source = "unused", target = target, backup = backup, existed = TRUE), journal)
  jsonlite::write_json(list(chunks = list(`task--ARG` = list(status = "succeeded"))), file.path(root, "output/run_logs/run/country-run.json"), auto_unbox = TRUE)
  saveRDS(owner, file.path(lock, "owner.rds"))
  result <- country_output_lock_recover(root)
  expect_true(result$complete)
  expect_equal(readLines(target), "new")
  expect_false(file.exists(backup))
})

test_that("empty dated synthetic workbooks retain the prior unselected generation", {
  root <- country_output_fixture(); stage <- file.path(root, "stage")
  old <- "output/synthetic_microfiles/smicrofile_esn_nat_14Oct2026.xlsx"
  new <- "output/synthetic_microfiles/smicrofile_esn_nat_19Oct2026.xlsx"
  country_output_book(file.path(root, old), list(ARG2000 = data.frame(p = 0, avg = 2), COL2000 = data.frame(p = 0, avg = 8),
    `Summary-estimates` = data.frame(country = c("ARG", "COL"), year = 2000L, avg = c(2, 8))))
  dir.create(stage)
  write.csv(data.frame(path = new, sheet = "Summary-estimates"), file.path(stage, "_empty_workbooks.csv"), row.names = FALSE)
  country_output_prepare_empty_workbooks(stage)
  expect_true(file.exists(file.path(stage, new)))
  baselines <- setNames(list(old), basename(new))
  country_output_publish(stage, root, list(id = "07a-test", country_scope = "selected"), "ARG", c("ARG", "COL"),
    file.path(root, "publication.rds"), baselines = baselines)
  expect_setequal(openxlsx::getSheetNames(file.path(root, new)), c("COL2000", "Summary-estimates"))
  expect_equal(openxlsx::read.xlsx(file.path(root, new), "Summary-estimates")$country, "COL")
  expect_true("ARG2000" %in% openxlsx::getSheetNames(file.path(root, old)))
})
