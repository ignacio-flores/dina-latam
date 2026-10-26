# Run the actual 01b source-selection/extraction blocks in a small isolated root.
# Only the relevant accepted workbooks are linked; no pipeline task is launched.
col_ecu_stata <- function() {
  stata <- Sys.getenv("DINA_STATA_CMD", unset = "/Applications/Stata/StataMP.app/Contents/MacOS/stata-mp")
  skip_if_not(file.exists(stata), "Stata is required for the bounded source-handoff check")
  stata
}

col_ecu_fixture <- function(updated = c("COL", "ECU")) {
  root <- tempfile("sna-handoff-")
  dir.create(root)
  sources <- c(
    "COL/anex-CuentasNalAnuales-CuentasEconomicasIntegradas-2023p.xlsx",
    "ECU/mcs_cei_2018_2023p.xlsx", "ECU/ceinivel2sd.xlsx", "ECU/CEI2007-2020p.xlsx")
  if ("COL" %in% updated) sources <- c(sources,
    "COL/anex-CuentasNalAnuales-CuentasEconomicasIntegradas-2024p.xlsx")
  if ("ECU" %in% updated) sources <- c(sources, "ECU/mcn_cei_2018_2024p.xlsx")
  for (rel in sources) {
    src <- file.path(repo_root_for_tests, "input_data/sna_country_data", rel)
    skip_if_not(file.exists(src), paste("Accepted fixture source unavailable:", rel))
    dest <- file.path(root, "input_data/sna_country_data", rel)
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    stopifnot(file.symlink(src, dest))
  }
  dir.create(file.path(root, "code/Stata/auxiliar"), recursive = TRUE)
  writeLines("// Graphic configuration is irrelevant to this extraction fixture.",
    file.path(root, "code/Stata/auxiliar/aux_general.do"))
  root
}

col_ecu_run <- function(root, last = 2024L) {
  stata <- col_ecu_stata()
  code <- readLines(file.path(repo_root_for_tests, "code/Stata/01b-add-country-sna.do"), warn = FALSE)
  start <- grep("^// Use the accepted recent releases", code)
  end <- grep("^// Rebuild Peru", code)
  first <- grep("^//first year", code)
  final <- grep("^//define labels", code)
  stopifnot(length(start) == 1L, length(end) == 1L,
            length(first) == 1L, length(final) == 1L)
  script <- c("clear all", "set more off", "global ctries_cei COL ECU",
    "global first_y 2014", paste("global last_y", last), "global list_ratios",
    code[seq.int(start, end - 1L)], code[seq.int(first, final - 1L)],
    "save result.dta, replace", "file open done using complete.txt, write replace",
    "file write done \"complete\" _n", "file close done", "exit, clear")
  writeLines(script, file.path(root, "handoff.do"))
  old <- setwd(root)
  on.exit(setwd(old))
  system2(stata, c("-b", "do", "handoff.do"), stdout = FALSE, stderr = FALSE)
  file.exists("complete.txt")
}

test_that("01b consumes accepted COL/ECU revisions and their 2024 sheets", {
  col_ecu_stata()
  root <- col_ecu_fixture()
  on.exit(unlink(root, recursive = TRUE))
  expect_true(col_ecu_run(root))
  if (!file.exists(file.path(root, "result.dta"))) {
    fail(paste(readLines(file.path(root, "handoff.log")), collapse = "\n"))
    return(invisible(NULL))
  }
  actual <- haven::read_dta(file.path(root, "result.dta"))
  engine <- new.env(parent = globalenv())
  sys.source(file.path(repo_root_for_tests, "code/R/source-diagnostics/country_sna_include.R"), engine)
  contract <- engine$country_sna_include_read_contract(repo_root_for_tests)
  core <- c("D11_cei", "D4_cei", "D7_cei", "D75_cei", "B2g_cei", "B3g_cei", "B5g_cei", "ratio_d43d44")
  for (country in c("COL", "ECU")) {
    for (year in c(2018L, 2022L, 2023L, 2024L)) {
      expected <- engine$country_sna_include_extract_year(country, year, contract, root)$values
      expected <- expected[match(core, expected$variable), ]
      if (country == "ECU") {
        # ECU repeats these account codes with zero placeholders. The existing
        # Stata reader treats zeros as missing before selecting household values;
        # the review reader instead flags these rows as duplicate conflicts.
        # Check the actual source cells independently under the existing rule.
        raw <- suppressMessages(readxl::read_excel(expected$source_file[[1L]],
          sheet = expected$sheet[[1L]], range = "A38:BD128", col_names = FALSE,
          .name_repair = "minimal"))
        for (variable in c("D11_cei", "B2g_cei", "B3g_cei")) {
          account <- c(D11_cei = "D.11", B2g_cei = "B.2b", B3g_cei = "B.3b")[[variable]]
          value <- as.numeric(raw[[51L]][trimws(raw[[1L]]) == account])
          value <- unique(value[is.finite(value) & value != 0])
          expect_length(value, 1L)
          expected$value_standardized[expected$variable == variable] <- value * 1000
        }
      }
      found <- actual[actual$country == country & actual$year == year, core]
      expect_equal(nrow(found), 1L)
      expect_true(all(is.finite(unlist(found))))
      # 01b deliberately stores these existing macro columns as Stata floats.
      expect_equal(as.numeric(found), expected$value_standardized,
        tolerance = 1e-6, info = paste(country, year))
    }
  }
  # Specifically prove historical values came from the revised release.
  col2023 <- actual$D11_cei[actual$country == "COL" & actual$year == 2023]
  ecu2023 <- actual$D11_cei[actual$country == "ECU" & actual$year == 2023]
  expect_gt(abs(col2023 / 4.39238e14 - 1), 1e-5)
  expect_gt(abs(ecu2023 / 3.665429e10 - 1), 1e-5)
})

test_that("legacy-only sources work through 2023 and cannot silently cover 2024", {
  col_ecu_stata()
  root <- col_ecu_fixture(updated = character())
  on.exit(unlink(root, recursive = TRUE))
  expect_true(col_ecu_run(root, 2023L))
  saved <- readBin(file.path(root, "result.dta"), "raw", n = file.info(file.path(root, "result.dta"))$size)
  unlink(file.path(root, "complete.txt"))
  expect_false(col_ecu_run(root, 2024L))
  expect_identical(readBin(file.path(root, "result.dta"), "raw", n = length(saved)), saved)
  expect_match(paste(readLines(file.path(root, "handoff.log")), collapse = "\n"),
    "required COL CEI table cannot be read")
})

test_that("missing Ecuador 2024 sheet fails before any macro publication", {
  col_ecu_stata()
  root <- col_ecu_fixture(updated = "COL")
  on.exit(unlink(root, recursive = TRUE))
  expect_false(col_ecu_run(root, 2024L))
  expect_false(file.exists(file.path(root, "result.dta")))
  expect_match(paste(readLines(file.path(root, "handoff.log")), collapse = "\n"),
    "required ECU CEI table cannot be read")
})
