testthat::test_that("actual Stata export aggregates use exact boundaries and common monetary units", {
  stata <- Sys.getenv("DINA_STATA_CMD", unset = "/Applications/Stata/StataMP.app/Contents/MacOS/stata-mp")
  testthat::skip_if(!file.exists(stata), "Stata is not installed")
  testthat::skip_if_not_installed("haven")
  testthat::skip_if_not_installed("processx")
  root <- tempfile("export-aggregates-"); dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  report <- Sys.getenv("DINA_AGGREGATE_TEST_REPORT_DIR")
  if (nzchar(report) && !grepl("^(/|[A-Za-z]:)", report)) report <- file.path(repo_root_for_tests, report)
  p <- round(c(seq(0, .99, .01), seq(.991, .999, .001),
    seq(.9991, .9999, .0001), seq(.99991, .99999, .00001)) * 1e5)
  mass <- diff(c(p, 1e5)) / 1e5
  # Clear jumps at both boundaries expose an inclusive-cumulative error.
  avg <- ifelse(p < 50000, .2, ifelse(p < 90000, 1, 5))
  avg <- avg / sum(avg * mass)
  s <- mass * avg
  bottom <- c(0, head(cumsum(s), -1))
  make_case <- function(country, year) data.frame(country = country, year = year,
    step = "nat", p = p, bracketpop = mass * 1e5, bracketavg = avg,
    bracketavg_norm = avg, s = s, thr = c(0, head(avg, -1)),
    topsh = rev(cumsum(rev(s))), topavg = rev(cumsum(rev(s))) / (1 - p / 1e5),
    bottomsh = bottom, bottomavg = ifelse(p == 0, 0, bottom / (p / 1e5)),
    finished_dina = as.integer(country == "SUP"),
    pub_source = if (country == "SUP") "approved-fixture" else "",
    pub_origin = if (country == "SUP") "supplied" else "",
    data_points = "[2024]", extrap_points = "[2025]", interp_points = "")
  input <- do.call(rbind, lapply(c("INT", "SUP"), function(c)
    do.call(rbind, lapply(2024:2025, function(y) make_case(c, y)))))
  haven::write_dta(input, file.path(root, "input.dta"))
  # The absent 2025 WID benchmark must leave monetary values missing, while
  # retaining valid distribution shares for the carry-forward year.
  haven::write_dta(data.frame(iso = c("INT", "SUP"), year = 2024,
    anninc992i = c(10000, 20000)), file.path(root, "wid.dta"))
  code <- readLines(file.path(repo_root_for_tests, "code/Stata/07d-export-results-to-wid.do"))
  block <- function(start, end) {
    a <- which(grepl(start, code, fixed = TRUE))[[1]]
    b <- which(grepl(end, code, fixed = TRUE) & seq_along(code) > a)[[1]]
    code[a:(b - 1L)]
  }
  run_stata <- function(name, commands, success = TRUE) {
    writeLines(c("clear all", "set more off", "set type float", commands,
      sprintf('file open done using "%s.done", write replace', name),
      'file write done "done"', "file close done"), file.path(root, paste0(name, ".do")))
    processx::run(stata, c("-b", "do", paste0(name, ".do")), wd = root,
      timeout = 60000, error_on_status = FALSE)
    log <- readLines(file.path(root, paste0(name, ".log")), warn = FALSE)
    testthat::expect_identical(file.exists(file.path(root, paste0(name, ".done"))), success,
      info = paste(tail(log, 35), collapse = "\n"))
    if (nzchar(report)) {
      dir.create(report, recursive = TRUE, showWarnings = FALSE)
      file.copy(file.path(root, paste0(name, ".log")), file.path(report, paste0(name, ".log")), overwrite = TRUE)
    }
    invisible(log)
  }
  run_stata("aggregate", c('use "input.dta", clear', "preserve",
    "keep if finished_dina == 1", "tempfile supplied", "save `supplied'", "restore",
    "drop if finished_dina == 1", "drop bottomsh bottomavg",
    block("*compute bottom shares", "// Restore exactly the validated normalized distribution"),
    "append using `supplied'", 'gen str3 iso = country', 'local tf_ni_wid "wid.dta"',
    "local dina_finished_count = 254", 'local dina_scale_floatvars ""',
    block("*scale to national income level", "//create main folders"),
    'save "scaled.dta", replace', 'local dina_legacy_p_type "double"',
    block("qui replace p = p / 1000", "qui save `main', replace"),
    "tempfile main", "save `main'", block("*Bottom", "*Bottom avg"),
    block("*Bottom avg", "append using `top'"),
    "use `bottom', clear", 'save "shares.dta", replace',
    "use `bottomavg', clear", 'save "means.dta", replace'))
  scaled <- as.data.frame(haven::read_dta(file.path(root, "scaled.dta")))
  shares <- as.data.frame(haven::read_dta(file.path(root, "shares.dta")))
  means <- as.data.frame(haven::read_dta(file.path(root, "means.dta")))
  wanted <- c(p0p20 = sum(s[p < 20000]), p0p50 = sum(s[p < 50000]),
    p0p90 = sum(s[p < 90000]), p50p90 = sum(s[p >= 50000 & p < 90000]))
  widths <- c(p0p20 = .2, p0p50 = .5, p0p90 = .9, p50p90 = .4)
  for (country in c("INT", "SUP")) for (year in 2024:2025) {
    d <- scaled[scaled$country == country & scaled$year == year, ]; d <- d[order(d$p), ]
    z <- shares[shares$country == country & shares$year == year, ]
    a <- means[means$country == country & means$year == year, ]
    testthat::expect_equal(z$value, unname(wanted[z$p]), tolerance = 1e-12)
    testthat::expect_equal(sum(z$value[z$p %in% c("p0p50", "p50p90")]) + sum(s[p >= 90000]), 1)
    testthat::expect_equal(d$bottomsh, bottom, tolerance = 1e-12)
    testthat::expect_equal(d$s, s, tolerance = 0)
    testthat::expect_equal(d$topsh, rev(cumsum(rev(s))), tolerance = 0)
    testthat::expect_equal(d$pub_source, rep(if (country == "SUP") "approved-fixture" else "", 127))
    if (year == 2024) {
      benchmark <- if (country == "INT") 10000 else 20000
      testthat::expect_equal(a$value, unname(wanted[a$p] / widths[a$p] * benchmark), tolerance = 1e-12)
      testthat::expect_equal(d$bracketavg, avg * benchmark, tolerance = 0)
      testthat::expect_equal(d$thr, c(0, head(avg, -1)) * benchmark, tolerance = 0)
    } else {
      testthat::expect_true(all(is.na(a$value)))
      testthat::expect_true(all(is.na(d$bracketavg)))
      testthat::expect_true(all(is.na(d$bottomavg)))
    }
  }
  # Construct an archived-style long file with deliberately wrong aggregates.
  # The helper must correct only those rows, never the archived file itself.
  label <- function(x) vapply(x, function(z)
    format(z, trim = TRUE, scientific = FALSE, digits = 8), character(1))
  interval <- paste0("p", label(p / 1000), "p", label(c(tail(p, -1), 1e5) / 1000))
  historical <- do.call(rbind, lapply(c("INT", "SUP"), function(country)
    do.call(rbind, lapply(2024:2025, function(year) {
      mu <- if (year == 2025) NA_real_ else if (country == "INT") 10000 else 20000
      rows <- rbind(data.frame(p = interval, widcode = "sptinc992j", value = s),
        data.frame(p = interval, widcode = "aptinc992j", value = avg * mu),
        data.frame(p = interval, widcode = "tptinc992j", value = c(0, head(avg, -1)) * mu),
        data.frame(p = c("p0p100", "p90p100", "p99p100"), widcode = "sptinc992j",
          value = c(1, sum(s[p >= 90000]), sum(s[p >= 99000]))),
        data.frame(p = "p0p100", widcode = "aptinc992j", value = mu),
        data.frame(p = names(wanted), widcode = "sptinc992j", value = .123),
        data.frame(p = c("p0p50", "p0p90", "p50p90"), widcode = "aptinc992j", value = .456))
      rows$country <- country; rows$year <- year; rows$comment <- "untouched source attribution"
      rows
    }))))
  archived <- file.path(root, "archived.dta"); haven::write_dta(historical, archived)
  archived_before <- tools::md5sum(archived)
  helper <- normalizePath(file.path(repo_root_for_tests, "code/Stata/auxiliar/dina_export_comparison_aggregates.do"))
  run_stata("comparison", c('use "archived.dta", clear', sprintf('do "%s"', helper),
    'save "comparison.dta", replace'))
  testthat::expect_identical(tools::md5sum(archived), archived_before)
  comparison <- as.data.frame(haven::read_dta(file.path(root, "comparison.dta")))
  joined <- merge(historical, comparison, by = c("country", "year", "p", "widcode"), suffixes = c("_old", "_new"))
  changed <- joined$p %in% names(wanted) & substr(joined$widcode, 1, 1) %in% c("s", "a")
  testthat::expect_equal(joined$value_new[!changed], joined$value_old[!changed], tolerance = 0)
  testthat::expect_identical(joined$comment_new, joined$comment_old)
  testthat::expect_equal(nrow(comparison), nrow(historical))
  z <- comparison[comparison$p %in% names(wanted) & comparison$widcode == "sptinc992j", ]
  testthat::expect_equal(z$value, unname(wanted[z$p]), tolerance = 1e-12)
  z <- comparison[comparison$p %in% names(wanted) & comparison$widcode == "aptinc992j", ]
  benchmark <- ifelse(z$year == 2025, NA_real_, ifelse(z$country == "INT", 10000, 20000))
  testthat::expect_equal(z$value, unname(wanted[z$p] / widths[z$p]) * benchmark, tolerance = 1e-12)
  bad <- historical[!(historical$country == "INT" & historical$year == 2024 &
    historical$p == "p50p51" & historical$widcode == "sptinc992j"), ]
  testthat::expect_equal(nrow(bad), nrow(historical) - 1L)
  haven::write_dta(bad, file.path(root, "incomplete.dta"))
  log <- run_stata("incomplete", c('use "incomplete.dta", clear', sprintf('do "%s"', helper)), success = FALSE)
  testthat::expect_true(any(grepl("complete detailed p0-p90 brackets", log, fixed = TRUE)))
  fractional <- historical
  fractional$p[fractional$country == "INT" & fractional$year == 2024 &
    fractional$p == "p50p51" & fractional$widcode == "sptinc992j"] <- "p50.5p51.5"
  haven::write_dta(fractional, file.path(root, "fractional.dta"))
  log <- run_stata("fractional", c('use "fractional.dta", clear', sprintf('do "%s"', helper)), success = FALSE)
  testthat::expect_true(any(grepl("complete detailed p0-p90 brackets", log, fixed = TRUE)))
  if (nzchar(report)) {
    # Small tabulations only: independent review evidence, never microdata.
    utils::write.csv(scaled, file.path(report, "fixture-wide-scaled.csv"), row.names = FALSE)
    utils::write.csv(shares, file.path(report, "fixture-bottom-shares.csv"), row.names = FALSE)
    utils::write.csv(means, file.path(report, "fixture-bottom-means.csv"), row.names = FALSE)
    utils::write.csv(comparison, file.path(report, "fixture-comparison.csv"), row.names = FALSE)
    utils::write.csv(historical, file.path(report, "fixture-archived.csv"), row.names = FALSE)
    utils::write.csv(haven::read_dta(file.path(root, "wid.dta")), file.path(report, "fixture-wid.csv"), row.names = FALSE)
  }
})
