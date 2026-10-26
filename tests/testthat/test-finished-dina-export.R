# Exercise the actual 07d overlay and cleanup blocks against bounded fixtures.
# These tests never invoke 07d on production inputs or publish final exports.
finished_export_fixture <- function(country = "TST", years = 1999:2002,
    step = "nat", shape = 0.8, plateau = TRUE) {
  p <- round(c(seq(0, 0.99, 0.01), seq(0.991, 0.999, 0.001),
    seq(0.9991, 0.9999, 0.0001), seq(0.99991, 0.99999, 0.00001)) * 1e5) / 1e5
  mass <- diff(c(p, 1))
  avg <- seq(0.5, 20, length.out = length(p)) ^ shape
  if (plateau) avg[50:52] <- sum(avg[50:52] * mass[50:52]) / sum(mass[50:52])
  avg <- avg / sum(avg * mass)
  thr <- c(0, (head(avg, -1) + tail(avg, -1)) / 2)
  shares <- mass * avg
  bottom <- c(0, head(cumsum(shares), -1L))
  bottomavg <- numeric(length(p)); bottomavg[p > 0] <- bottom[p > 0] / p[p > 0]
  gini <- 1 - sum(mass * (c(0, head(cumsum(shares), -1L)) + cumsum(shares)))
  do.call(rbind, lapply(years, function(year) data.frame(country = country,
    year = year, step = step, unit = "esn", p = p, avg = avg, thr = thr,
    average = 1, bckt_sh = shares, topavg = rev(cumsum(rev(shares))) / (1 - p),
    topsh = rev(cumsum(rev(shares))), bottomsh = bottom, bottomavg = bottomavg,
    origin = if (year <= 2000) "supplied" else if (year == 2001) "continuation" else "carry_forward",
    source_id = "generic-series", citation = "Fixture author", doi = "fixture-doi",
    source_version = "v1", adjustment = if (year <= 2000) "none" else "group_join", anchor_year = 2000,
    supplied_gini = if (year <= 2000) 0.4 else NA_real_, grouped_gini = gini)))
}

finished_export_helper <- function() {
  env <- new.env(parent = globalenv())
  sys.source(file.path(repo_root_for_tests, "code", "R", "07d_prepare_finished_dina.R"), env)
  env$dina_finished_stata_files
}

finished_export_config <- function() list(countries = c("TST", "ABC"),
  export_validation = list(steps = c("nat", "pon"), unit = "esn"))

testthat::test_that("07d refreshes approved continuations before freezing comparison metadata", {
  production <- readLines(file.path(repo_root_for_tests, "code", "Stata", "07d-export-results-to-wid.do"))
  refresh <- which(grepl('07d_prepare_finished_dina.R', production, fixed = TRUE))
  metadata <- which(grepl('export-comparison-metadata.R begin', production, fixed = TRUE))
  scaling <- which(grepl('*scale to national income level', production, fixed = TRUE))
  testthat::expect_length(refresh, 1L)
  testthat::expect_length(metadata, 1L)
  testthat::expect_lt(refresh, metadata)
  testthat::expect_lt(metadata, scaling)
  erase <- which(grepl("cap erase \"`dina_finished_dir'/", production, fixed = TRUE))
  confirm <- which(grepl("confirm file \"`dina_finished_dir'/", production, fixed = TRUE))
  testthat::expect_length(erase, 2L)
  testthat::expect_true(all(erase < refresh))
  testthat::expect_true(all(confirm > refresh & confirm < metadata))
  adapter <- readLines(file.path(repo_root_for_tests, "code", "R", "07d_prepare_finished_dina.R"))
  testthat::expect_true(any(grepl('dina_finished_publication_ensure(root, config)', adapter, fixed = TRUE)))
})

testthat::test_that("07d adapter writes empty inactive inputs and validates active generic contributions", {
  testthat::skip_if_not_installed("haven")
  root <- tempfile("finished-export-"); dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  helper <- finished_export_helper()
  empty <- helper(list(data = data.frame()), finished_export_config(), root)
  testthat::expect_equal(nrow(empty$overlay), 0)
  testthat::expect_equal(nrow(haven::read_dta(file.path(root, "overlay.dta"))), 0)
  testthat::expect_equal(nrow(haven::read_dta(file.path(root, "attribution.dta"))), 0)
  active <- helper(list(data = finished_export_fixture()), finished_export_config(), root)
  testthat::expect_equal(nrow(active$overlay), 4 * 127)
  testthat::expect_equal(nrow(active$attribution), 4)
  testthat::expect_equal(sort(unique(active$overlay$p)), sort(unique(round(finished_export_fixture()$p * 1e5))))
  testthat::expect_equal(active$attribution$pub_origin, c("supplied", "supplied", "continuation", "carry_forward"))
  testthat::expect_equal(active$attribution$pub_adjustment, c("none", "none", "group_join", "group_join"))
  overlap <- finished_export_fixture(); overlap <- rbind(overlap, overlap[1, ])
  testthat::expect_error(helper(list(data = overlap), finished_export_config(), root), "overlap")
  other <- finished_export_fixture("ZZZ")
  testthat::expect_error(helper(list(data = other), finished_export_config(), root), "outside")
})

testthat::test_that("actual Stata overlay preserves approved shape, other countries, and donor use", {
  stata <- Sys.getenv("DINA_STATA_CMD", unset = "/Applications/Stata/StataMP.app/Contents/MacOS/stata-mp")
  testthat::skip_if(!file.exists(stata), "Stata is not installed")
  testthat::skip_if_not_installed("processx"); testthat::skip_if_not_installed("haven")
  root <- tempfile("finished-stata-export-"); dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  helper <- finished_export_helper()
  candidate <- finished_export_fixture()
  overlay <- helper(list(data = candidate), finished_export_config(), file.path(root, "approved"))
  # Existing distributions cover configured years only, in two countries and
  # two concepts. The incoming source replaces only TST/nat and adds 1999 there.
  internal <- do.call(rbind, lapply(c("TST", "ABC"), function(country)
    do.call(rbind, lapply(c("nat", "pon"), function(step)
      finished_export_fixture(country, 2000:2002, step,
        shape = if (country == "TST") 1.2 else 1.1, plateau = FALSE)))))
  base <- helper(list(data = internal), finished_export_config(), file.path(root, "base"))$overlay
  base$finished_dina <- NULL; base$imputed <- NULL
  base$bottomsh <- NULL; base$bottomavg <- NULL
  base$check2 <- 2
  base$data_points <- ""; base$extrap_points <- ""; base$interp_points <- ""
  haven::write_dta(base, file.path(root, "base.dta"), version = 14)
  wid <- expand.grid(iso = c("TST", "ABC"), year = 1999:2002, stringsAsFactors = FALSE)
  wid$anninc992i <- 1000 + wid$year
  haven::write_dta(wid, file.path(root, "wid.dta"), version = 14)
  production <- readLines(file.path(repo_root_for_tests, "code", "Stata", "07d-export-results-to-wid.do"), warn = FALSE)
  section <- function(start, stop, after = 0L) {
    first <- which(grepl(start, production, fixed = TRUE) & seq_along(production) > after)[[1L]]
    end <- which(grepl(stop, production, fixed = TRUE) & seq_along(production) > first)[[1L]]
    production[first:(end - 1L)]
  }
  rectangle <- section("*rectangularize", "*interpolate within countries")
  overlay_block <- section("// Overlay after fillin", "*get bracket population share")
  bracket <- section("*get bracket population share", "*global all_ctries")
  cleanup_start <- which(grepl("// Accepted contributions already passed validation.", production, fixed = TRUE))[[1L]]
  cleanup <- section("// Accepted contributions already passed validation.", "*harmonise country names", after = cleanup_start - 1L)
  collapse <- section("*collapse normalized bracket averages", "tab `check2'")
  donor_notes <- section("// Carry the actual donor pool's supplied-source attribution.", 'local dina_expected ""')
  scaling <- section("*scale to national income level", "//create main folders")
  export_grid <- section("qui replace p = p / 1000", "qui save `main', replace")
  bottom_share <- section("*Bottom", "*Bottom avg")
  bottom_average <- section("*Bottom avg", "append using `top'")
  # Guard the essential ordering even if production sections are reorganized.
  testthat::expect_lt(which(grepl("qui fillin country year step p", production, fixed = TRUE))[[1L]],
    which(grepl('do "code/Stata/auxiliar/dina_finished_overlay.do"', production, fixed = TRUE))[[1L]])
  testthat::expect_lt(which(grepl('do "code/Stata/auxiliar/dina_finished_overlay.do"', production, fixed = TRUE))[[1L]],
    which(grepl("*III. Compute regional averages", production, fixed = TRUE))[[1L]])
  overlay_do <- normalizePath(file.path(repo_root_for_tests, "code", "Stata", "auxiliar", "dina_finished_overlay.do"))
  overlay_block <- gsub("code/Stata/auxiliar/dina_finished_overlay.do", overlay_do, overlay_block, fixed = TRUE)
  # Two runs of the same cleanup: a control and the approved-source variant.
  # The output equality check isolates the overlay's effect on other countries.
  run <- function(active) {
    count <- if (active) nrow(overlay$overlay) else 0L
    stem <- if (active) "active" else "control"
    donor <- if (active) c("preserve", "keep if step == \"nat\" & year >= 2000", "gen byte donor_tst = country == \"TST\"",
      "gen byte donor_abc = country == \"ABC\"", donor_notes, 'local dina_availability "donor_tst donor_abc"', collapse,
      "merge m:1 year step using `dina_donor_notes', keep(master match) nogen",
      'assert strpos(pub_donor_note, "generic-series") > 0',
      'save "donors.dta", replace', "restore") else character()
    commands <- c("clear all", "set more off", "set type float", 'use "base.dta", clear',
      # haven writes doubles; use production-style legacy floats so appending
      # the accepted double contribution exercises Stata's type promotion.
      "recast float p bracketavg bracketavg_norm s thr topavg topsh, force",
      "local ly = 2002", 'local fw "nat"', sprintf("local dina_finished_count = %d", count),
      sprintf('local dina_finished_dir "%s"', file.path(root, "approved")),
      rectangle, "rename _fillin imputed", overlay_block,
      sprintf('save "%s-before.dta", replace', stem),
      "assert year >= 2000 if country != \"TST\" | step != \"nat\"", bracket, donor,
      cleanup, sprintf('save "%s-after.dta", replace', stem),
      'gen str3 iso = country', 'local tf_ni_wid "wid.dta"', scaling,
      sprintf('save "%s-scaled.dta", replace', stem),
      export_grid, "tempfile main", "save `main'",
      sprintf('save "%s-export-grid.dta", replace', stem),
      bottom_share, bottom_average,
      "use `bottom', clear", sprintf('save "%s-bottom-shares.dta", replace', stem),
      "use `bottomavg', clear", sprintf('save "%s-bottom-averages.dta", replace', stem),
      sprintf('file open passed using "%s-passed.txt", write replace', stem),
      'file write passed "passed"', "file close passed")
    path <- file.path(root, paste0(stem, ".do")); writeLines(commands, path)
    processx::run(stata, c("-b", "do", basename(path)), wd = root, timeout = 60000, error_on_status = FALSE)
    log <- readLines(file.path(root, paste0(stem, ".log")), warn = FALSE)
    testthat::expect_true(file.exists(file.path(root, paste0(stem, "-passed.txt"))),
      info = paste(tail(log, 35), collapse = "\n"))
  }
  run(FALSE); run(TRUE)
  sort_rows <- function(x) {
    x <- as.data.frame(x)
    x <- x[do.call(order, x[c("country", "year", "step", "p")]), , drop = FALSE]
    rownames(x) <- NULL
    x
  }
  before <- sort_rows(haven::read_dta(file.path(root, "active-before.dta")))
  after <- sort_rows(haven::read_dta(file.path(root, "active-after.dta")))
  control <- sort_rows(haven::read_dta(file.path(root, "control-after.dta")))
  protected <- sort_rows(subset(after, country == "TST" & step == "nat"))
  expected <- sort_rows(overlay$overlay)
  fields <- c("country", "year", "step", "p", "bracketavg", "bracketavg_norm", "thr", "s", "topavg", "topsh", "bottomsh", "bottomavg")
  testthat::expect_equal(protected[fields], expected[fields], tolerance = 1e-13)
  testthat::expect_true(any(diff(subset(protected, year == 2001)$bracketavg_norm) == 0))
  retained <- sort_rows(subset(after, country != "TST" | step != "nat"))
  retained_control <- sort_rows(subset(control, country != "TST" | step != "nat"))
  common <- intersect(names(retained), names(retained_control))
  # Generated Stata temporary-variable names differ between control and active.
  common <- common[!grepl("^__", common)]
  testthat::expect_equal(retained[common], retained_control[common], tolerance = 0)
  testthat::expect_equal(sort(unique(subset(after, country == "ABC")$year)), 2000:2002)
  testthat::expect_equal(sort(unique(protected$year)), 1999:2002)
  testthat::expect_equal(nrow(protected), nrow(expected))
  original_before <- sort_rows(subset(before, country != "TST" | step != "nat"))
  original_base <- sort_rows(subset(haven::read_dta(file.path(root, "control-before.dta")), country != "TST" | step != "nat"))
  base_fields <- intersect(names(original_base), names(original_before))
  testthat::expect_equal(original_before[base_fields], original_base[base_fields], tolerance = 0)
  # Compare actual published-value transformations too: promotion can leave
  # cleanup unchanged but alter the rounding of subsequent WID multiplication.
  for (stage in c("scaled", "export-grid", "bottom-shares", "bottom-averages")) {
    selected <- sort_rows(subset(haven::read_dta(file.path(root, paste0("active-", stage, ".dta"))), country != "TST" | step != "nat"))
    unchanged <- sort_rows(subset(haven::read_dta(file.path(root, paste0("control-", stage, ".dta"))), country != "TST" | step != "nat"))
    common_fields <- intersect(names(selected), names(unchanged))
    common_fields <- common_fields[!grepl("^__", common_fields)]
    testthat::expect_equal(selected[common_fields], unchanged[common_fields], tolerance = 0, info = stage)
  }
  donor <- as.data.frame(haven::read_dta(file.path(root, "donors.dta")))
  donor <- donor[order(donor$year, donor$p), ]
  approved <- subset(expected, year >= 2000)
  abc <- subset(original_base, country == "ABC" & step == "nat")
  expected_donor <- merge(approved[c("year", "p", "bracketavg_norm")],
    abc[c("year", "p", "bracketavg_norm")], by = c("year", "p"))
  expected_donor <- expected_donor[order(expected_donor$year, expected_donor$p), ]
  testthat::expect_equal(donor$bracketavg, (expected_donor$bracketavg_norm.x + expected_donor$bracketavg_norm.y) / 2, tolerance = 1e-13)
  exported_means <- as.data.frame(haven::read_dta(file.path(root, "active-bottom-averages.dta")))
  exported_shares <- as.data.frame(haven::read_dta(file.path(root, "active-bottom-shares.dta")))
  scaled <- as.data.frame(haven::read_dta(file.path(root, "active-scaled.dta")))
  for (year in 1999:2002) {
    group <- protected[protected$year == year, ]
    original <- expected[expected$year == year, ]
    group_id <- cut(group$p, c(-Inf, 50000, 90000, 99000, Inf), right = FALSE)
    original_group_id <- cut(original$p, c(-Inf, 50000, 90000, 99000, Inf), right = FALSE)
    testthat::expect_equal(tapply(group$s, group_id, sum),
      tapply(original$s, original_group_id, sum), tolerance = 1e-13)
    testthat::expect_equal(unique(group$pub_adjustment), if (year <= 2000) "none" else "group_join")
    supplied_bottom50 <- sum(group$s[group$p < 50000])
    testthat::expect_equal(group$bottomsh[group$p == 50000], supplied_bottom50, tolerance = 1e-13)
    testthat::expect_gt(abs(group$bottomsh[group$p == 50000] - sum(group$s[group$p <= 50000])), 1e-4)
    monetary <- scaled[scaled$country == "TST" & scaled$step == "nat" & scaled$year == year, ]
    monetary <- monetary[order(monetary$p), ]
    testthat::expect_equal(monetary$bottomavg, group$bottomavg * (1000 + year), tolerance = 1e-12)
    selected_mean <- exported_means[exported_means$country == "TST" & exported_means$step == "nat" & exported_means$year == year, ]
    selected_share <- exported_shares[exported_shares$country == "TST" & exported_shares$step == "nat" & exported_shares$year == year, ]
    testthat::expect_equal(selected_share$value[selected_share$p == "p0p50"], supplied_bottom50, tolerance = 1e-13)
    testthat::expect_equal(selected_mean$value[selected_mean$p == "p0p50"], supplied_bottom50 / 0.5 * (1000 + year), tolerance = 1e-12)
    middle <- sum(group$s[group$p >= 50000 & group$p < 90000])
    testthat::expect_equal(selected_share$value[selected_share$p == "p50p90"], middle, tolerance = 1e-13)
    testthat::expect_equal(selected_mean$value[selected_mean$p == "p50p90"], middle / 0.4 * (1000 + year), tolerance = 1e-12)
  }
})

testthat::test_that("Stata combines BRA and ECU history without backcasts and scales their approved shapes once", {
  stata <- Sys.getenv("DINA_STATA_CMD", unset = "/Applications/Stata/StataMP.app/Contents/MacOS/stata-mp")
  testthat::skip_if(!file.exists(stata), "Stata is not installed")
  testthat::skip_if_not_installed("processx"); testthat::skip_if_not_installed("haven")
  root <- tempfile("finished-bra-ecu-"); dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  config <- list(countries = c("BRA", "ECU", "COL"), export_validation = list(steps = c("nat", "pon"), unit = "esn"))
  bra <- finished_export_fixture("BRA", c(1976, 1990, 1992, 2015:2025), shape = .7)
  bra$source_id <- "bra-supplied"; bra$anchor_year <- 2015
  bra$origin <- ifelse(bra$year <= 2015, "supplied", ifelse(bra$year == 2025, "carry_forward", "continuation"))
  bra$supplied_gini <- NA_real_; bra$fitted_gini <- ifelse(bra$origin == "supplied", .5, NA_real_)
  bra$gini_method <- ifelse(bra$origin == "supplied", "gpinter_fitted", "grouped_lorenz_trapezoid")
  ecu <- finished_export_fixture("ECU", c(1990, 2000, 2015:2025), shape = .9)
  ecu$source_id <- "ecu-supplied"; ecu$anchor_year <- 2022
  ecu$origin <- ifelse(ecu$year <= 2022, "supplied", ifelse(ecu$year == 2025, "carry_forward", "continuation"))
  ecu$fitted_gini <- ifelse(ecu$origin == "supplied", .51, NA_real_)
  ecu$supplied_gini <- ifelse(ecu$origin == "supplied", .4, NA_real_)
  ecu$gini_method <- ifelse(ecu$origin == "supplied", "supplied", "grouped_lorenz_trapezoid")
  helper <- finished_export_helper()
  approved <- helper(list(data = rbind(bra, ecu)), config, file.path(root, "approved"))
  internal <- do.call(rbind, lapply(config$countries, function(country) do.call(rbind,
    lapply(c("nat", "pon"), function(step) finished_export_fixture(country, c(2000, 2015, 2024, 2025), step, shape = 1.1)))))
  base <- helper(list(data = internal), config, file.path(root, "base"))$overlay
  base$finished_dina <- NULL
  haven::write_dta(base, file.path(root, "base.dta"), version = 14)
  overlay <- normalizePath(file.path(repo_root_for_tests, "code/Stata/auxiliar/dina_finished_overlay.do"))
  commands <- c("clear all", "set more off", 'use "base.dta", clear',
    sprintf('do "%s" "%s"', overlay, file.path(root, "approved/overlay.dta")),
    sprintf('merge m:1 country year step using "%s", keep(master match) nogen', file.path(root, "approved/attribution.dta")),
    'assert year >= 2000 if country == "COL" | step == "pon"',
    'assert year != 1991 & year != 1994 if country == "BRA"',
    'assert missing(pub_supplied_gini) if pub_source == "bra-supplied"',
    'assert pub_gini_method == "gpinter_fitted" if pub_source == "bra-supplied" & pub_origin == "supplied"',
    'save "combined.dta", replace', "preserve", 'keep if year == 2015 & step == "nat"',
    "collapse (mean) bracketavg_norm (count) donors=bracketavg, by(year p)", "assert donors == 3",
    'save "donors.dta", replace', "restore", "gen double wid_mean = 1000 + year",
    "foreach var in bracketavg thr topavg bottomavg {", "replace `var' = `var' * wid_mean", "}",
    'save "scaled.dta", replace', 'file open passed using "passed.txt", write replace',
    'file write passed "passed"', "file close passed")
  writeLines(commands, file.path(root, "combined.do"))
  processx::run(stata, c("-b", "do", "combined.do"), wd = root, timeout = 60000, error_on_status = FALSE)
  log <- readLines(file.path(root, "combined.log"), warn = FALSE)
  testthat::expect_true(file.exists(file.path(root, "passed.txt")), info = paste(tail(log, 30), collapse = "\n"))
  if (!file.exists(file.path(root, "passed.txt"))) return(invisible(NULL))
  sort_rows <- function(d) {d <- as.data.frame(d); d <- d[do.call(order, d[c("country", "year", "step", "p")]), ]; rownames(d) <- NULL; d}
  combined <- sort_rows(haven::read_dta(file.path(root, "combined.dta")))
  changed <- combined$finished_dina == 1
  wanted <- sort_rows(approved$overlay)
  fields <- c("country", "year", "step", "p", "bracketavg", "bracketavg_norm", "s", "thr", "topavg", "topsh")
  testthat::expect_equal(sort_rows(combined[changed, ])[fields], wanted[fields], tolerance = 1e-13)
  kept <- sort_rows(combined[!changed, ])
  replacement_keys <- unique(paste(wanted$country, wanted$year, wanted$step))
  unchanged <- sort_rows(base[!paste(base$country, base$year, base$step) %in% replacement_keys, ])
  testthat::expect_equal(kept[fields], unchanged[fields], tolerance = 0)
  donors <- as.data.frame(haven::read_dta(file.path(root, "donors.dta")))
  donors <- donors[order(donors$p), ]
  shares <- subset(combined, year == 2015 & step == "nat")
  expected <- aggregate(bracketavg_norm ~ p, shares, mean)
  testthat::expect_equal(donors$bracketavg_norm, expected$bracketavg_norm, tolerance = 1e-13)
  scaled <- sort_rows(haven::read_dta(file.path(root, "scaled.dta")))
  testthat::expect_equal(scaled$bracketavg, combined$bracketavg * (1000 + combined$year), tolerance = 1e-12)
  testthat::expect_equal(scaled$s, combined$s, tolerance = 0)
})
