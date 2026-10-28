source(file.path(repo_root_for_tests, "code/R/export-income-means.R"))

testthat::test_that("disposable monetary means retain observed ratios and actual donor pools", {
  countries <- c("ARG", "ECU", "URY", "CRI")
  means <- expand.grid(country = countries, year = c(2020L, 2024L),
    step = c("nat", "pod"), stringsAsFactors = FALSE)
  means$unit <- "esn"; means$variable <- "average"; means$group <- "tot"
  means$value <- 100 * ifelse(means$step == "nat", 1,
    match(means$country, countries) / 10 + (means$year - 2020) / 100)
  targets <- expand.grid(country = c(countries, "CUB", "BOL"), year = 2019:2025,
    step = c("nat", "pon", "pod"), stringsAsFactors = FALSE)
  result <- export_income_mean_ratios(means, targets, "esn", countries)
  testthat::expect_true(all(result$income_mean_ratio[result$step != "pod"] == 1))
  get <- function(c, y) result$income_mean_ratio[result$country == c & result$year == y & result$step == "pod"]
  testthat::expect_equal(get("ARG", 2019), .1)
  testthat::expect_equal(get("ARG", 2022), .12)
  testthat::expect_equal(get("ARG", 2025), .14)
  testthat::expect_equal(get("BOL", 2022), .22) # excludes CRI, exactly like distributions
  testthat::expect_equal(get("CUB", 2022), .22)
  observed <- means[means$step == "pod", c("country", "year", "step")]
  observed <- observed[!(observed$country == "ARG" & observed$year == 2020), ]
  limited <- export_income_mean_ratios(means, targets, "esn", countries, observed)
  testthat::expect_equal(limited$income_mean_ratio[limited$country == "ARG" & limited$step == "pod"], rep(.14, 7))
  testthat::expect_error(export_income_mean_ratios(means[!(means$country == "ARG" & means$step == "nat"), ],
    targets, "esn", countries), "matching internal")
  testthat::expect_error(export_income_mean_ratios(means[means$country != "ARG", ],
    targets, "esn", countries), "Missing disposable/national means for ARG")
  testthat::expect_error(export_income_mean_ratios(rbind(means, means[1, ]), targets, "esn", countries), "Duplicate")
  broken <- means; broken$value[1] <- Inf
  testthat::expect_error(export_income_mean_ratios(broken, targets, "esn", countries), "finite and positive")
})

testthat::test_that("actual Stata exports multiple concepts without pooled means or metadata", {
  stata <- Sys.getenv("DINA_STATA_CMD", unset = "/Applications/Stata/StataMP.app/Contents/MacOS/stata-mp")
  testthat::skip_if(!file.exists(stata), "Stata is not installed")
  testthat::skip_if_not_installed("haven"); testthat::skip_if_not_installed("processx")
  root <- tempfile("export-disposable-"); dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  # Only code is linked; every input and output is an isolated tiny fixture.
  stopifnot(file.symlink(file.path(repo_root_for_tests, "code"), file.path(root, "code")))
  dir.create(file.path(root, "output/synthetic_microfiles"), recursive = TRUE)
  dir.create(file.path(root, "output/latest_wid_series"), recursive = TRUE)
  countries <- c("ARG", "ECU", "URY", "CRI")
  p <- round(c(seq(0, .99, .01), seq(.991, .999, .001),
    seq(.9991, .9999, .0001), seq(.99991, .99999, .00001)) * 1e5)
  mass <- diff(c(p, 1e5)) / 1e5
  make_case <- function(country, step) {
    mu <- c(nat = 200, pon = 300, pod = 80)[step]
    avg <- (1 + seq_along(p)) ^ c(nat = 1.5, pon = .8, pod = .6)[step]
    avg <- avg / sum(avg * mass); shares <- avg * mass
    data.frame(country, year = 2024L, step, p, iso = country, bracketpop = mass * 1e5,
      bracketavg = avg * mu, s = shares, thr = c(0, head(avg, -1)) * mu,
      topavg = rev(cumsum(rev(shares))) / (1 - p / 1e5) * mu,
      topsh = rev(cumsum(rev(shares))), data_points = paste0("[", step, "]"),
      extrap_points = "", interp_points = "")
  }
  input <- do.call(rbind, lapply(countries, function(c)
    do.call(rbind, lapply(c("nat", "pon", "pod"), function(s) make_case(c, s)))))
  haven::write_dta(input, file.path(root, "input.dta"))
  haven::write_dta(data.frame(iso = countries, year = 2024L, anninc992i = 10000), file.path(root, "wid.dta"))
  means <- expand.grid(country = countries, year = 2024L, step = c("nat", "pod"), stringsAsFactors = FALSE)
  means$unit <- "esn"; means$variable <- "average"; means$group <- "tot"
  means$value <- ifelse(means$step == "nat", 200, 80)
  write.csv(means, file.path(root, "output/synthetic_microfiles/_smicrofile_long_grouped.csv"), row.names = FALSE)
  write.csv(input[input$p == 0, c("country", "year", "step")], file.path(root, "observed.csv"), row.names = FALSE)
  writeLines(c("baseline", "esn", "nat pon pod", "2025", paste(countries, collapse = " "), "2000", "2024"),
    file.path(root, "request.txt"))
  code <- readLines(file.path(repo_root_for_tests, "code/Stata/07d-export-results-to-wid.do"))
  block <- function(start, end) {
    a <- which(grepl(start, code, fixed = TRUE))[[1L]]
    b <- which(grepl(end, code, fixed = TRUE) & seq_along(code) > a)[[1L]]
    code[a:(b - 1L)]
  }
  run <- function(name, steps) {
    commands <- c("clear all", "set more off", "set type double", 'use "input.dta", clear',
      if (steps == "nat") 'keep if step == "nat"',
      block("*normalize variables in lcu", "*II. Extrapolate/Interpolate"),
      sprintf('save "%s-normalized.dta", replace', name),
      "replace bracketavg = bracketavg_norm",
      sprintf('local consistency_work "%s"', file.path(root, paste0(name, "-consistency"))),
      'tempfile consistency_input consistency_output', "local dina_finished_count = 0",
      block("// Consistency preparation of assembled", "// End consistency assembled preparation."),
      sprintf("global export_steps %s", dina_stata_quote_list(strsplit(steps, " ")[[1L]])),
      "local steps: copy global export_steps", 'local tf_ni_wid "wid.dta"', "local dina_finished_count = 0",
      'local comparison_request "request.txt"', 'local dina_income_observed "observed.csv"',
      block("*scale to national income level", "//create main folders"),
      sprintf('save "%s-scaled.dta", replace', name),
      "replace p = p / 1000", "tempfile main", "save `main'",
      block("//check if there are missing p at this step", "*Top"),
      block("*Top", "*Bottom"), block("*Bottom", "*Bottom avg"),
      block("*Bottom avg", "append using `top'"),
      "append using `top'", "append using `bottom'", "append using `bottomavg'",
      "isid country year p widcode",
      sprintf('save "%s-long.dta", replace', name),
      "drop step",
      sprintf('global dina_output_root "%s"', root),
      sprintf('local date "%s"', name),
      block("*One main export contains", "// Audit the actual saved file."),
      'use "input.dta", clear', "keep if p == 0", "expand 2",
      "bysort country step: replace year = 2023 + _n",
      'gen extrap = 1 if step == "pod" & year == 2025', "gen imputed = !missing(extrap)",
      'replace data_points = ""', 'replace extrap_points = ""', 'replace interp_points = ""',
      sprintf('local all_ctries "%s"', paste(countries, collapse = " ")),
      "global first_y 2024", "local ly = 2025",
      block("*tag data points, extrapolations and interpolations", "*harmonise country names"),
      sprintf('save "%s-coverage.dta", replace', name),
      sprintf('file open done using "%s.done", write replace', name),
      'file write done "done"', "file close done")
    writeLines(commands, file.path(root, paste0(name, ".do")))
    processx::run(stata, c("-b", "do", paste0(name, ".do")), wd = root, timeout = 90000, error_on_status = FALSE)
    log <- readLines(file.path(root, paste0(name, ".log")), warn = FALSE)
    testthat::expect_true(file.exists(file.path(root, paste0(name, ".done"))), info = paste(tail(log, 45), collapse = "\n"))
    if (!file.exists(file.path(root, paste0(name, ".done")))) return(NULL)
    list(normalized = haven::read_dta(file.path(root, paste0(name, "-normalized.dta"))),
      scaled = haven::read_dta(file.path(root, paste0(name, "-scaled.dta"))),
      long = haven::read_dta(file.path(root, paste0(name, "-long.dta"))),
      published = haven::read_dta(file.path(root, "output/latest_wid_series", paste0("dina_latam_", name, ".dta"))),
      coverage = haven::read_dta(file.path(root, paste0(name, "-coverage.dta"))))
  }
  single <- run("nat", "nat"); combined <- run("combined", "nat pon pod")
  if (is.null(single) || is.null(combined)) return(invisible(NULL))
  order_rows <- function(x) x[order(x$country, x$year, x$p, x$widcode), ]
  coverage <- combined$coverage
  testthat::expect_true(all(coverage$data_points[coverage$step == "nat"] == "[2024,2025]"))
  testthat::expect_true(all(coverage$data_points[coverage$step == "pod"] == "[2024]"))
  testthat::expect_true(all(coverage$extrap_points[coverage$step == "nat"] == ""))
  testthat::expect_true(all(coverage$extrap_points[coverage$step == "pod"] == "[2024, 2025]"))
  lhs <- order_rows(single$long); rhs <- order_rows(combined$long[combined$long$step == "nat", ])
  testthat::expect_equal(as.data.frame(lhs), as.data.frame(rhs), tolerance = 0)
  for (result in list(single, combined)) {
    expected <- result$long[, setdiff(names(result$long), "step")]
    testthat::expect_equal(as.data.frame(order_rows(result$published)),
      as.data.frame(order_rows(expected)), tolerance = 0)
  }
  testthat::expect_setequal(substr(combined$published$widcode, 2L, 6L), c("ptinc", "diinc", "cainc"))
  testthat::expect_setequal(substr(single$published$widcode, 2L, 6L), "ptinc")
  testthat::expect_equal(as.data.frame(order_rows(single$published)),
    as.data.frame(order_rows(combined$published[grepl("ptinc", combined$published$widcode), ])), tolerance = 0)
  testthat::expect_length(list.files(file.path(root, "output/latest_wid_series"), pattern = "_disposable[.]dta$"), 0L)
  testthat::expect_length(list.files(file.path(root, "output/latest_wid_series"), pattern = "_amory[.]dta$"), 0L)
  for (concept in c("nat", "pon", "pod")) {
    z <- combined$normalized[combined$normalized$country == "ARG" & combined$normalized$step == concept, ]
    z <- z[order(z$p), ]
    testthat::expect_equal(sum(z$bracketavg_norm * mass), 1, tolerance = 1e-7)
    code <- c(nat = "ptinc", pon = "diinc", pod = "cainc")[concept]
    x <- combined$long[combined$long$country == "ARG" & combined$long$step == concept, ]
    testthat::expect_true(all(x$widcode %in% paste0(c("a", "s", "t"), code, "992j")))
    testthat::expect_true(all(x$data_points == paste0("[", concept, "]")))
    overall <- x$value[x$p == "p0p100" & x$widcode == paste0("a", code, "992j")]
    expected_mean <- if (concept == "pod") 4000 else 10000
    testthat::expect_equal(overall, expected_mean, tolerance = 1e-7)
    scaled <- combined$scaled[combined$scaled$country == "ARG" & combined$scaled$step == concept, ]
    scaled <- scaled[order(scaled$p), ]
    testthat::expect_equal(sum(scaled$bracketavg * mass), expected_mean, tolerance = 1e-7)
    for (group in c("p0p50", "p50p90", "p90p100")) {
      use <- switch(group, p0p50 = p < 50000, p50p90 = p >= 50000 & p < 90000, p90p100 = p >= 90000)
      sh <- x$value[x$p == group & x$widcode == paste0("s", code, "992j")]
      testthat::expect_equal(sh, sum(scaled$s[use]), tolerance = 1e-7)
    }
  }
})

testthat::test_that("regional disposable donors permit an empty supplied-source attribution", {
  stata <- Sys.getenv("DINA_STATA_CMD", unset="/Applications/Stata/StataMP.app/Contents/MacOS/stata-mp")
  testthat::skip_if(!file.exists(stata), "Stata unavailable")
  root <- tempfile("disposable-donor-attribution-"); dir.create(root)
  on.exit(unlink(root,recursive=TRUE),add=TRUE)
  countries <- c("ARG","BRA","CHL","COL","DOM","ECU","MEX","PER","SLV","URY")
  x <- expand.grid(country=countries,year=2024:2025,step=c("nat","pod"),p=c(0,50000),stringsAsFactors=FALSE)
  x$bracketavg_norm <- 1; x$thr <- 0; x$bracketpop <- 50000
  supplied <- x$country=="ECU" & x$step=="nat"
  x$pub_source <- ifelse(supplied,"supplied-pretax","")
  x$pub_citation <- ifelse(supplied,"Supplied pretax reference","")
  x$pub_doi <- ""
  haven::write_dta(x,file.path(root,"donors.dta"))
  lines <- readLines(file.path(repo_root_for_tests,"code/Stata/07d-export-results-to-wid.do"))
  start <- which(lines=="*loop over groups")
  end <- which(grepl("^\\*merge with population data",lines))
  commands <- c("clear all","set more off",'use "donors.dta", clear',
    paste0('global all_countries ',dina_stata_quote_list(countries)),
    paste0('global export_steps ',dina_stata_quote_list(c("nat","pod"))),
    "local steps : copy global export_steps", "local dina_finished_count = 1",lines[start:(end-1L)])
  for (concept in c("nat","pod")) for (pool in c("all","CUB")) {
    commands <- c(commands,sprintf("use `tf_reg_avg_%s_%s', clear",pool,concept),
      "assert _N == 4", "assert bracketavg == 1", "assert topsh == 1 if p == 0",
      if(concept=="nat") 'assert strpos(pub_donor_note,"supplied-pretax") > 0' else 'assert pub_donor_note == ""')
  }
  commands <- c(commands,'file open done using "passed.txt", write replace','file write done "passed"',"file close done")
  writeLines(commands,file.path(root,"check.do"))
  processx::run(stata,c("-b","do","check.do"),wd=root,timeout=30000,error_on_status=FALSE)
  testthat::expect_true(file.exists(file.path(root,"passed.txt")),
    info=paste(tail(readLines(file.path(root,"check.log"),warn=FALSE),45),collapse="\n"))
})
