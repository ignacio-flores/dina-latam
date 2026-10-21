finished_dina_workflow_rows <- function(country = "AAA") {
  data.frame(
    percentile = c("p0p50", "p50p100"), step = "nat", unit = "esn",
    country = country, year = 2022L, gini = 0.25, average = 100,
    p = c(0, 0.5), thr = c(0, 100), avg = c(50, 150),
    bckt_sh = c(0.25, 0.75), topavg = c(100, 150), topsh = c(1, 0.75),
    stringsAsFactors = FALSE
  )
}

finished_dina_workflow_source <- function(country = "AAA") {
  list(
    id = paste0(tolower(country), "-finished-fixture"),
    family = "dina", country = country, method = "manual", integration = "review_only",
    inbox = paste0("input_data/_new/dina/", country, "_finished_v2.dta"),
    canonical = paste0("input_data/dina/", country, "_finished_v2.dta"),
    finished_series = list(
      version = "v2", first_year = 2022L, last_year = 2022L,
      steps = "nat", units = "esn", publication_step = "nat", publication_unit = "esn",
      citation = "Fixture Researcher. 2026. A finished distribution.",
      doi = "https://doi.org/10.1234/finished-fixture",
      currency = "unconfirmed", price_basis = "unconfirmed", population = "unconfirmed"
    )
  )
}

finished_dina_workflow_root <- function(root, countries = "AAA") {
  skip_if_not_installed("haven")
  skip_if_not_installed("yaml")
  dir.create(file.path(root, "input_data", "_new", "dina"), recursive = TRUE)
  dir.create(file.path(root, "input_data", "dina"), recursive = TRUE)
  dir.create(file.path(root, "output", "latest_wid_series"), recursive = TRUE)
  for (module in c("finished_dina_explorer.R", "finished_dina_preparer.R")) {
    copied <- file.copy(
      file.path(repo_root_for_tests, "code", "R", "source-diagnostics", module),
      file.path(root, "code", "R", "source-diagnostics", module)
    )
    stopifnot(copied)
  }
  config <- dina_read_yaml(file.path(root, "config", "dina.yml"))
  config$countries <- countries
  dina_write_yaml(config, file.path(root, "config", "dina.yml"))
  sources <- lapply(countries, finished_dina_workflow_source)
  dina_write_yaml(list(sources = sources), file.path(root, "config", "sources.yml"))
  for (source in sources) {
    haven::write_dta(finished_dina_workflow_rows(source$country), file.path(root, source$inbox))
    current <- finished_dina_workflow_rows(source$country)
    current$gini <- 0.3
    haven::write_dta(current, file.path(root, source$canonical))
  }
  writeLines("previous published scientific results", file.path(root, "output", "latest_wid_series", "sentinel.txt"))
  root
}

finished_dina_workflow_scientific_hashes <- function(root) {
  paths <- list.files(
    file.path(root, c("input_data/_new/dina", "input_data/dina", "output/latest_wid_series")),
    full.names = TRUE, recursive = TRUE
  )
  setNames(vapply(paths, dina_hash_file, character(1)), paths)
}

finished_dina_workflow_text <- function(table) {
  paste(unlist(table, use.names = FALSE), collapse = "\n")
}

test_that("finished DINA review output stays in experiments and preserves small differences", {
  source_cli_for_tests()
  root <- mini_repo()
  expect_match(dina_finished_output_root(root), "/output/experiments/finished_dina_explore$")
  expect_error(dina_finished_output_root(root, TRUE), "output-dir")
  expect_error(dina_finished_output_root(root, "output/experiments/../../input_data/new"), "output-dir")
  dir.create(file.path(root, "output", "experiments"))
  if (file.symlink(file.path(root, "input_data"), file.path(root, "output", "experiments", "linked"))) {
    expect_error(dina_finished_output_root(root, "output/experiments/linked/new"), "output-dir")
  }
  run <- file.path(root, "output", "experiments", "precision")
  dir.create(file.path(run, "tables"), recursive = TRUE)
  utils::write.csv(data.frame(country = "AAA", check_id = "share_identity", observed = 0.000362107330566, expected = 0.000365119330566, difference = 0.000003012),
    file.path(run, "tables", "review_problems.csv"), row.names = FALSE)
  output <- paste(capture.output(dina_finished_show_table(list(family = "dina", run = run), "problems", filters = list(exact = TRUE))), collapse = "\n")
  expect_match(output, "3.012e-06", fixed = TRUE)
  expect_match(output, "0.000362107330566", fixed = TRUE)
})

test_that("finished DINA exploration needs no internal calculation or publication inputs", {
  source_cli_for_tests()
  root <- mini_repo()
  finished_dina_workflow_root(root)
  before <- finished_dina_workflow_scientific_hashes(root)
  expect_false(dir.exists(file.path(root, "output", "ineqstats")))
  expect_false(dir.exists(file.path(root, "output", "synthetic_microfiles")))

  result <- run_dina_cli(c("sources", "explore", "dina"), root)
  expect_equal(result$status, 0L, info = result$output)
  expect_match(result$output, "Finished DINA series", fixed = TRUE)
  record <- dina_review_read(root, "dina")
  expect_equal(record$family, "dina")
  # Two scientifically coherent brackets cover the full population; a
  # different export grid requires review, not a missing-observation failure.
  expect_equal(record$status, "needs_review")
  expect_true(file.exists(file.path(record$run, "review.json")))
  for (table in c("review_coverage", "checks", "review_problems", "review_files")) {
    expect_true(file.exists(file.path(record$run, "tables", paste0(table, ".csv"))), info = table)
  }
  coverage <- finished_dina_workflow_text(dina_review_table(record, "coverage"))
  expect_match(coverage, "AAA", fixed = TRUE)
  expect_match(coverage, "2022", fixed = TRUE)
  expect_match(coverage, "nat", fixed = TRUE)
  expect_match(coverage, "esn", fixed = TRUE)
  checks <- dina_review_table(record, "checks")
  expect_true(nrow(checks) > 0L)
  problems <- dina_review_table(record, "problems")
  expect_true(all(c("population_age", "currency_price_basis", "bracket_semantics") %in%
    problems$field[problems$check_id == "metadata"]))
  continuation <- problems[apply(problems, 1L, function(row) any(grepl("continuation|internal.*result", row, ignore.case = TRUE))), , drop = FALSE]
  if (nrow(continuation)) {
    expect_false(any(grepl("block|fail", finished_dina_workflow_text(continuation), ignore.case = TRUE)))
  }
  files <- finished_dina_workflow_text(dina_review_table(record, "files"))
  expect_match(files, "AAA_finished_v2.dta", fixed = TRUE)
  expect_match(files, "v2", fixed = TRUE)
  expect_match(files, "Fixture Researcher. 2026. A finished distribution.", fixed = TRUE)
  expect_match(files, "https://doi.org/10.1234/finished-fixture", fixed = TRUE)
  expect_equal(finished_dina_workflow_scientific_hashes(root), before)
  expect_false(dir.exists(file.path(root, "output", "run_logs")))
})

test_that("saved finished DINA tables are accessible and country filters do not narrow validation", {
  source_cli_for_tests()
  root <- mini_repo()
  finished_dina_workflow_root(root, c("AAA", "BBB"))
  bad_source <- finished_dina_workflow_source("BBB")
  duplicated <- finished_dina_workflow_rows("BBB")
  haven::write_dta(rbind(duplicated, duplicated[1L, ]), file.path(root, bad_source$inbox))
  before <- finished_dina_workflow_scientific_hashes(root)

  result <- run_dina_cli(c("sources", "explore", "dina", "--country", "AAA"), root)
  expect_equal(result$status, 0L, info = result$output)
  record <- dina_review_read(root, "dina")
  expect_equal(record$status, "blocked")
  expect_match(finished_dina_workflow_text(dina_review_table(record, "coverage")), "BBB", fixed = TRUE)
  problems <- finished_dina_workflow_text(dina_review_table(record, "problems"))
  expect_match(problems, "BBB", fixed = TRUE)
  expect_match(problems, "duplicate", ignore.case = TRUE)

  # --run must read the named review without rereading or validating the source.
  saved_run <- record$run
  unlink(file.path(root, bad_source$inbox))
  for (table in c("coverage", "checks", "problems", "files")) {
    shown <- run_dina_cli(c("sources", "table", "dina", table, "--run", saved_run), root)
    expect_equal(shown$status, 0L, info = paste(table, shown$output))
  }
  filtered <- run_dina_cli(c("sources", "table", "dina", "coverage", "--country", "AAA", "--run", saved_run), root)
  expect_equal(filtered$status, 0L, info = filtered$output)
  expect_match(filtered$output, "AAA", fixed = TRUE)
  expect_false(grepl("BBB", filtered$output, fixed = TRUE))
  expect_equal(dina_review_read(root, "dina")$run, saved_run)
  retained <- finished_dina_workflow_scientific_hashes(root)
  expect_equal(retained, before[names(retained)])
})

test_that("finished DINA missing and malformed inputs produce actionable saved reviews", {
  source_cli_for_tests()
  root <- mini_repo()
  finished_dina_workflow_root(root)
  candidate <- file.path(root, finished_dina_workflow_source()$inbox)
  unlink(candidate)
  missing <- run_dina_cli(c("sources", "explore", "dina"), root)
  expect_equal(missing$status, 0L, info = missing$output)
  record <- dina_review_read(root, "dina")
  expect_equal(record$status, "blocked")
  problems <- finished_dina_workflow_text(dina_review_table(record, "problems"))
  expect_match(problems, "missing|not found|absent", ignore.case = TRUE)
  expect_match(problems, "AAA_finished_v2.dta", fixed = TRUE)

  malformed <- finished_dina_workflow_rows()
  malformed$avg <- NULL
  haven::write_dta(malformed, candidate)
  before <- finished_dina_workflow_scientific_hashes(root)
  checked <- run_dina_cli(c("sources", "explore", "dina"), root)
  expect_equal(checked$status, 0L, info = checked$output)
  record <- dina_review_read(root, "dina")
  expect_equal(record$status, "blocked")
  expect_match(finished_dina_workflow_text(dina_review_table(record, "problems")), "avg", fixed = TRUE)
  expect_equal(finished_dina_workflow_scientific_hashes(root), before)
})

test_that("re-exploration does not restore or replace accepted finished series", {
  source_cli_for_tests()
  root <- mini_repo()
  finished_dina_workflow_root(root)
  expect_equal(run_dina_cli(c("sources", "explore", "dina"), root)$status, 0L)
  before <- finished_dina_workflow_scientific_hashes(root)
  record <- dina_review_read(root, "dina")
  record$status <- "included"
  record$included_at <- "2026-10-01"
  record$confirmation <- file.path(root, "an-earlier-inclusion.json")
  dina_review_save(record, root)

  result <- run_dina_cli(c("sources", "explore", "dina"), root)
  expect_equal(result$status, 0L, info = result$output)
  expect_equal(dina_review_read(root, "dina")$status, "needs_review")
  expect_equal(finished_dina_workflow_scientific_hashes(root), before)
})

test_that("invalid finished-series registry declarations block the saved review", {
  source_cli_for_tests()
  root <- mini_repo()
  finished_dina_workflow_root(root)
  before <- finished_dina_workflow_scientific_hashes(root)
  for (invalid in c("reversed_years", "publication_unit")) {
    registered <- finished_dina_workflow_source()
    if (invalid == "reversed_years") {
      registered$finished_series$first_year <- 2022L
      registered$finished_series$last_year <- 1990L
    } else {
      registered$finished_series$publication_unit <- "zzz"
    }
    dina_write_yaml(list(sources = list(registered)), file.path(root, "config", "sources.yml"))
    result <- run_dina_cli(c("sources", "explore", "dina"), root)
    expect_equal(result$status, 0L, info = paste(invalid, result$output))
    record <- dina_review_read(root, "dina")
    expect_equal(record$status, "blocked", info = invalid)
    problems <- dina_review_table(record, "problems")
    expect_true(any(problems$check_id == "source_metadata" & problems$status == "fail"), info = invalid)
    expect_equal(finished_dina_workflow_scientific_hashes(root), before)
  }
})

test_that("finished DINA menu offers inclusion and invalid routes preserve data", {
  source_cli_for_tests()
  root <- mini_repo()
  finished_dina_workflow_root(root)
  expect_equal(unname(dina_review_families()[["dina"]]), "Finished DINA series")
  expect_true("dina" %in% dina_review_source_family_order())
  listed <- run_dina_cli(c("sources", "list", "dina"), root)
  expect_equal(listed$status, 0L, info = listed$output)
  expect_match(listed$output, "AAA|aaa-finished-fixture")
  guide <- run_dina_cli(c("sources", "list", "guide", "dina"), root)
  expect_equal(guide$status, 0L, info = guide$output)
  expect_match(guide$output, "aaa-finished-fixture", fixed = TRUE)
  expect_match(guide$output, "dina sources explore dina", fixed = TRUE)
  expect_match(guide$output, "dina sources include dina --dry-run", fixed = TRUE)

  input <- textConnection("q\n")
  on.exit(close(input), add = TRUE)
  menu <- paste(capture.output(dina_review_family_menu(root, "dina", input, TRUE)), collapse = "\n")
  expect_match(menu, "Finished DINA series", fixed = TRUE)
  expect_match(menu, "Explore", fixed = TRUE)
  expect_match(menu, "\\bInclude\\b")

  expect_equal(run_dina_cli(c("sources", "explore", "dina"), root)$status, 0L)
  record <- dina_review_read(root, "dina")
  pointer <- dina_hash_file(dina_review_pointer(root, "dina"))
  before <- finished_dina_workflow_scientific_hashes(root)
  variants <- list(
    "--apply", c("--confirm", "--dry-run"),
    c("--restore", "nonexistent-confirmation.json"),
    c("--include-run", record$run, "--confirm"),
    c("--exploration-run", record$run)
  )
  for (flags in variants) {
    result <- run_dina_cli(c("sources", "include", "dina", flags), root)
    expect_true(result$status != 0L, info = paste(flags, result$output))
    expect_match(result$output, "not|cannot|choose|retired|candidate|unsupported|missing|must be inside", ignore.case = TRUE)
    expect_equal(finished_dina_workflow_scientific_hashes(root), before)
    expect_equal(dina_hash_file(dina_review_pointer(root, "dina")), pointer)
  }
})

test_that("public finished DINA commands prepare review accept and restore the same candidate", {
  skip_if_not_installed("gpinter")
  source_cli_for_tests()
  root <- mini_repo()
  finished_dina_workflow_root(root)
  p <- c(0, 0.5, 0.9, 0.99)
  mass <- c(0.49, 0.41, 0.09, 0.01)
  means <- c(1, 3, 8, 20)
  average <- sum(mass * means)
  shares <- mass * means / average
  data <- data.frame(
    percentile = c("p0p50", "p50p90", "p90p99", "p99p100"),
    country = "AAA", year = 2022L, step = "nat", unit = "esn",
    p = p, gini = 0.45, average = average, avg = means, thr = c(0, 2, 9, 15),
    bckt_sh = shares, topsh = rev(cumsum(rev(shares))),
    topavg = rev(cumsum(rev(mass * means))) / rev(cumsum(rev(mass)))
  )
  path <- file.path(root, finished_dina_workflow_source()$inbox)
  haven::write_dta(data, path)
  before <- finished_dina_workflow_scientific_hashes(root)
  explored <- run_dina_cli(c("sources", "explore", "dina"), root)
  expect_equal(explored$status, 0L, info = explored$output)
  dry <- run_dina_cli(c("sources", "include", "dina", "--dry-run"), root)
  expect_equal(dry$status, 0L, info = dry$output)
  expect_equal(finished_dina_workflow_scientific_hashes(root), before)
  record <- dina_review_read(root, "dina")
  candidate <- dina_finished_candidate_read(record)
  expect_equal(candidate$status, "ready")

  changes <- run_dina_cli(c("sources", "table", "dina", "changes",
    "--rule", "threshold_repair", "--limit", "2"), root)
  expect_equal(changes$status, 0L, info = changes$output)
  expect_match(changes$output, "threshold_repair", fixed = TRUE)
  expect_match(changes$output, "Before\\s+After")
  impact <- run_dina_cli(c("sources", "table", "dina", "impact", "--exact"), root)
  expect_equal(impact$status, 0L, info = impact$output)
  expect_match(impact$output, "same_population_boundary", fixed = TRUE)
  overview <- run_dina_cli(c("sources", "table", "dina"), root)
  expect_equal(overview$status, 0L, info = overview$output)
  expect_match(overview$output, "Ready for acceptance", fixed = TRUE)
  expect_false(grepl("Consistency review", overview$output, fixed = TRUE))

  accepted <- run_dina_cli(c("sources", "include", "dina", "--confirm",
    "--include-run", candidate$run), root)
  expect_equal(accepted$status, 0L, info = accepted$output)
  record <- dina_review_read(root, "dina")
  expect_equal(record$status, "included")
  state <- dina_review_family_status(root, "dina")
  expect_equal(state$code, "included", info = state$reason)
  prepared <- haven::read_dta(file.path(root, "input_data/dina/AAA_finished_v2_prepared.dta"))
  expect_equal(nrow(prepared), 127L)
  expect_true(all(prepared$gini == data$gini[[1L]]))
  expect_equal(dina_hash_file(path),
    dina_hash_file(file.path(root, finished_dina_workflow_source()$canonical)))
  restored <- run_dina_cli(c("sources", "include", "dina", "--restore", record$confirmation), root)
  expect_equal(restored$status, 0L, info = restored$output)
  expect_equal(finished_dina_workflow_scientific_hashes(root), before)
})
