numbered_pipeline <- function(root) {
  dina_write_yaml(list(tasks = list(
    list(id = "01a-clean-macro-data", stage = "macro", type = "stata", script = "code/Stata/01a.do", deps = list(), inputs = c("input_data/a.txt"), outputs = c("output/a.txt")),
    list(id = "01b-add-country-sna", stage = "macro", type = "stata", script = "code/Stata/01b.do", deps = c("01a-clean-macro-data"), inputs = c("output/a.txt"), outputs = c("output/b.txt")),
    list(id = "02a-get-survey-populations", stage = "preparation", type = "stata", script = "code/Stata/02a.do", deps = c("01b-add-country-sna"), inputs = c("output/b.txt"), outputs = c("output/c.txt")),
    list(id = "07d-export-results-to-wid", stage = "export", type = "stata", script = "code/Stata/07d.do", deps = c("02a-get-survey-populations"), inputs = c("output/c.txt"), outputs = c("output/d.txt"))
  )), file.path(root, "config", "pipeline.yml"))
}

run_dina_cli <- function(args, root = repo_root_for_tests, env = character()) {
  output <- system2(
    file.path(repo_root_for_tests, "bin", "dina"),
    args,
    stdout = TRUE,
    stderr = TRUE,
    env = c(sprintf("DINA_REPO_ROOT=%s", root), env)
  )
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  list(status = status, output = paste(output, collapse = "\n"))
}

fake_executable <- function(dir, name) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(dir, name)
  writeLines(c("#!/bin/sh", "exit 0"), path)
  Sys.chmod(path, "0755")
  path
}

test_that("task selector aliases resolve numbered tasks and blocks", {
  root <- mini_repo()
  numbered_pipeline(root)
  ids <- vapply(dina_pipeline(root)$tasks, function(x) x$id, character(1))

  expect_equal(dina_resolve_task_selector("01a", ids), "01a-clean-macro-data")
  expect_equal(dina_resolve_task_selector("07d", ids), "07d-export-results-to-wid")
  expect_equal(dina_resolve_task_selector("01", ids, mode = "from"), "01a-clean-macro-data")
  expect_equal(dina_resolve_task_selector("01", ids, mode = "to"), "01b-add-country-sna")
  expect_equal(dina_task_short_id("07d-export-results-to-wid"), "07d")

  selected_block <- vapply(dina_select_tasks(root, task = "01"), function(x) x$id, character(1))
  expect_equal(selected_block, c("01a-clean-macro-data", "01b-add-country-sna"))

  selected_many <- vapply(dina_select_tasks(root, task = "01a,02a"), function(x) x$id, character(1))
  expect_equal(selected_many, c("01a-clean-macro-data", "02a-get-survey-populations"))

  selected_range <- vapply(dina_select_tasks(root, from = "01", to = "02"), function(x) x$id, character(1))
  expect_equal(selected_range, c("01a-clean-macro-data", "01b-add-country-sna", "02a-get-survey-populations"))

  expect_error(dina_resolve_task_selector("01", ids, mode = "single"), "matches multiple tasks")
  expect_error(dina_resolve_task_selector("09a", ids), "Unknown task selector")
})

test_that("project pipeline keeps admin task aliases sequential", {
  tasks <- dina_pipeline(repo_root_for_tests)$tasks
  ids <- vapply(tasks, function(task) task$id, character(1))
  deps <- unlist(lapply(tasks, function(task) task$deps %||% character()), use.names = FALSE)
  scripts <- vapply(tasks, function(task) task$script %||% "", character(1))
  names(tasks) <- ids

  expect_true(all(c(
    "02a-get-survey-populations",
    "02b-prepare-admin-microdata",
    "02c-prepare-static-admin-data",
    "02d-prepare-updated-admin-data",
    "02e-format-for-bfm"
  ) %in% ids))
  expect_false(isTRUE(dina_task_active(tasks[["02b-prepare-admin-microdata"]])))
  expect_true(all(vapply(tasks[c(
    "02a-get-survey-populations",
    "02c-prepare-static-admin-data",
    "02d-prepare-updated-admin-data",
    "02e-format-for-bfm"
  )], dina_task_active, logical(1))))
  expect_true("02e-format-for-bfm" %in% deps)
  expect_false(any(grepl("02d-prepare-frequenly|02b-prepare-static-admin-data|02c-prepare-updated-admin-data|02d-format-for-bfm", scripts)))

  selected_block <- vapply(dina_select_tasks(repo_root_for_tests, task = "02"), function(task) task$id, character(1))
  expect_equal(selected_block, c(
    "02a-get-survey-populations",
    "02c-prepare-static-admin-data",
    "02d-prepare-updated-admin-data",
    "02e-format-for-bfm"
  ))
  selected_inactive <- vapply(dina_select_tasks(repo_root_for_tests, task = "02b"), function(task) task$id, character(1))
  expect_equal(selected_inactive, "02b-prepare-admin-microdata")
  expect_equal(dina_task_status(tasks[["02b-prepare-admin-microdata"]], repo_root_for_tests)$status, "inactive")
})

test_that("Pushover local config can be initialized and reported by doctor", {
  old_token <- Sys.getenv("PUSHOVER_APP_TOKEN", unset = NA_character_)
  old_user <- Sys.getenv("PUSHOVER_USER_KEY", unset = NA_character_)
  on.exit({
    if (is.na(old_token)) Sys.unsetenv("PUSHOVER_APP_TOKEN") else Sys.setenv(PUSHOVER_APP_TOKEN = old_token)
    if (is.na(old_user)) Sys.unsetenv("PUSHOVER_USER_KEY") else Sys.setenv(PUSHOVER_USER_KEY = old_user)
  }, add = TRUE)
  Sys.unsetenv(c("PUSHOVER_APP_TOKEN", "PUSHOVER_USER_KEY"))

  root <- mini_repo()
  status <- dina_pushover_status(root)
  expect_false(status$configured)

  initialized <- dina_notify_init(root)
  expect_true(initialized$created)
  expect_true(file.exists(dina_pushover_local_path(root)))

  status <- dina_pushover_status(root)
  expect_true(status$configured)
  expect_true(status$local_configured)
  expect_equal(status$source, "local_file")

  doctor <- dina_doctor(root)
  expect_equal(doctor$pushover$source, "local_file")
})

test_that("doctor distinguishes configured and discovered Stata commands", {
  old_stata <- Sys.getenv("DINA_STATA_CMD", unset = NA_character_)
  old_path <- Sys.getenv("PATH", unset = "")
  on.exit({
    if (is.na(old_stata)) Sys.unsetenv("DINA_STATA_CMD") else Sys.setenv(DINA_STATA_CMD = old_stata)
    Sys.setenv(PATH = old_path)
  }, add = TRUE)

  root <- mini_repo()
  Sys.unsetenv("DINA_STATA_CMD")
  empty <- dina_doctor(root, stata_path_names = character(), stata_app_dirs = character())$stata
  expect_false(empty$configured)
  expect_false(empty$discovered)

  app_dir <- tempfile("stata-app-")
  app_exe <- fake_executable(file.path(app_dir, "StataMP.app", "Contents", "MacOS"), "stata-mp")
  discovered <- dina_doctor(root, stata_path_names = character(), stata_app_dirs = app_dir)$stata
  expect_false(discovered$configured)
  expect_true(discovered$discovered)
  expect_equal(discovered$discovered_command, app_exe)
  expect_match(discovered$suggestion, "export DINA_STATA_CMD=")

  path_dir <- tempfile("stata-path-")
  path_exe <- fake_executable(path_dir, "stata-mp")
  Sys.setenv(PATH = paste(path_dir, old_path, sep = .Platform$path.sep))
  Sys.setenv(DINA_STATA_CMD = "stata-mp")
  configured <- dina_doctor(root, stata_path_names = "stata-mp", stata_app_dirs = character())$stata
  expect_true(configured$configured)
  expect_true(configured$available)
  expect_equal(unname(Sys.which("stata-mp")), path_exe)

  Sys.setenv(DINA_STATA_CMD = "missing-stata")
  missing_configured <- dina_doctor(root, stata_path_names = "stata-mp", stata_app_dirs = character())$stata
  expect_true(missing_configured$configured)
  expect_false(missing_configured$available)
  expect_true(missing_configured$discovered)

  output <- run_dina_cli(
    c("doctor"),
    root = root,
    env = c(sprintf("DINA_STATA_APP_DIRS=%s", app_dir), "DINA_STATA_CMD=")
  )
  expect_equal(output$status, 0L)
  expect_match(output$output, "Installed but not configured for DINA")
  expect_match(output$output, "export DINA_STATA_CMD=\"")
})

test_that("help output documents run variants and short selectors", {
  result <- run_dina_cli(c("help", "run"))
  expect_equal(result$status, 0L)
  text <- result$output
  expect_match(text, "dina run 01a --dry-run")
  expect_match(text, "01[[:space:]]+Whole numbered block")
  expect_match(text, "--task 01a,01b[[:space:]]+Same selection")
  expect_match(text, "--from 03 --to 05[[:space:]]+Range")
  expect_match(text, "--notify")
  expect_match(text, "Dry-run is the default")
  expect_match(text, "--execute")

  result <- run_dina_cli(c("run", "--help"))
  expect_equal(result$status, 0L)
  expect_match(result$output, "Task selectors")
})

test_that("help and default dispatch accept optional global separator", {
  source_root <- mini_repo()
  commands <- list(
    list(args = c("help")),
    list(args = c("--help")),
    list(args = c("--", "help")),
    list(args = c("help", "workflow")),
    list(args = c("sources"), root = source_root),
    list(args = c("config")),
    list(args = c("data")),
    list(args = c("tasks")),
    list(args = c("notify", "--help")),
    list(args = c("--", "config", "--help")),
    list(args = c("--", "run", "01a", "--dry-run"))
  )

  for (command in commands) {
    result <- run_dina_cli(command$args, root = command$root %||% repo_root_for_tests)
    expect_equal(result$status, 0L)
  }
})

test_that("dashboard offers executable numbered actions without prompting in non-interactive runs", {
  root <- mini_repo()
  result <- run_dina_cli(character(), root = root)
  expect_equal(result$status, 0L)
  year <- format(Sys.Date(), "%Y")
  expect_match(result$output, "No active update session.")
  expect_match(result$output, sprintf("Recommended next action: Start an update with `dina update start %s`", year))
  expect_match(result$output, "Useful actions:")
  expect_match(result$output, sprintf("1\\. dina update start %s", year))
  expect_equal(length(gregexpr(sprintf("dina update start %s", year), result$output, fixed = TRUE)[[1]][gregexpr(sprintf("dina update start %s", year), result$output, fixed = TRUE)[[1]] > 0]), 2L)
  expect_match(result$output, "2\\. dina doctor")
  expect_match(result$output, "3\\. dina update roadmap")
  expect_match(result$output, "4\\. dina update gate")
  expect_match(result$output, "5\\. dina sources inbox guide")
  expect_match(result$output, "6\\. dina tasks list")
  expect_match(result$output, "7\\. dina run --dry-run")
  expect_false(grepl("Choose an action number", result$output, fixed = TRUE))
})

test_that("main and config help explain detailed topics and subtleties", {
  main <- run_dina_cli(c("help"))
  expect_equal(main$status, 0L)
  expect_match(main$output, "DINA-LatAm CLI")
  expect_match(main$output, "Use `dina help COMMAND`")
  expect_match(main$output, "\\[read-only\\]")
  expect_match(main$output, "\\[writes session\\]")
  expect_match(main$output, "\\[writes files\\]")
  expect_match(main$output, "\\[writes config\\]")
  expect_match(main$output, "writes archive")
  expect_match(main$output, "Command map")
  expect_match(main$output, "Annual update:")
  expect_match(main$output, "`help workflow`[[:space:]]+\\[read-only\\]")
  expect_match(main$output, "`update start \\[YEAR\\]`[[:space:]]+\\[writes session\\]")
  expect_match(main$output, "Source data:")
  expect_match(main$output, "`update roadmap\\|gate \\[GATE\\]`[[:space:]]+\\[read-only\\]")
  expect_match(main$output, "`update mark\\|unmark GATE/CHECK`[[:space:]]+\\[writes session\\]")
  expect_match(main$output, "`sources refresh \\[--dry-run\\]`[[:space:]]+\\[read-only/writes session\\]")
  expect_match(main$output, "`sources integrate --incoming`[[:space:]]+\\[writes files\\]")
  expect_match(main$output, "Pipeline:")
  expect_match(main$output, "`run \\.\\.\\. --execute`[[:space:]]+\\[writes files\\]")
  expect_match(main$output, "Setup and config:")
  expect_match(main$output, "Maintenance:")
  expect_match(main$output, "Pipeline selectors")
  expect_match(main$output, "`01a`[[:space:]]+one task")
  expect_match(main$output, "`01`[[:space:]]+whole numbered block for `dina run`")
  expect_match(main$output, "`--from 03 --to 05`[[:space:]]+range")
  expect_match(main$output, "`tasks why` needs a unique selector")
  expect_match(main$output, "Critical defaults")
  expect_match(main$output, "Update progress is recorded with roadmap gate checks")
  expect_match(main$output, "`dina run` is dry-run unless `--execute`")
  expect_match(main$output, "does not edit `_config.do`")
  expect_match(main$output, "dina help workflow")
  expect_match(main$output, "dina update roadmap")
  expect_match(main$output, "dina update gate tax-admin")
  expect_false(grepl("sources complete", main$output, fixed = TRUE))
  expect_false(grepl("update checklist", main$output, fixed = TRUE))
  expect_false(grepl("* has detailed help", main$output, fixed = TRUE))
  expect_false(grepl("**Command map**", main$output, fixed = TRUE))
  expect_false(grepl("**Pipeline selectors**", main$output, fixed = TRUE))
  expect_false(grepl("**Critical defaults**", main$output, fixed = TRUE))
  expect_false(grepl("workflow*", main$output, fixed = TRUE))
  expect_false(grepl("update*", main$output, fixed = TRUE))
  expect_false(grepl("sources*", main$output, fixed = TRUE))
  expect_false(grepl("The CLI is organized around", main$output, fixed = TRUE))
  expect_false(grepl("Start here:", main$output, fixed = TRUE))
  expect_false(grepl("Annual update commands:", main$output, fixed = TRUE))

  workflow <- run_dina_cli(c("help", "workflow"))
  expect_equal(workflow$status, 0L)
  expect_match(workflow$output, "annual update guide")
  expect_match(workflow$output, "1\\. Start with the roadmap")
  expect_match(workflow$output, "dina update start \\[YEAR\\]")
  expect_match(workflow$output, "dina update roadmap")
  expect_match(workflow$output, "dina update gate tax-admin")
  expect_match(workflow$output, "dina update mark GATE/CHECK")
  expect_match(workflow$output, "dina sources integrate --incoming")
  expect_match(workflow$output, "Task selectors:")
  expect_match(workflow$output, "`tasks why` needs one unique task selector")
  expect_match(workflow$output, "dina update finalize")
  expect_false(grepl("dina update checklist", workflow$output, fixed = TRUE))
  expect_false(grepl("dina sources complete", workflow$output, fixed = TRUE))

  run <- run_dina_cli(c("help", "run"))
  expect_equal(run$status, 0L)
  expect_match(run$output, "--task 01a,01b[[:space:]]+Same selection")
  expect_match(run$output, "--from 03 --to 05[[:space:]]+Range")

  tasks <- run_dina_cli(c("help", "tasks"))
  expect_equal(tasks$status, 0L)
  expect_match(tasks$output, "stage, language, and freshness")
  expect_match(tasks$output, "`tasks why` needs one unique task")
  expect_match(tasks$output, "Block selectors such as `07`")

  config <- run_dina_cli(c("help", "config"))
  expect_equal(config$status, 0L)
  expect_match(config$output, "show[[:space:]]+Prints the committed default YAML")
  expect_match(config$output, "set KEY VALUE[[:space:]]+Edits `config/dina.yml`")
  expect_match(config$output, "edit[[:space:]]+Opens `config/dina.yml`")
  expect_match(config$output, "render \\[PATH\\][[:space:]]+Writes a Stata `config.do`")
  expect_match(config$output, "does not edit[[:space:]]+`_config.do`")
})

test_that("sources list and show expose the source registry", {
  listed <- run_dina_cli(c("sources", "list"))
  expect_equal(listed$status, 0L)
  expect_match(listed$output, "Source Registry")
  expect_match(listed$output, "country-sna-index[[:space:]]+country_sna[[:space:]]+12 countries[[:space:]]+manual[[:space:]]+1[[:space:]]+yes")
  expect_match(listed$output, "wid-prices-xrates[[:space:]]+prices[[:space:]]+11 countries[[:space:]]+wid")
  expect_match(listed$output, "wb-xrates[[:space:]]+prices[[:space:]]+11 countries[[:space:]]+manual")
  expect_false(grepl("MULTI", listed$output, fixed = TRUE))
  expect_match(listed$output, "downloader")
  expect_match(listed$output, "transformer")

  project_scan <- dina_scan_sources(repo_root_for_tests)
  scan_countries <- vapply(project_scan, function(source) source$country, character(1))
  expect_equal(project_scan[["country-sna-index"]]$country, "12 countries")
  expect_false(any(scan_countries == "MULTI"))

  scan_root <- mini_repo()
  dina_write_yaml(list(sources = list(
    list(
      id = "broad-source",
      family = "fixture",
      country = "MULTI",
      method = "manual",
      canonical = c("input_data/broad_*.csv"),
      notes = "Fixture broad-country source."
    )
  )), file.path(scan_root, "config", "sources.yml"))
  scanned <- run_dina_cli(c("sources", "scan"), root = scan_root)
  expect_equal(scanned$status, 0L)
  expect_match(scanned$output, "broad-source \\[fixture/2 countries\\]")
  expect_false(grepl("MULTI", scanned$output, fixed = TRUE))

  methods <- run_dina_cli(c("sources", "methods"))
  expect_equal(methods$status, 0L)
  expect_match(methods$output, "url[[:space:]]+yes[[:space:]]+Direct URL")
  expect_match(methods$output, "zip[[:space:]]+yes[[:space:]]+Direct archive")
  expect_match(methods$output, "script[[:space:]]+no[[:space:]]+Custom acquisition script")
  expect_match(methods$output, "manual[[:space:]]+no[[:space:]]+Human-curated")
  expect_match(methods$output, "wid[[:space:]]+no[[:space:]]+Currently acquired")

  urls <- run_dina_cli(c("sources", "list", "--urls"))
  expect_equal(urls$status, 0L)
  expect_match(urls$output, "ARG: https://sitioanterior.indec.gob.ar")
  expect_match(urls$output, "https://wid.world/")

  country <- run_dina_cli(c("sources", "list", "--country", "CHL"))
  expect_equal(country$status, 0L)
  expect_match(country$output, "Filter: country CHL, including broad-country sources")
  expect_match(country$output, "country-sna-chl")
  expect_false(grepl("bra-admin-tax", country$output, fixed = TRUE))

  country_friendly <- run_dina_cli(c("sources", "list", "country", "CHL"))
  expect_equal(country_friendly$status, 0L)
  expect_match(country_friendly$output, "Filter: country CHL, including broad-country sources")
  expect_match(country_friendly$output, "country-sna-chl")
  expect_false(grepl("bra-admin-tax", country_friendly$output, fixed = TRUE))

  country_flag_friendly <- run_dina_cli(c("sources", "list", "country", "--CHL"))
  expect_equal(country_flag_friendly$status, 0L)
  expect_match(country_flag_friendly$output, "Filter: country CHL, including broad-country sources")
  expect_match(country_flag_friendly$output, "country-sna-chl")
  expect_false(grepl("bra-admin-tax", country_flag_friendly$output, fixed = TRUE))

  family <- run_dina_cli(c("sources", "list", "family", "admin-data"))
  expect_equal(family$status, 0L)
  expect_match(family$output, "Filter: family admin-data \\(admin_tax, admin_tax_aux\\)")
  expect_match(family$output, "chl-pit-total")
  expect_match(family$output, "bra-minwage")
  expect_false(grepl("country-sna-index", family$output, fixed = TRUE))

  family_flag_friendly <- run_dina_cli(c("sources", "list", "family", "--admin-data"))
  expect_equal(family_flag_friendly$status, 0L)
  expect_match(family_flag_friendly$output, "Filter: family admin-data \\(admin_tax, admin_tax_aux\\)")

  family_canonical <- run_dina_cli(c("sources", "list", "--family", "admin-data"))
  expect_equal(family_canonical$status, 0L)
  expect_match(family_canonical$output, "Filter: family admin-data \\(admin_tax, admin_tax_aux\\)")

  method <- run_dina_cli(c("sources", "list", "method", "manual"))
  expect_equal(method$status, 0L)
  expect_match(method$output, "Filter: method manual")
  expect_match(method$output, "country-sna-index")
  expect_false(grepl("chl-pit-total", method$output, fixed = TRUE))

  method_flag_friendly <- run_dina_cli(c("sources", "list", "method", "--manual"))
  expect_equal(method_flag_friendly$status, 0L)
  expect_match(method_flag_friendly$output, "Filter: method manual")

  method_canonical <- run_dina_cli(c("sources", "list", "--method", "manual"))
  expect_equal(method_canonical$status, 0L)
  expect_match(method_canonical$output, "Filter: method manual")

  malformed_positional <- run_dina_cli(c("sources", "list", "CHL"))
  expect_equal(malformed_positional$status, 1L)
  expect_match(malformed_positional$output, "Unknown sources list filter: CHL")

  malformed_flag <- run_dina_cli(c("sources", "list", "--CHL"))
  expect_equal(malformed_flag$status, 1L)
  expect_match(malformed_flag$output, "Unknown sources list option: --CHL")

  malformed_family <- run_dina_cli(c("sources", "list", "family", "not-a-family"))
  expect_equal(malformed_family$status, 1L)
  expect_match(malformed_family$output, "Unknown source family: not-a-family")

  shown <- run_dina_cli(c("sources", "show", "country-sna-index"))
  expect_equal(shown$status, 0L)
  expect_match(shown$output, "country: 12 countries")
  expect_match(shown$output, "country coverage:")
  expect_match(shown$output, "input_data/sna_country_data/_sna-web-site-index\\.ods")
  expect_match(shown$output, "method: manual - Human-curated/manual input or URL index")
  expect_match(shown$output, "BRA single-year downloads")
  expect_match(shown$output, "https://www.inegi.org.mx/datos/\\?t=0190")

  missing <- run_dina_cli(c("sources", "show", "does-not-exist"))
  expect_equal(missing$status, 1L)
  expect_match(missing$output, "Unknown source id: does-not-exist")

  help <- run_dina_cli(c("help", "sources"))
  expect_equal(help$status, 0L)
  expect_match(help$output, "dina sources list \\[--family FAMILY\\] \\[--country ISO\\] \\[--method METHOD\\] \\[--urls\\]")
  expect_match(help$output, "dina sources list country ISO \\[--urls\\]")
  expect_match(help$output, "dina sources list family FAMILY \\[--urls\\]")
  expect_match(help$output, "dina sources list method METHOD \\[--urls\\]")
  expect_match(help$output, "dina sources show ID \\[--urls\\]")
  expect_match(help$output, "dina sources methods")
  expect_match(help$output, "dina sources inbox guide \\[--family FAMILY\\] \\[--urls\\]")
  expect_match(help$output, "dina sources inbox init \\[--dry-run\\]")
  expect_match(help$output, "dina sources status \\[--metadata-only\\] \\[--hash-all\\] \\[--deep\\]")
  expect_match(help$output, "dina sources integrate --incoming --source ID \\[--yes\\]")
  expect_match(help$output, "central `input_data/_new` buckets")
  expect_match(help$output, "Update progress itself is[[:space:]]+recorded with `dina update mark GATE/CHECK`")
  expect_match(help$output, "url[[:space:]]+Direct URL fetchable")
  expect_match(help$output, "manual[[:space:]]+Human-curated input or URL index")
  expect_false(grepl("sources complete", help$output, fixed = TRUE))
})

test_that("update roadmap and gate records provide the update progress model", {
  root <- mini_repo()
  path <- file.path(root, "input_data", "source_2024.xlsx")
  touch(path, "2024-01-01")

  started <- run_dina_cli(c("update", "start", "2026"), root = root)
  expect_equal(started$status, 0L)
  expect_match(started$output, "Preparing update session")
  expect_match(started$output, "Creating session scaffold directories")
  expect_match(started$output, "Rendering session config")
  expect_match(started$output, "Scanning source registry and hashing source baseline")
  expect_match(started$output, "Source baseline summary")
  expect_match(started$output, "Source baseline hash mode: all")
  expect_match(started$output, "Recommended next action: dina update roadmap")

  no_hash_root <- mini_repo()
  no_hash <- run_dina_cli(c("update", "start", "2026", "--no-source-hash"), root = no_hash_root)
  expect_equal(no_hash$status, 0L)
  expect_match(no_hash$output, "Scanning source registry without source hashes")
  expect_match(no_hash$output, "hash mode none")
  expect_match(no_hash$output, "Source baseline hash mode: none")

  status <- run_dina_cli(c("sources", "status"), root = root)
  expect_equal(status$status, 0L)
  expect_match(status$output, "Source Status")
  expect_match(status$output, "hash: changed")
  expect_match(status$output, "File status counts")
  expect_match(status$output, "unchanged")
  expect_match(status$output, "Source status is diagnostic")

  Sys.setFileTime(path, as.POSIXct("2024-01-02", tz = "UTC"))
  timestamp_only <- run_dina_cli(c("sources", "status"), root = root)
  expect_equal(timestamp_only$status, 0L)
  expect_match(timestamp_only$output, "timestamp_only")

  roadmap <- run_dina_cli(c("update", "roadmap"), root = root)
  expect_equal(roadmap$status, 0L)
  expect_match(roadmap$output, "Update Roadmap")
  expect_match(roadmap$output, "parameters[[:space:]]+pending[[:space:]]+year-scope")

  gate <- run_dina_cli(c("update", "gate", "parameters"), root = root)
  expect_equal(gate$status, 0L)
  expect_match(gate$output, "Gate: parameters")
  expect_match(gate$output, "\\[pending\\] year-scope")

  marked <- run_dina_cli(c("update", "mark", "parameters/year-scope", "--status", "done"), root = root)
  expect_equal(marked$status, 0L)
  expect_match(marked$output, "Recorded parameters/year-scope: done")

  update_status <- run_dina_cli(c("update", "status"), root = root)
  expect_equal(update_status$status, 0L)
  expect_match(update_status$output, "Next gate: Admin tax microdata and GPInter")
  expect_false(grepl("sources_unreviewed", update_status$output, fixed = TRUE))
  expect_false(grepl("sources complete", update_status$output, fixed = TRUE))

  unmarked <- run_dina_cli(c("update", "unmark", "parameters/year-scope"), root = root)
  expect_equal(unmarked$status, 0L)
  expect_match(unmarked$output, "Cleared parameters/year-scope")

  removed_update_checklist <- run_dina_cli(c("update", "checklist"), root = root)
  expect_equal(removed_update_checklist$status, 1L)
  expect_match(removed_update_checklist$output, "Use `dina update roadmap`")

  removed_sources_complete <- run_dina_cli(c("sources", "complete", "--status", "no-new-data"), root = root)
  expect_equal(removed_sources_complete$status, 1L)
  expect_match(removed_sources_complete$output, "dina update mark GATE/CHECK")
})

test_that("tasks list shows task language", {
  root <- mini_repo()
  listed <- run_dina_cli(c("tasks", "list"), root = root)
  expect_equal(listed$status, 0L)
  expect_match(listed$output, "language")
  expect_match(listed$output, "task1[[:space:]]+task1[[:space:]]+one[[:space:]]+Stata")
})

test_that("source refresh dry-run is non-mutating and grouped by method", {
  root <- mini_repo()
  remote_dir <- file.path(root, "remote")
  dir.create(remote_dir, recursive = TRUE)
  remote_file <- file.path(remote_dir, "source.txt")
  writeLines("remote source", remote_file)
  dina_write_yaml(list(sources = list(
    list(id = "url-source", family = "fixture", country = "AAA", method = "url", url = paste0("file://", normalizePath(remote_file, mustWork = TRUE)), canonical = c("input_data/url-source.txt"), staging_name = "URL/source.txt", destination = "input_data/url-source.txt"),
    list(id = "manual-source", family = "fixture", country = "AAA", method = "manual", urls = list(list(label = "manual page", url = "https://example.test/manual"), list(label = "manual docs", url = "https://example.test/docs")), canonical = c("input_data/manual-source.txt"), staging_name = "MANUAL/manual-source.txt"),
    list(id = "script-source", family = "fixture", country = "AAA", method = "script", urls = list(list(label = "script page", url = "https://example.test/script")), canonical = c("input_data/script-source.txt"), staging_name = "SCRIPT/script-source.txt"),
    list(id = "wid-source", family = "fixture", country = "AAA", method = "wid", urls = list(list(label = "wid page", url = "https://example.test/wid")), canonical = c("input_data/wid-source.txt"), staging_name = "WID/wid-source.txt")
  )), file.path(root, "config", "sources.yml"))
  session <- dina_update_start("2026", root = root)
  staging_root <- dina_source_staging_root(session, root)

  dry <- run_dina_cli(c("sources", "refresh", "--dry-run"), root = root)
  expect_equal(dry$status, 0L)
  expect_match(dry$output, "Active update: 2026-update")
  expect_match(dry$output, "Staging root: output/updates/")
  expect_match(dry$output, "Targets in the table are relative to the staging root")
  expect_match(dry$output, "Fetchable now")
  expect_match(dry$output, "url-source[[:space:]]+will_fetch")
  expect_match(dry$output, "Manual download/stage")
  expect_match(dry$output, "manual-source[[:space:]]+manual_needed")
  expect_match(dry$output, "Script acquisition")
  expect_match(dry$output, "script-source[[:space:]]+script_needed")
  expect_match(dry$output, "Pipeline online dependency")
  expect_match(dry$output, "wid-source[[:space:]]+wid_pipeline")
  expect_match(dry$output, "URL appendix")
  expect_match(dry$output, "https://example.test/manual")
  expect_match(dry$output, "https://example.test/script")
  expect_match(dry$output, "https://example.test/wid")
  expect_match(dry$output, "1 more URL")
  expect_match(dry$output, "Dry-run only: no folders")
  staging_mentions <- gregexpr("output/updates/", dry$output, fixed = TRUE)[[1]]
  expect_equal(sum(staging_mentions > 0), 1L)
  expect_equal(list.files(staging_root, recursive = TRUE, all.files = TRUE, no.. = TRUE), character())
  expect_equal(length(dina_load_session(root = root)$source_refreshes), 0L)

  expanded <- run_dina_cli(c("sources", "refresh", "--dry-run", "--urls", "--source", "manual-source"), root = root)
  expect_equal(expanded$status, 0L)
  expect_match(expanded$output, "https://example.test/manual")
  expect_match(expanded$output, "https://example.test/docs")
})

test_that("source refresh fetches only URL ZIP sources and records real attempts", {
  root <- mini_repo()
  remote_dir <- file.path(root, "remote")
  dir.create(remote_dir, recursive = TRUE)
  remote_file <- file.path(remote_dir, "source.txt")
  writeLines("remote source", remote_file)
  dina_write_yaml(list(sources = list(
    list(id = "url-source", family = "fixture", country = "AAA", method = "url", url = paste0("file://", normalizePath(remote_file, mustWork = TRUE)), canonical = c("input_data/url-source.txt"), staging_name = "URL/source.txt", destination = "input_data/url-source.txt"),
    list(id = "manual-source", family = "fixture", country = "AAA", method = "manual", canonical = c("input_data/manual-source.txt"), staging_name = "MANUAL/manual-source.txt")
  )), file.path(root, "config", "sources.yml"))
  session <- dina_update_start("2026", root = root)

  refreshed <- run_dina_cli(c("sources", "refresh"), root = root)
  expect_equal(refreshed$status, 0L)
  expect_match(refreshed$output, "url-source[[:space:]]+staged")
  expect_match(refreshed$output, "manual-source[[:space:]]+manual_needed")
  expect_match(refreshed$output, "Next: run `dina sources review`")
  expect_true(file.exists(file.path(dina_source_staging_root(session, root), "URL", "source.txt")))
  expect_false(dir.exists(file.path(dina_source_staging_root(session, root), "MANUAL")))
  loaded <- dina_load_session(root = root)
  expect_equal(length(loaded$source_refreshes), 1L)
  first_refresh <- loaded$source_refreshes[[1]]
  expect_true("url-source" %in% names(first_refresh))
  expect_false("manual-source" %in% names(first_refresh))
})

test_that("manual source staging review and bulk integration are explicit", {
  root <- mini_repo()
  dina_write_yaml(list(sources = list(
    list(id = "manual-ready", family = "fixture", country = "AAA", method = "manual", canonical = c("input_data/manual-ready.csv"), staging_name = "manual/{basename}", destination = "input_data/{basename}"),
    list(id = "manual-ambiguous", family = "fixture", country = "AAA", method = "manual", canonical = c("input_data/manual-ambiguous.csv"), staging_name = "ambiguous/{basename}")
  )), file.path(root, "config", "sources.yml"))
  dina_update_start("2026", root = root)
  dir.create(file.path(root, "downloads"), recursive = TRUE)
  ready_file <- file.path(root, "downloads", "manual-ready.csv")
  ambiguous_file <- file.path(root, "downloads", "manual-ambiguous.csv")
  writeLines("ready", ready_file)
  writeLines("ambiguous", ambiguous_file)

  staged <- run_dina_cli(c("sources", "stage", "--source", "manual-ready", "--file", ready_file), root = root)
  expect_equal(staged$status, 0L)
  expect_match(staged$output, "Staged manual-ready")
  staged_ambiguous <- run_dina_cli(c("sources", "stage", "--source", "manual-ambiguous", "--file", ambiguous_file), root = root)
  expect_equal(staged_ambiguous$status, 0L)

  reviewed <- run_dina_cli(c("sources", "review"), root = root)
  expect_equal(reviewed$status, 0L)
  expect_match(reviewed$output, "Ready for bulk integration")
  expect_match(reviewed$output, "manual-ready")
  expect_match(reviewed$output, "missing_destination")
  expect_match(reviewed$output, "manual-ambiguous")
  expect_false(grepl("unknown_source", reviewed$output, fixed = TRUE))

  preview <- run_dina_cli(c("sources", "integrate", "--all"), root = root)
  expect_equal(preview$status, 0L)
  expect_match(preview$output, "Would integrate")
  expect_match(preview$output, "Skipped")
  expect_false(file.exists(file.path(root, "input_data", "manual-ready.csv")))

  integrated <- run_dina_cli(c("sources", "integrate", "--all", "--yes"), root = root)
  expect_equal(integrated$status, 0L)
  expect_match(integrated$output, "Integrated")
  expect_match(integrated$output, "Skipped")
  expect_true(file.exists(file.path(root, "input_data", "manual-ready.csv")))
  expect_false(file.exists(file.path(root, "input_data", "manual-ambiguous.csv")))
})

test_that("source inbox guide and init use central buckets and safe legacy copying", {
  root <- mini_repo()
  dina_write_yaml(list(sources = list(
    list(
      id = "chl-pit-total",
      family = "admin_tax",
      country = "CHL",
      method = "manual",
      urls = list(list(label = "landing page", url = "https://example.test/chl")),
      canonical = c("input_data/admin_data/CHL/PUB_Total_*.xlsb"),
      inbox = c("input_data/_new/admin_tax/PUB_Total_*.xlsb"),
      legacy_inbox = c("input_data/admin_data/CHL/_new/PUB_Total_*.xlsb"),
      inbox_examples = c("PUB_Total_2024.xlsb"),
      destination = "input_data/admin_data/CHL/{basename}"
    ),
    list(
      id = "surveys-cepal",
      family = "surveys",
      country = "MULTI",
      method = "manual",
      canonical = c("input_data/surveys_CEPAL/*/*.dta"),
      inbox = c("input_data/_new/surveys/*"),
      inbox_examples = c("country survey .dta files", "survey metadata spreadsheets")
    )
  )), file.path(root, "config", "sources.yml"))
  legacy <- file.path(root, "input_data", "admin_data", "CHL", "_new", "PUB_Total_2024.xlsb")
  dir.create(dirname(legacy), recursive = TRUE, showWarnings = FALSE)
  writeLines("legacy", legacy)
  central <- file.path(root, "input_data", "_new", "admin_tax", "PUB_Total_2024.xlsb")

  guide <- run_dina_cli(c("sources", "inbox", "guide"), root = root)
  expect_equal(guide$status, 0L)
  expect_match(guide$output, "Source Inbox Guide")
  expect_match(guide$output, "input_data/_new/admin_tax")
  expect_match(guide$output, "input_data/_new/surveys")
  expect_match(guide$output, "PUB_Total_2024\\.xlsb")
  expect_match(guide$output, "country survey \\.dta files")

  urls <- run_dina_cli(c("sources", "inbox", "guide", "--family", "admin_tax", "--urls"), root = root)
  expect_equal(urls$status, 0L)
  expect_match(urls$output, "https://example.test/chl")
  expect_false(grepl("surveys-cepal", urls$output, fixed = TRUE))

  dry <- run_dina_cli(c("sources", "inbox", "init", "--dry-run"), root = root)
  expect_equal(dry$status, 0L)
  expect_match(dry$output, "would_create")
  expect_match(dry$output, "would_copy")
  expect_false(dir.exists(file.path(root, "input_data", "_new", "admin_tax")))
  expect_false(file.exists(central))

  initialized <- run_dina_cli(c("sources", "inbox", "init"), root = root)
  expect_equal(initialized$status, 0L)
  expect_match(initialized$output, "created")
  expect_match(initialized$output, "copied")
  expect_true(dir.exists(file.path(root, "input_data", "_new", "surveys")))
  expect_true(file.exists(central))

  same <- run_dina_cli(c("sources", "inbox", "init"), root = root)
  expect_equal(same$status, 0L)
  expect_match(same$output, "already_present")

  writeLines("central changed", central)
  conflict <- run_dina_cli(c("sources", "inbox", "init"), root = root)
  expect_equal(conflict$status, 0L)
  expect_match(conflict$output, "conflict")
  expect_equal(readLines(central), "central changed")
})

test_that("incoming _new source inbox files are reviewed, validated, integrated, and ignored for freshness", {
  root <- mini_repo()
  dina_write_yaml(list(sources = list(
    list(
      id = "chl-pit-total",
      family = "admin_tax",
      country = "CHL",
      method = "manual",
      canonical = c("input_data/admin_data/CHL/PUB_Total_*.xlsb"),
      inbox = c("input_data/_new/admin_tax/PUB_Total_*.xlsb"),
      legacy_inbox = c("input_data/admin_data/CHL/_new/PUB_Total_*.xlsb"),
      inbox_examples = c("PUB_Total_2024.xlsb"),
      destination = "input_data/admin_data/CHL/{basename}",
      checks = c("file_exists", "years_detected")
    )
  )), file.path(root, "config", "sources.yml"))
  incoming <- file.path(root, "input_data", "_new", "admin_tax", "PUB_Total_2023.xlsb")
  canonical_old <- file.path(root, "input_data", "admin_data", "CHL", "PUB_Total_2022.xlsb")
  touch(canonical_old, "2024-01-01")
  touch(incoming, "2024-01-05")
  writeLines("incoming", incoming)
  dina_update_start("2026", root = root)

  latest <- dina_latest_mtime("input_data/admin_data/CHL", root = root, ignore = TRUE, recursive_dirs = TRUE)
  expect_equal(format(latest, "%Y-%m-%d"), "2024-01-01")

  review <- run_dina_cli(c("sources", "review"), root = root)
  expect_equal(review$status, 0L)
  expect_match(review$output, "Source Inbox Guide")
  expect_match(review$output, "input_data/_new/admin_tax")
  expect_match(review$output, "PUB_Total_2024\\.xlsb")
  expect_match(review$output, "Incoming Source Inbox")
  expect_match(review$output, "chl-pit-total[[:space:]]+file[[:space:]]+ok")
  expect_match(review$output, "input_data/_new/admin_tax/PUB_Total_2023\\.xlsb")

  preview <- run_dina_cli(c("sources", "integrate", "--incoming", "--source", "chl-pit-total"), root = root)
  expect_equal(preview$status, 0L)
  expect_match(preview$output, "Would integrate incoming")
  expect_false(file.exists(file.path(root, "input_data", "admin_data", "CHL", "PUB_Total_2023.xlsb")))

  integrated <- run_dina_cli(c("sources", "integrate", "--incoming", "--source", "chl-pit-total", "--yes"), root = root)
  expect_equal(integrated$status, 0L)
  expect_match(integrated$output, "Integrated incoming")
  expect_true(file.exists(file.path(root, "input_data", "admin_data", "CHL", "PUB_Total_2023.xlsb")))
  decisions <- dina_load_session(root = root)$source_decisions
  expect_equal(decisions[[length(decisions)]]$origin, "incoming")
})

test_that("update lifecycle commands list, dry-run delete, delete, and restart safely", {
  root <- mini_repo()
  first <- dina_update_start("2026", root = root)

  listed <- run_dina_cli(c("update", "list"), root = root)
  expect_equal(listed$status, 0L)
  expect_match(listed$output, "Update Sessions")
  expect_match(listed$output, first$id)
  expect_match(listed$output, "\\*[[:space:]]+2026-update")

  dry_delete <- run_dina_cli(c("update", "delete"), root = root)
  expect_equal(dry_delete$status, 0L)
  expect_match(dry_delete$output, "Would delete update")
  expect_true(dir.exists(dina_update_dir(first$id, root)))
  expect_equal(dina_current_update(root), first$id)

  dry_restart <- run_dina_cli(c("update", "restart"), root = root)
  expect_equal(dry_restart$status, 0L)
  expect_match(dry_restart$output, "Would reset update")
  expect_match(dry_restart$output, "Year: 2026")
  expect_match(dry_restart$output, "Current status: initialized")
  expect_match(dry_restart$output, "Files to clear:")
  expect_match(dry_restart$output, "same update id")
  expect_equal(dina_load_session(root = root)$status, "initialized")

  writeLines("old staged file", file.path(dina_update_dir(first$id, root), "source_staging", "old.txt"))
  writeLines("old log", file.path(dina_update_dir(first$id, root), "logs", "old.log"))
  session <- dina_load_session(root = root)
  session$source_refreshes <- list(old = list(status = "done"))
  session$task_runs <- list(old = list(status = "done"))
  dina_save_session(session, root)

  restarted <- run_dina_cli(c("update", "restart", "--yes"), root = root)
  expect_equal(restarted$status, 0L)
  expect_match(restarted$output, "Update Restart")
  expect_match(restarted$output, "Resetting session directory")
  expect_match(restarted$output, "Preparing update session")
  expect_match(restarted$output, "Source baseline summary")
  expect_match(restarted$output, "Writing manifest and active update pointer")
  restarted_id <- dina_current_update(root)
  expect_equal(restarted_id, first$id)
  restarted_session <- dina_load_session(first$id, root)
  expect_equal(restarted_session$status, "initialized")
  expect_equal(length(restarted_session$source_refreshes), 0L)
  expect_equal(length(restarted_session$task_runs), 0L)
  expect_true(is.null(restarted_session$successor_update))
  expect_false(file.exists(file.path(dina_update_dir(first$id, root), "source_staging", "old.txt")))
  expect_false(file.exists(file.path(dina_update_dir(first$id, root), "logs", "old.log")))
  expect_true(dir.exists(dina_update_dir(first$id, root)))

  dry_delete_id <- run_dina_cli(c("update", "delete", first$id), root = root)
  expect_equal(dry_delete_id$status, 0L)
  expect_match(dry_delete_id$output, first$id)
  expect_true(dir.exists(dina_update_dir(first$id, root)))

  deleted <- run_dina_cli(c("update", "delete", first$id, "--yes"), root = root)
  expect_equal(deleted$status, 0L)
  expect_false(dir.exists(dina_update_dir(first$id, root)))
  expect_true(is.null(dina_current_update(root)))

  start_root <- mini_repo()
  existing <- dina_update_start("2027", root = start_root)
  refused <- run_dina_cli(c("update", "start", "2027"), root = start_root)
  expect_equal(refused$status, 1L)
  expect_match(refused$output, "Unfinished same-day update already exists")
  expect_equal(dina_current_update(start_root), existing$id)
  expect_false(dir.exists(dina_update_dir(paste0(existing$id, "-02"), start_root)))

  suffixed <- run_dina_cli(c("update", "start", "2027", "--yes"), root = start_root)
  expect_equal(suffixed$status, 0L)
  expect_match(suffixed$output, "-02")
  expect_equal(dina_current_update(start_root), paste0(existing$id, "-02"))

  expect_true(dina_confirm_response("y"))
  expect_true(dina_confirm_response("yes"))
  expect_false(dina_confirm_response(""))
  expect_false(dina_confirm_response("no"))
  con <- textConnection("y\n")
  prompt_output <- capture.output(confirmed <- dina_confirm_continue(input = con, is_terminal = TRUE))
  close(con)
  expect_true(confirmed)
  expect_match(paste(prompt_output, collapse = "\n"), "Continue\\? \\[y/N\\]")

  con <- textConnection("\n")
  capture.output(rejected <- dina_confirm_continue(input = con, is_terminal = TRUE))
  close(con)
  expect_false(rejected)
  con <- textConnection("y\n")
  expect_false(dina_confirm_continue(input = con, is_terminal = FALSE))
  close(con)

  help <- run_dina_cli(c("help", "update"))
  expect_equal(help$status, 0L)
  expect_match(help$output, "dina update list")
  expect_match(help$output, "dina update restart \\[ID\\] \\[--yes\\]")
  expect_match(help$output, "dina update delete \\[ID\\] \\[--yes\\]")
})

test_that("update commands agree when active session manifest is missing", {
  root <- mini_repo()
  session <- dina_update_start("2026", root = root)
  unlink(dina_session_manifest_path(session$id, root))

  status <- run_dina_cli(c("update", "status"), root = root)
  expect_equal(status$status, 0L)
  expect_match(status$output, "manifest\\.json is missing")
  expect_match(status$output, paste0("dina update restart ", session$id))

  listed <- run_dina_cli(c("update", "list"), root = root)
  expect_equal(listed$status, 0L)
  expect_match(listed$output, "missing_manifest")
  expect_match(listed$output, session$id)

  restart <- run_dina_cli(c("update", "restart"), root = root)
  expect_equal(restart$status, 0L)
  expect_match(restart$output, "Current status: missing_manifest")
  expect_match(restart$output, "Restart reuses the same update id")

  start <- run_dina_cli(c("update", "start", "2026"), root = root)
  expect_equal(start$status, 1L)
  expect_match(start$output, "manifest is missing")
  expect_match(start$output, paste0("dina update restart ", session$id))
  expect_match(start$output, paste0(session$id, "-02"))

  restarted <- run_dina_cli(c("update", "restart", "--yes"), root = root)
  expect_equal(restarted$status, 0L)
  expect_match(restarted$output, "Restarted update")
  expect_equal(dina_current_update(root), session$id)
  expect_true(file.exists(dina_session_manifest_path(session$id, root)))
})

test_that("Git ignore keeps update records trackable and local Pushover private", {
  ignore <- trimws(readLines(file.path(repo_root_for_tests, ".gitignore"), warn = FALSE))
  expect_false(any(ignore %in% c("output/archives/", "output/run_logs/", "output/updates/")))
  expect_true(any(ignore == "config/pushover.local.R"))
})
