numbered_pipeline <- function(root) {
  dina_write_yaml(list(tasks = list(
    list(id = "01a-clean-macro-data", stage = "macro", type = "stata", script = "code/Stata/01a.do", deps = list(), inputs = c("input_data/a.txt"), outputs = c("output/a.txt")),
    list(id = "01b-add-country-sna", stage = "macro", type = "stata", script = "code/Stata/01b.do", deps = c("01a-clean-macro-data"), inputs = c("output/a.txt"), outputs = c("output/b.txt")),
    list(id = "02a-get-survey-populations", stage = "preparation", type = "stata", script = "code/Stata/02a.do", deps = c("01b-add-country-sna"), inputs = c("output/b.txt"), outputs = c("output/c.txt")),
    list(id = "07d-export-results-to-wid", stage = "export", type = "stata", script = "code/Stata/07d.do", deps = c("02a-get-survey-populations"), inputs = c("output/c.txt"), outputs = c("output/d.txt"))
  )), file.path(root, "config", "pipeline.yml"))
}

run_dina_cli <- function(args, root = repo_root_for_tests) {
  output <- system2(
    file.path(repo_root_for_tests, "bin", "dina"),
    args,
    stdout = TRUE,
    stderr = TRUE,
    env = c(sprintf("DINA_REPO_ROOT=%s", root))
  )
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  list(status = status, output = paste(output, collapse = "\n"))
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
  commands <- list(
    c("help"),
    c("--help"),
    c("--", "help"),
    c("help", "workflow"),
    c("sources"),
    c("config"),
    c("data"),
    c("tasks"),
    c("notify", "--help"),
    c("--", "config", "--help"),
    c("--", "run", "01a", "--dry-run")
  )

  for (args in commands) {
    result <- run_dina_cli(args)
    expect_equal(result$status, 0L)
  }
})

test_that("dashboard offers executable numbered actions without prompting in non-interactive runs", {
  result <- run_dina_cli(character())
  expect_equal(result$status, 0L)
  year <- format(Sys.Date(), "%Y")
  if (grepl("No active update session.", result$output, fixed = TRUE)) {
    expect_match(result$output, sprintf("Recommended next action: Start an update with `dina update start %s`", year))
  } else {
    expect_match(result$output, "Active update:")
    expect_match(result$output, "Recommended next action:")
  }
  expect_match(result$output, "Useful actions:")
  expect_match(result$output, "1\\. dina doctor")
  expect_match(result$output, sprintf("2\\. dina update start %s", year))
  expect_match(result$output, "3\\. dina update resume")
  expect_match(result$output, "4\\. dina sources scan")
  expect_match(result$output, "5\\. dina tasks list")
  expect_match(result$output, "6\\. dina run --dry-run")
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
  expect_match(main$output, "`sources refresh \\[--dry-run\\]`[[:space:]]+\\[writes session\\]")
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
  expect_match(main$output, "`dina run` is dry-run unless `--execute`")
  expect_match(main$output, "does not edit `_config.do`")
  expect_match(main$output, "dina help workflow")
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
  expect_match(workflow$output, "recommended order")
  expect_match(workflow$output, "1\\. Check the machine")
  expect_match(workflow$output, "dina update start \\[YEAR\\]")
  expect_match(workflow$output, "If YEAR is omitted")
  expect_match(workflow$output, "dina sources refresh")
  expect_match(workflow$output, "Task selectors:")
  expect_match(workflow$output, "`tasks why` needs one unique task selector")
  expect_match(workflow$output, "dina update finalize")

  run <- run_dina_cli(c("help", "run"))
  expect_equal(run$status, 0L)
  expect_match(run$output, "--task 01a,01b[[:space:]]+Same selection")
  expect_match(run$output, "--from 03 --to 05[[:space:]]+Range")

  tasks <- run_dina_cli(c("help", "tasks"))
  expect_equal(tasks$status, 0L)
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
  expect_match(help$output, "url[[:space:]]+Direct URL fetchable")
  expect_match(help$output, "manual[[:space:]]+Human-curated input or URL index")
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
  expect_match(dry_restart$output, "Would preserve update")
  expect_equal(dina_load_session(root = root)$status, "initialized")

  restarted <- run_dina_cli(c("update", "restart", "--yes"), root = root)
  expect_equal(restarted$status, 0L)
  second_id <- dina_current_update(root)
  expect_false(identical(second_id, first$id))
  old_session <- dina_load_session(first$id, root)
  expect_equal(old_session$status, "abandoned")
  expect_equal(old_session$successor_update, second_id)
  expect_true(dir.exists(dina_update_dir(first$id, root)))
  expect_true(dir.exists(dina_update_dir(second_id, root)))

  dry_delete_id <- run_dina_cli(c("update", "delete", second_id), root = root)
  expect_equal(dry_delete_id$status, 0L)
  expect_match(dry_delete_id$output, second_id)
  expect_true(dir.exists(dina_update_dir(second_id, root)))

  deleted <- run_dina_cli(c("update", "delete", second_id, "--yes"), root = root)
  expect_equal(deleted$status, 0L)
  expect_false(dir.exists(dina_update_dir(second_id, root)))
  expect_null(dina_current_update(root))

  help <- run_dina_cli(c("help", "update"))
  expect_equal(help$status, 0L)
  expect_match(help$output, "dina update list")
  expect_match(help$output, "dina update restart \\[YEAR\\] \\[--yes\\]")
  expect_match(help$output, "dina update delete \\[ID\\] \\[--yes\\]")
})

test_that("Git ignore keeps update records trackable and local Pushover private", {
  ignore <- trimws(readLines(file.path(repo_root_for_tests, ".gitignore"), warn = FALSE))
  expect_false(any(ignore %in% c("output/archives/", "output/run_logs/", "output/updates/")))
  expect_true(any(ignore == "config/pushover.local.R"))
})
