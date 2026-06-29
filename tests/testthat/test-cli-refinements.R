numbered_pipeline <- function(root) {
  dina_write_yaml(list(tasks = list(
    list(id = "01a-clean-macro-data", stage = "macro", type = "stata", script = "code/Stata/01a.do", deps = list(), inputs = c("input_data/a.txt"), outputs = c("output/a.txt")),
    list(id = "01b-add-country-sna", stage = "macro", type = "stata", script = "code/Stata/01b.do", deps = c("01a-clean-macro-data"), inputs = c("output/a.txt"), outputs = c("output/b.txt")),
    list(id = "02a-get-survey-populations", stage = "preparation", type = "stata", script = "code/Stata/02a.do", deps = c("01b-add-country-sna"), inputs = c("output/b.txt"), outputs = c("output/c.txt")),
    list(id = "07d-export-results-to-wid", stage = "export", type = "stata", script = "code/Stata/07d.do", deps = c("02a-get-survey-populations"), inputs = c("output/c.txt"), outputs = c("output/d.txt"))
  )), file.path(root, "config", "pipeline.yml"))
}

run_dina_cli <- function(args) {
  output <- system2(file.path(repo_root_for_tests, "bin", "dina"), args, stdout = TRUE, stderr = TRUE)
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
  expect_match(listed$output, "country-sna-index[[:space:]]+country_sna[[:space:]]+MULTI[[:space:]]+manual_index[[:space:]]+1[[:space:]]+yes")
  expect_match(listed$output, "wid-prices-xrates[[:space:]]+prices[[:space:]]+MULTI[[:space:]]+stata_wid")
  expect_match(listed$output, "wb-xrates[[:space:]]+prices[[:space:]]+MULTI[[:space:]]+manual")
  expect_match(listed$output, "downloader")
  expect_match(listed$output, "transformer")

  urls <- run_dina_cli(c("sources", "list", "--urls"))
  expect_equal(urls$status, 0L)
  expect_match(urls$output, "ARG: https://sitioanterior.indec.gob.ar")
  expect_match(urls$output, "https://wid.world/")

  shown <- run_dina_cli(c("sources", "show", "country-sna-index"))
  expect_equal(shown$status, 0L)
  expect_match(shown$output, "input_data/sna_country_data/_sna-web-site-index\\.ods")
  expect_match(shown$output, "BRA single-year downloads")
  expect_match(shown$output, "https://www.inegi.org.mx/datos/\\?t=0190")

  missing <- run_dina_cli(c("sources", "show", "does-not-exist"))
  expect_equal(missing$status, 1L)
  expect_match(missing$output, "Unknown source id: does-not-exist")

  help <- run_dina_cli(c("help", "sources"))
  expect_equal(help$status, 0L)
  expect_match(help$output, "dina sources list \\[--family FAMILY\\] \\[--country ISO\\] \\[--urls\\]")
  expect_match(help$output, "dina sources show ID \\[--urls\\]")
})

test_that("Git ignore keeps update records trackable and local Pushover private", {
  ignore <- trimws(readLines(file.path(repo_root_for_tests, ".gitignore"), warn = FALSE))
  expect_false(any(ignore %in% c("output/archives/", "output/run_logs/", "output/updates/")))
  expect_true(any(ignore == "config/pushover.local.R"))
})
