repo_root_for_tests <- if (file.exists(file.path(getwd(), "code", "R", "cli", "dina_lib.R"))) {
  normalizePath(getwd(), mustWork = TRUE)
} else {
  normalizePath(file.path(getwd(), "..", ".."), mustWork = TRUE)
}
source(file.path(repo_root_for_tests, "code", "R", "cli", "dina_lib.R"))

source_cli_for_tests <- function() {
  old_source_only <- Sys.getenv("DINA_CLI_SOURCE_ONLY", unset = NA_character_)
  old_source_file <- Sys.getenv("DINA_CLI_SOURCE_FILE", unset = NA_character_)
  on.exit({
    if (is.na(old_source_only)) Sys.unsetenv("DINA_CLI_SOURCE_ONLY") else Sys.setenv(DINA_CLI_SOURCE_ONLY = old_source_only)
    if (is.na(old_source_file)) Sys.unsetenv("DINA_CLI_SOURCE_FILE") else Sys.setenv(DINA_CLI_SOURCE_FILE = old_source_file)
  }, add = TRUE)
  cli_path <- file.path(repo_root_for_tests, "code", "R", "cli", "dina.R")
  Sys.setenv(DINA_CLI_SOURCE_ONLY = "1", DINA_CLI_SOURCE_FILE = cli_path)
  sys.source(cli_path, envir = parent.frame())
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

numbered_pipeline <- function(root) {
  dina_write_yaml(list(tasks = list(
    list(id = "01a-clean-macro-data", stage = "macro", type = "stata", script = "code/Stata/01a.do", deps = list(), inputs = c("input_data/a.txt"), outputs = c("output/a.txt")),
    list(id = "01b-add-country-sna", stage = "macro", type = "stata", script = "code/Stata/01b.do", deps = c("01a-clean-macro-data"), inputs = c("output/a.txt"), outputs = c("output/b.txt")),
    list(id = "02a-get-survey-populations", stage = "preparation", type = "stata", script = "code/Stata/02a.do", deps = c("01b-add-country-sna"), inputs = c("output/b.txt"), outputs = c("output/c.txt")),
    list(id = "07d-export-results-to-wid", stage = "export", type = "stata", script = "code/Stata/07d.do", deps = c("02a-get-survey-populations"), inputs = c("output/c.txt"), outputs = c("output/d.txt"))
  )), file.path(root, "config", "pipeline.yml"))
}

fake_executable <- function(dir, name) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(dir, name)
  writeLines(c("#!/bin/sh", "exit 0"), path)
  Sys.chmod(path, "0755")
  path
}

mini_repo <- function() {
  root <- tempfile("dina-mini-repo-")
  dir.create(root, recursive = TRUE)
  if (requireNamespace("withr", quietly = TRUE)) {
    withr::defer(unlink(root, recursive = TRUE), envir = parent.frame())
  }
  dir.create(file.path(root, "config"), recursive = TRUE)
  dir.create(file.path(root, "code", "Stata"), recursive = TRUE)
  dir.create(file.path(root, "code", "R"), recursive = TRUE)
  dir.create(file.path(root, "input_data"), recursive = TRUE)
  dir.create(file.path(root, "output"), recursive = TRUE)
  dir.create(file.path(root, "previous_series"), recursive = TRUE)

  dina_write_yaml(list(
    project = list(name = "mini"),
    countries = c("COL", "ARG"),
    years = list(first = 2000L, last = 2023L),
    run = list(lang = "eng", debug = FALSE, bfm_replace = FALSE, units = c("ind", "esn"), steps = c("natinc", "pon")),
    export_validation = list(
      unit = "esn",
      steps = c("nat"),
      last_year = 2024L,
      previous_update_date = "3Oct2024",
      previous_update_file = "previous_series/dina_latam_3Oct2024.dta"
    ),
    stata = list(command = "${DINA_STATA_CMD}", batch_args = c("-b", "do")),
    paths = list(
      input_data = "input_data",
      output = "output",
      previous_series = "previous_series",
      updates = "output/updates",
      run_logs = "output/run_logs",
      final_outputs = c("output/latest_wid_series")
    ),
    notifications = list(pushover = list(enabled = FALSE, token = "${PUSHOVER_APP_TOKEN}", user = "${PUSHOVER_USER_KEY}")),
    archives = list(default_dir = "output/archives", primary_paths = c("input_data", "previous_series")),
    dependencies = list(r_packages = c("cli", "yaml", "jsonlite"))
  ), file.path(root, "config", "dina.yml"))

  writeLines("display \"task 1\"", file.path(root, "code", "Stata", "01a.do"))
  writeLines("display \"task 2\"", file.path(root, "code", "Stata", "02a.do"))

  dina_write_yaml(list(tasks = list(
    list(id = "task1", stage = "one", type = "stata", script = "code/Stata/01a.do", deps = list(), inputs = c("input_data/a.txt"), outputs = c("output/a.txt")),
    list(id = "task2", stage = "two", type = "stata", script = "code/Stata/02a.do", deps = c("task1"), inputs = c("output/a.txt"), outputs = c("output/b.txt"))
  )), file.path(root, "config", "pipeline.yml"))

  dina_write_yaml(list(sources = list(
    list(
      id = "source-a",
      family = "fixture",
      country = "AAA",
      method = "manual",
      urls = list(list(label = "fixture page", url = "https://example.test/source-a")),
      canonical = c("input_data/source_*.xlsx"),
      inbox = c("input_data/_new/fixture/source_*.xlsx"),
      inbox_examples = c("source_2024.xlsx"),
      destination = "input_data/{basename}",
      transformer = "code/Stata/01a.do",
      notes = "Fixture source used for source list and inbox review tests.",
      checks = c("file_exists", "years_detected")
    ),
    list(
      id = "source-missing",
      family = "fixture",
      country = "BBB",
      method = "manual",
      canonical = c("input_data/missing_*.csv"),
      integration = "none",
      notes = "Reference-only missing-source fixture.",
      checks = c("file_exists")
    )
  )), file.path(root, "config", "sources.yml"))

  dina_write_yaml(list(items = list(
    list(id = "review-config", label = "Review update configuration."),
    list(id = "review-sources", label = "Review incoming source files."),
    list(id = "run-pipeline", label = "Run stale or selected pipeline tasks.")
  )), file.path(root, "config", "todo.yml"))

  root
}

touch <- function(path, time) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (!file.exists(path)) writeLines("x", path)
  Sys.setFileTime(path, as.POSIXct(time, tz = "UTC"))
}
