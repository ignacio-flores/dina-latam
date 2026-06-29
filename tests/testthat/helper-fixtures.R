repo_root_for_tests <- if (file.exists(file.path(getwd(), "code", "R", "cli", "dina_lib.R"))) {
  normalizePath(getwd(), mustWork = TRUE)
} else {
  normalizePath(file.path(getwd(), "..", ".."), mustWork = TRUE)
}
source(file.path(repo_root_for_tests, "code", "R", "cli", "dina_lib.R"))

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

  writeLines(c(
    "* Configuration file",
    "global all_countries \" \"COL\" \"ARG\" \" \"",
    "global first_y 2000",
    "global last_y 2023",
    "global lang \"eng\"",
    "global debug \"no\"",
    "global bfm_replace \"no\"",
    "global all_units \" \"ind\" \"esn\" \" \"",
    "global all_steps \" \"natinc\" \"pon\" \" \""
  ), file.path(root, "_config.do"))

  dina_write_yaml(list(
    project = list(name = "mini"),
    countries = c("COL", "ARG"),
    years = list(first = 2000L, last = 2023L),
    run = list(lang = "eng", debug = FALSE, bfm_replace = FALSE, units = c("ind", "esn"), steps = c("natinc", "pon")),
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
    list(id = "source-a", family = "fixture", country = "AAA", method = "manual", canonical = c("input_data/source_*.xlsx"), staging_name = "AAA/source_{date}.xlsx", checks = c("file_exists", "years_detected")),
    list(id = "source-missing", family = "fixture", country = "BBB", method = "manual", canonical = c("input_data/missing_*.csv"), staging_name = "BBB/missing_{date}.csv", checks = c("file_exists"))
  )), file.path(root, "config", "sources.yml"))

  root
}

touch <- function(path, time) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (!file.exists(path)) writeLines("x", path)
  Sys.setFileTime(path, as.POSIXct(time, tz = "UTC"))
}
