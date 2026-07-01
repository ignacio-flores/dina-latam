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
    "* Runtime DINA config loader.",
    "local dina_config_runtime : environment DINA_CONFIG_DO",
    "if \"`dina_config_runtime'\" != \"\" {",
    "\trun \"`dina_config_runtime'\"",
    "\texit",
    "}",
    "di as error \"DINA config globals are no longer stored in _config.do.\"",
    "exit 198"
  ), file.path(root, "_config.do"))

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
    list(id = "source-a", family = "fixture", country = "AAA", method = "manual", canonical = c("input_data/source_*.xlsx"), staging_name = "AAA/source_{date}.xlsx", destination = "input_data/{basename}", checks = c("file_exists", "years_detected")),
    list(id = "source-missing", family = "fixture", country = "BBB", method = "manual", canonical = c("input_data/missing_*.csv"), staging_name = "BBB/missing_{date}.csv", checks = c("file_exists"))
  )), file.path(root, "config", "sources.yml"))

  dina_write_yaml(list(gates = list(
    list(
      id = "parameters",
      label = "Update parameters",
      goal = "Review update parameters.",
      source_families = list(),
      tasks = list(),
      old_refs = c("Preliminary: include new last year in the update config."),
      checks = list(
        list(id = "year-scope", label = "Year scope reviewed.")
      ),
      commands = c("dina config show")
    ),
    list(
      id = "tax-admin",
      label = "Admin tax microdata and GPInter",
      goal = "Review admin tax inputs.",
      source_families = c("fixture"),
      tasks = c("task1"),
      checks = list(
        list(id = "raw-accepted", label = "Raw files accepted."),
        list(id = "structure-validated", label = "Structure validated.")
      ),
      commands = c("dina sources review")
    )
  )), file.path(root, "config", "update_roadmap.yml"))

  root
}

touch <- function(path, time) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (!file.exists(path)) writeLines("x", path)
  Sys.setFileTime(path, as.POSIXct(time, tz = "UTC"))
}
