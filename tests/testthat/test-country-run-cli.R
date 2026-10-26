# CLI integration uses isolated repositories and mocked workers; no scientific run.
country_cli_fixture <- function(root) {
  ids <- c("01a-clean-macro-data", "03a-run-bfm", "03b-run-bfm-2stage", "03c-graph-thetas", "06a-run-ineqstats", "07d-export-results-to-wid")
  tasks <- lapply(ids, function(id) list(id = id, stage = "fixture", type = "r", script = "code/R/fixture.R",
    deps = list(), inputs = list(), outputs = list(), country_scope = if (id %in% c(ids[4], ids[6])) "shared" else "selected"))
  dina_write_yaml(list(tasks = tasks), file.path(root, "config", "pipeline.yml"))
  root
}

country_cli_bind <- function(env, values, cleanup = parent.frame()) {
  for (name in names(values)) local({
    key <- name
    present <- exists(key, envir = env, inherits = FALSE)
    old <- if (present) get(key, envir = env, inherits = FALSE) else NULL
    withr::defer(if (present) assign(key, old, envir = env) else rm(list = key, envir = env), envir = cleanup)
    assign(key, values[[key]], envir = env)
  })
}

test_that("country previews preserve task order and complete effective configuration", {
  source_cli_for_tests()
  root <- mini_repo()
  country_cli_fixture(root)
  env <- environment(dina_cmd_run)
  calls <- list()
  country_cli_bind(env, list(
    dina_country_run_load = function() NULL,
    country_run_preview = function(task, root, session, countries, force, run_context, planned_tasks) {
      calls[[length(calls) + 1L]] <<- list(task = task$id, countries = countries, prior = planned_tasks,
        universe = dina_session_config(session, root)$countries)
      list(task = task$id, status = "planned", compute = countries, shared = if (task$country_scope == "shared") "complete panel" else character())
    },
    country_run_begin = function(...) stop("A preview must not write a run"),
    dina_cli_run_task = function(...) stop("A preview must not launch a worker")
  ))
  output <- capture.output(result <- dina_cmd_run(root, c("--from", "03a", "--to", "03c", "--countries", "ARG", "--dry-run")))
  expect_equal(vapply(calls, `[[`, "", "task"), c("03a-run-bfm", "03b-run-bfm-2stage", "03c-graph-thetas"))
  expect_equal(calls[[1]]$prior, character())
  expect_equal(calls[[3]]$prior, c("03a-run-bfm", "03b-run-bfm-2stage"))
  expect_true(all(vapply(calls, function(call) identical(call$countries, "ARG"), logical(1))))
  expect_true(all(vapply(calls, function(call) identical(call$universe, c("COL", "ARG")), logical(1))))
  expect_true(any(grepl("complete panel", output, fixed = TRUE)))
  expect_false(dir.exists(file.path(root, "output", "run_logs")))

  calls <- list()
  capture.output(dina_cmd_run(root, c("list", "--countries=COL")))
  expect_equal(length(calls), 5L)
  expect_false(any(vapply(calls, function(call) startsWith(call$task, "01"), logical(1))))
  calls <- list()
  capture.output(dina_cmd_tasks(root, c("why", "06a", "--countries", "ARG")))
  expect_equal(vapply(calls, `[[`, "", "task"), "06a-run-ineqstats")
})

test_that("country options reject unsupported or ambiguous requests", {
  source_cli_for_tests()
  root <- mini_repo()
  country_cli_fixture(root)
  env <- environment(dina_cmd_run)
  country_cli_bind(env, list(dina_country_run_load = function() NULL))
  expect_error(dina_cmd_run(root, c("01a", "--countries", "ARG", "--dry-run")), "supports tasks 03a through 07d")
  expect_error(dina_cmd_run(root, c("--from", "01a", "--to", "03a", "--countries", "ARG", "--dry-run")), "full configured run")
  expect_error(dina_cmd_run(root, c("03a", "--countries", "ZZZ", "--dry-run")), "outside the effective configuration")
  expect_error(dina_cmd_run(root, c("03a", "--countries")), "Missing value for --countries")
  expect_error(dina_cmd_run(root, c("03a", "--countries=")), "Missing value for --countries")
  expect_error(dina_cmd_run(root, c("--resume")), "Missing value for --resume")
  expect_error(dina_cmd_run(root, c("--resume", "saved", "--force")), "cannot be combined with --force")
  expect_error(dina_cmd_run(root, c("03a", "--resume", "saved")), "saved task selection")
})

test_that("scoped execution and resume delegate without certifying a full task run", {
  source_cli_for_tests()
  root <- mini_repo()
  country_cli_fixture(root)
  env <- environment(dina_cmd_run)
  calls <- list(); begins <- list()
  country_cli_bind(env, list(
    dina_country_run_load = function() NULL,
    dina_pushover_status = function(...) list(enabled = FALSE, configured = FALSE),
    dina_assert_current_config_validation = function(...) NULL,
    dina_pipeline_input_preflight = function(tasks, root, session, progress, countries) {
      expect_equal(countries, "ARG")
      list(admin_trust_snapshot = list(created = FALSE))
    },
    country_run_preview = function(task, ...) list(task = task$id, status = "planned", compute = "ARG", issues = character()),
    country_run_read = function(root, id) list(id = id, tasks = "06a-run-ineqstats", countries = "ARG"),
    country_run_begin = function(root, tasks, config, countries, resume = NULL, dry_run = FALSE) {
      begins[[length(begins) + 1L]] <<- list(dry = dry_run, countries = countries)
      list(id = resume %||% "fixture-run", countries = countries %||% "ARG", tasks = vapply(tasks, `[[`, "", "id"))
    },
    country_run_finish = function(context, status) { expect_equal(status, "succeeded"); invisible(context) },
    country_run_execute = function(task, root, session, countries, force, progress, run_context) {
      calls[[length(calls) + 1L]] <<- list(task = task$id, countries = countries, force = force, id = run_context$id)
      list(task = task$id, status = "succeeded", log_dir = "output/run_logs/fixture-run")
    },
    dina_cli_run_task = function(...) stop("Scoped work must use the country runner")
  ))
  capture.output(dina_cmd_run(root, c("06a", "--countries", "ARG", "--force")))
  expect_equal(calls[[1]]$countries, "ARG")
  expect_true(calls[[1]]$force)
  expect_equal(length(begins), 1L)
  expect_false(begins[[1]]$dry)
  capture.output(dina_cmd_run(root, c("--resume", "fixture-run", "--dry-run")))
  expect_equal(length(calls), 1L)
  expect_true(tail(begins, 1)[[1]]$dry)
  capture.output(dina_cmd_run(root, c("--resume", "fixture-run")))
  expect_equal(length(calls), 2L)
  expect_equal(calls[[2]]$countries, "ARG")
  expect_equal(calls[[2]]$id, "fixture-run")
})

test_that("omitted countries and end use the same resumable execution as an explicit full scope", {
  source_cli_for_tests()
  root <- mini_repo()
  file.copy(file.path(repo_root_for_tests, "config/pipeline.yml"), file.path(root, "config/pipeline.yml"), overwrite = TRUE)
  env <- environment(dina_cmd_run)
  universe <- dina_session_config(NULL, root, expand_env = FALSE)$countries
  expected <- vapply(dina_select_tasks(root, from = "04b", to = "07d"), `[[`, "", "id")
  calls <- list(); begins <- list(); notices <- list(); finishes <- character()
  country_cli_bind(env, list(
    dina_country_run_load = function() NULL,
    dina_pushover_status = function(...) list(enabled = FALSE, configured = TRUE),
    dina_notify = function(message, root) { notices[[length(notices) + 1L]] <<- message; invisible(NULL) },
    dina_assert_current_config_validation = function(...) NULL,
    dina_pipeline_input_preflight = function(tasks, root, session, progress, countries) {
      expect_identical(countries, universe)
      list(admin_trust_snapshot = list(created = FALSE))
    },
    country_run_preview = function(task, root, session, countries, ...) {
      expect_identical(countries, universe)
      list(task = task$id, status = "planned", compute = countries, issues = character())
    },
    country_run_begin = function(root, tasks, config, countries, resume = NULL, dry_run = FALSE) {
      begins[[length(begins) + 1L]] <<- list(tasks = vapply(tasks, `[[`, "", "id"), countries = countries, dry = dry_run)
      list(id = "fixture-all-country-run", countries = countries, tasks = vapply(tasks, `[[`, "", "id"))
    },
    country_run_finish = function(context, status) { finishes <<- c(finishes, status); invisible(NULL) },
    country_run_execute = function(task, root, session, countries, force, progress, run_context) {
      calls[[length(calls) + 1L]] <<- list(task = task$id, countries = countries, context = run_context$id)
      list(task = task$id, status = "succeeded", log_dir = "fixture-logs")
    },
    dina_cli_run_task = function(...) stop("Omitting countries must not use the legacy launcher for supported tasks")
  ))
  capture.output(dina_cmd_run(root, c("--from", "04b", "--notify")))
  implicit <- calls
  calls <- list()
  capture.output(dina_cmd_run(root, c("--from", "04b", "--to", "07d", "--countries", paste(universe, collapse = ","), "--notify")))
  expect_identical(implicit, calls)
  expect_identical(vapply(implicit, `[[`, "", "task"), expected)
  expect_identical(expected[[1L]], "04b-build-pretax-surveys")
  expect_identical(tail(expected, 1L), "07d-export-results-to-wid")
  expect_length(begins, 2L)
  expect_identical(begins[[1L]], begins[[2L]])
  expect_false(begins[[1L]]$dry)
  expect_identical(begins[[1L]]$countries, universe)
  expect_identical(finishes, c("succeeded", "succeeded"))
  expect_length(notices, 2L)
})

test_that("omitted countries retain the established route for preparation and mixed selections", {
  source_cli_for_tests()
  root <- mini_repo()
  country_cli_fixture(root)
  env <- environment(dina_cmd_run)
  calls <- character()
  country_cli_bind(env, list(
    dina_country_run_load = function() stop("Preparation selections must keep the existing runner"),
    dina_pushover_status = function(...) list(enabled = FALSE, configured = FALSE),
    dina_cli_run_task = function(task, root, session, dry_run, force) {
      expect_true(dry_run)
      calls <<- c(calls, task$id)
      list(task = task$id, status = "skipped")
    }
  ))
  capture.output(dina_cmd_run(root, c("--from", "01a", "--to", "03a", "--dry-run")))
  expect_identical(calls, c("01a-clean-macro-data", "03a-run-bfm"))
})

test_that("only earlier planned outputs satisfy a missing prerequisite", {
  root <- mini_repo()
  writeLines("fixture", file.path(root, "input_data", "a.txt"))
  producer <- list(id = "producer", script = "code/Stata/01a.do", inputs = "input_data/a.txt", outputs = "output/new")
  consumer <- list(id = "consumer", script = "code/Stata/02a.do", inputs = "output/new/country.csv", outputs = "output/final.csv")
  env <- environment(dina_pipeline_input_preflight)
  country_cli_bind(env, list(dina_legacy_prepared_input_issues = function(...) character()))
  expect_silent(dina_pipeline_input_preflight(list(producer, consumer), root))
  expect_error(dina_pipeline_input_preflight(list(consumer, producer), root), "output/new/country.csv", fixed = TRUE)
})

test_that("country path filtering retains shared donors and selected inputs", {
  paths <- c("input_data/admin_data/MEX/wage_MEX_2010.xlsx", "input_data/admin_data/ARG/gpinter_ARG_2010.xlsx", "input_data/admin_data", "output/shared.xlsx")
  expect_equal(dina_country_input_paths(paths, "ARG", c("ARG", "MEX")), paths[-1])
  expect_equal(dina_country_input_paths(paths, NULL, c("ARG", "MEX")), paths)
})

test_that("worker launch receives scope only in the child environment", {
  root <- mini_repo()
  writeLines(c("args <- commandArgs(TRUE)",
    "for (i in seq_len(100)) { cat(paste(rep('verbose stdout', 1000), collapse=' '), '\\n'); cat(paste(rep('verbose stderr', 1000), collapse=' '), '\\n', file=stderr()) }",
    "writeLines(c(Sys.getenv('DINA_RUN_COUNTRIES'),args), 'output/worker-scope.txt')"), file.path(root, "code", "R", "worker.R"))
  env <- environment(dina_run_task)
  country_cli_bind(env, list(
    dina_assert_current_config_validation = function(...) list(identity = list(scope = "fixture"), receipt = list(checked_at = "fixture")),
    dina_source_review_advisory = function(...) list(),
    dina_task_status = function(...) stop("Scoped worker must not use whole-task freshness"),
    dina_save_session = function(...) stop("A country worker must not record full-task success")
  ))
  before <- Sys.getenv("DINA_RUN_COUNTRIES", unset = NA_character_)
  task <- list(id = "06a-fixture", type = "r", script = "code/R/worker.R", inputs = list(), outputs = "output/worker-scope.txt")
  result <- dina_run_task(task, root = root, session = list(id = "fixture", task_runs = list()), dry_run = FALSE,
    env_extra = c(DINA_RUN_COUNTRIES = "ARG"), script_args = c("--countries", "ARG"),
    skip_current = TRUE, record_session = FALSE)
  expect_equal(result$status, "succeeded")
  expect_equal(readLines(file.path(root, "output", "worker-scope.txt")), c("ARG", "--countries", "ARG"))
  expect_identical(Sys.getenv("DINA_RUN_COUNTRIES", unset = NA_character_), before)
  expect_gt(file.info(file.path(root, result$log_dir, "06a-fixture.out.log"))$size, 1000000)
  expect_gt(file.info(file.path(root, result$log_dir, "06a-fixture.err.log"))$size, 1000000)
})

test_that("export metadata stages artifacts while reading the canonical baseline", {
  source_cli_for_tests()
  root <- mini_repo()
  stage <- file.path(root, "stage")
  config <- dina_session_config(NULL, root, expand_env = FALSE)
  baseline <- file.path(root, config$export_validation$previous_update_file)
  writeLines("approved baseline fixture", baseline)
  env <- environment(dina_results_begin)
  country_cli_bind(env, list(dina_baseline_check = function(path, root) {
    expect_true(file.exists(file.path(root, path)))
    character()
  }))
  date <- "19Oct2026"
  published <- dina_results_metadata_path(root, date)
  dir.create(dirname(published), recursive = TRUE, showWarnings = FALSE)
  writeLines("previous complete receipt", published)
  dina_results_begin(root, date, config, output_root = stage)
  expect_equal(readLines(published), "previous complete receipt")
  expect_true(file.exists(dina_results_metadata_path(stage, date, TRUE)))
  expect_error(dina_results_complete(root, date, config, output_root = stage), "incomplete")
  graphs <- file.path(stage, "output", "figures", "updates", paste0("update-", date, "-sptinc992j-", names(dina_results_graph_names()), ".pdf"))
  series <- file.path(stage, "output", "latest_wid_series", paste0("dina_latam", c("_wide", "", ""), "_", date, c("", "", "_amory"), ".dta"))
  for (path in c(graphs, series)) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeLines("new artifact", path)
  }
  metadata <- dina_results_complete(root, date, config, output_root = stage)
  expect_equal(metadata$status, "complete")
  expect_equal(readLines(published), "previous complete receipt")
  expect_true(file.exists(dina_results_metadata_path(stage, date)))
  expect_false(file.exists(dina_results_metadata_path(stage, date, TRUE)))
  dina_results_begin(root, date, config, output_root = stage)
  writeLines("changed baseline", baseline)
  expect_error(dina_results_complete(root, date, config, output_root = stage), "baseline or settings changed")
})

test_that("comparison metadata detects integration changes but ignores a new preview", {
  source_cli_for_tests()
  root <- mini_repo()
  config <- dina_session_config(NULL, root, expand_env = FALSE)
  writeLines("baseline", file.path(root, config$export_validation$previous_update_file))
  country_cli_bind(environment(dina_results_begin), list(dina_baseline_check = function(...) character()))
  date <- "21Oct2026"
  graphs <- file.path(root, "output", "figures", "updates",
    paste0("update-", date, "-sptinc992j-", names(dina_results_graph_names()), ".pdf"))
  series <- file.path(root, "output", "latest_wid_series",
    paste0("dina_latam", c("_wide", "", ""), "_", date, c("", "", "_amory"), ".dta"))
  for (path in c(graphs, series)) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeLines("fixture artifact", path)
  }
  dina_results_begin(root, date, config)
  state <- list(active = list(`generic-source` = list(candidate = "validated-candidate", source_id = "generic-source")))
  pointer <- dina_finished_publication_pointer(root)
  dir.create(dirname(pointer), recursive = TRUE, showWarnings = FALSE)
  dina_write_json(state, pointer)
  expect_error(dina_results_complete(root, date, config), "integration changed during export")
  expect_false(file.exists(dina_results_metadata_path(root, date)))

  dina_results_begin(root, date, config)
  metadata <- dina_results_complete(root, date, config)
  expect_identical(metadata$finished_publications, state$active)
  expect_true(all(dina_results_inventory(root)$graphs$status == "Matches recorded baseline and settings"))
  state$latest_candidate <- "new-preview-candidate"
  dina_write_json(state, pointer)
  expect_true(all(dina_results_inventory(root)$graphs$status == "Matches recorded baseline and settings"))
  state$active[[1L]]$candidate <- "new-validated-candidate"
  dina_write_json(state, pointer)
  expect_true(all(dina_results_inventory(root)$graphs$status == "Finished-series integration changed; regenerate export"))

  # Existing comparison receipts predate the new field and remain valid when
  # there is no activated replacement.
  metadata$finished_publications <- NULL
  dina_results_publish_metadata(metadata, dina_results_metadata_path(root, date))
  dina_write_json(list(active = list()), pointer)
  expect_true(all(dina_results_inventory(root)$graphs$status == "Matches recorded baseline and settings"))
})

test_that("a fresh standalone country loader exposes publication validation without the main CLI", {
  root <- mini_repo()
  script <- tempfile("standalone-publication-loader-", fileext = ".R")
  withr::defer(unlink(script))
  writeLines(c(
    "args <- commandArgs(TRUE)",
    "setwd(args[[1L]])",
    "source('code/R/country_run.R')",
    "stopifnot(!exists('dina_finished_publication_state', mode = 'function'))",
    "country_run_load_cli(args[[1L]])",
    "required <- c('dina_finished_publication_state', 'dina_finished_publication_active', 'dina_finished_publication_verify', 'dina_finished_include_path', 'dina_review_hashes', 'dina_source_registry')",
    "stopifnot(all(vapply(required, exists, logical(1), mode = 'function')))",
    "stopifnot(length(dina_finished_publication_state(args[[2L]])$active) == 0L)",
    "stopifnot(nrow(dina_finished_publication_active(args[[2L]])$data) == 0L)",
    "country_run_load_cli(args[[1L]])",
    "cat('standalone publication loader passed\\n')"
  ), script)
  output <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
    c("--vanilla", shQuote(script), shQuote(repo_root_for_tests), shQuote(root)), stdout = TRUE, stderr = TRUE))
  expect_equal(attr(output, "status") %||% 0L, 0L, info = paste(output, collapse = "\n"))
  expect_true(any(grepl("standalone publication loader passed", output, fixed = TRUE)))
})

test_that("export previews report automatic refresh and still block changed source approvals", {
  source_cli_for_tests()
  dina_country_run_load()
  root <- mini_repo()
  country_cli_fixture(root)
  task <- dina_task_map(root)[["07d-export-results-to-wid"]]
  active <- list()
  invalid_source <- FALSE
  country_cli_bind(environment(country_run_preview), list(
    dina_finished_publication_readiness = function(root, config, internal_pending = FALSE) {
      if (invalid_source) stop("Accepted source changed; explicit review required.")
      list(status = if (length(active)) "refresh_pending" else "inactive",
        message = if (!length(active)) "no active replacement" else if (internal_pending)
          "automatic continuation refresh after the planned 07c" else "automatic continuation refresh pending")
    },
    dina_task_missing_inputs = function(...) character(),
    country_run_case_prerequisites = function(...) list(issues = character(), messages = character())
  ))
  absent <- country_run_preview(task, root, NULL, "ARG")
  expect_identical(absent$status, "planned")
  expect_true(any(grepl("no active replacement", absent$messages, fixed = TRUE)))
  active <- list(`generic-source` = list(candidate = "approved-candidate"))
  pending <- country_run_preview(task, root, NULL, "ARG")
  expect_identical(pending$status, "planned")
  expect_true(any(grepl("refresh pending", pending$messages, fixed = TRUE)))
  deferred <- country_run_preview(task, root, NULL, "ARG", planned_tasks = "07c-interpolate-ineqstats")
  expect_identical(deferred$status, "planned")
  expect_true(any(grepl("after the planned 07c", deferred$messages, fixed = TRUE)))
  expect_equal(deferred$compute, "COL,ARG ")
  invalid_source <- TRUE
  blocked <- country_run_preview(task, root, NULL, "ARG", planned_tasks = "07c-interpolate-ineqstats")
  expect_identical(blocked$status, "blocked")
  expect_match(blocked$issues, "Accepted source changed")
})
