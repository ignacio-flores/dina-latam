# Invocation-only concept/unit selection: fixtures and mocked workers only.
income_run_fixture <- function() {
  root <- tempfile("dina-income-selection-")
  for (path in c("config", "output", "input_data")) dir.create(file.path(root, path), recursive = TRUE)
  withr::defer(unlink(root, recursive = TRUE), envir = parent.frame())
  config <- dina_read_yaml(file.path(repo_root_for_tests, "config/dina.yml"))
  config$countries <- c("COL", "ARG")
  config$run$units <- c("ind", "esn")
  config$run$debug <- FALSE
  config$run$steps <- c("natinc", "pon", "pod")
  dina_write_yaml(config, file.path(root, "config/dina.yml"))
  ids <- c("03a-run-bfm", "06a-run-ineqstats", "06b-graph-ineqstats", "07a-export-synthetic-microfiles",
    "07b-export-availability", "07c-gpinter", "07d-export-results-to-wid")
  tasks <- lapply(ids, function(id) list(id = id, stage = "fixture", type = "r", script = "code/R/fixture.R",
    deps = list(), inputs = list(), outputs = list(),
    country_scope = if (startsWith(id, "07b") || startsWith(id, "07d")) "shared" else "selected"))
  dina_write_yaml(list(tasks = tasks), file.path(root, "config/pipeline.yml"))
  env <- new.env(parent = globalenv())
  withr::with_dir(repo_root_for_tests, sys.source(file.path(repo_root_for_tests, "code/R/country_run.R"), envir = env))
  env$country_run_load_cli <- function(...) invisible(NULL)
  list(root = root, config = config, tasks = tasks, env = env)
}

income_run_bind <- function(env, values, cleanup = parent.frame()) {
  for (name in names(values)) local({
    key <- name
    present <- exists(key, envir = env, inherits = FALSE)
    old <- if (present) get(key, envir = env, inherits = FALSE) else NULL
    withr::defer(if (present) assign(key, old, envir = env) else rm(list = key, envir = env), envir = cleanup)
    assign(key, values[[key]], envir = env)
  })
}

test_that("income selection validates configured values without changing configuration", {
  f <- income_run_fixture(); e <- f$env
  before <- f$config
  selected <- e$country_run_income_selection(list(steps = "pod,natinc,pod", units = "esn"), f$config)
  expect_identical(selected, list(steps = c("natinc", "pod"), units = "esn"))
  expect_identical(f$config, before)
  expect_null(e$country_run_income_selection(NULL, f$config))
  expect_identical(e$country_run_income_selection(list(steps = "pod"), f$config)$units, c("ind", "esn"))
  expect_error(e$country_run_income_selection(list(steps = "nat"), f$config), "Unknown: nat")
  expect_error(e$country_run_income_selection(list(units = "invalid"), f$config), "Unknown: invalid")
  expect_error(e$country_run_income_selection(list(steps = TRUE), f$config), "Missing value for --steps")
  expect_error(e$country_run_income_selection(list(units = " , "), f$config), "Missing value for --units")
  expect_error(e$country_run_income_tasks(f$tasks, selected), "06a through 07d only")
  expect_silent(e$country_run_income_tasks(f$tasks[-1], selected))
})

test_that("only selected 06a chunks are computed and other assemblies retain full inputs", {
  f <- income_run_fixture(); e <- f$env
  selected <- e$country_run_income_selection(list(steps = "pod", units = "esn"), f$config)
  all <- e$country_run_chunks(f$tasks[[2]], f$config, "ARG")
  limited <- e$country_run_chunks(f$tasks[[2]], f$config, "ARG", selected)
  expect_length(all, 6L)
  expect_length(limited, 1L)
  expect_identical(limited[[1]]$key, "06a-run-ineqstats--ARG--pod--esn")
  expect_identical(e$country_run_chunks(f$tasks[[6]], f$config, "ARG", selected),
    e$country_run_chunks(f$tasks[[6]], f$config, "ARG"))
  export <- e$country_run_chunks(f$tasks[[7]], f$config, "ARG", selected)
  expect_identical(export[[1]]$country, c("COL", "ARG"))
  expected_env <- c(DINA_RUN_STEPS = "pod", DINA_RUN_UNITS = "esn")
  expect_identical(e$country_run_income_env(f$tasks[[3]], selected), expected_env)
  expect_identical(e$country_run_income_env(f$tasks[[4]], selected), expected_env)
  for (i in c(2L, 5L, 6L, 7L)) expect_identical(e$country_run_income_env(f$tasks[[i]], selected), character())
  expect_identical(e$country_run_income_env(f$tasks[[4]], NULL), character())
})

test_that("resuming restores omitted income flags and rejects a changed selection", {
  f <- income_run_fixture(); e <- f$env; tasks <- f$tasks[-1]
  selected <- list(steps = "pod", units = "esn")
  context <- e$country_run_begin(f$root, tasks, f$config, "ARG", selection = selected)
  saved <- e$country_run_read(f$root, context$id)
  expect_identical(unlist(saved$selection$steps), "pod")
  expect_identical(unlist(saved$config$run$steps), c("natinc", "pon", "pod"))
  resumed <- e$country_run_begin(f$root, tasks, f$config, NULL, resume = context$id, dry_run = TRUE)
  expect_identical(resumed$selection, selected)
  expect_identical(e$country_run_begin(f$root, tasks, f$config, NULL, resume = context$id,
    dry_run = TRUE, selection = list(steps = "pod"))$selection, selected)
  expect_error(e$country_run_begin(f$root, tasks, f$config, NULL, resume = context$id,
    selection = list(steps = "pon")), "saved income steps")
  expect_error(e$country_run_begin(f$root, tasks, f$config, NULL, resume = context$id,
    selection = list(units = "ind")), "saved income steps")
  plain <- e$country_run_begin(f$root, tasks, f$config, "ARG")
  expect_null(e$country_run_begin(f$root, tasks, f$config, NULL, resume = plain$id)$selection)
  expect_null(e$country_run_begin(f$root, tasks, f$config, "ARG", dry_run = TRUE)$selection)
  expect_identical(f$config$run$steps, c("natinc", "pon", "pod"))
})

test_that("selected worker environment is invocation-local", {
  f <- income_run_fixture(); e <- f$env
  selected <- list(steps = "pod", units = "esn")
  before <- Sys.getenv(c("DINA_RUN_STEPS", "DINA_RUN_UNITS"), unset = NA_character_)
  captured <- list()
  e$dina_run_task <- function(task, root, session, dry_run, force, progress, env_extra, skip_current, record_session) {
    captured[[task$id]] <<- env_extra
    list(status = "succeeded")
  }
  for (i in c(2L, 3L, 4L, 6L, 7L)) {
    task <- f$tasks[[i]]
    chunk <- e$country_run_chunks(task, f$config, "ARG", selected)[[1]]
    e$country_run_worker(task, f$root, NULL, chunk, "fixture-stage", NULL,
      extra = e$country_run_income_env(task, selected))
  }
  for (i in 2:4) {
    expect_identical(unname(captured[[f$tasks[[i]]$id]]["DINA_RUN_STEPS"]), "pod")
    expect_identical(unname(captured[[f$tasks[[i]]$id]]["DINA_RUN_UNITS"]), "esn")
  }
  for (i in 6:7) expect_false(any(c("DINA_RUN_STEPS", "DINA_RUN_UNITS") %in% names(captured[[f$tasks[[i]]$id]])))
  expect_identical(Sys.getenv(names(before), unset = NA_character_), before)
})

test_that("CLI previews show limited computation and preserve full configured concepts", {
  source_cli_for_tests()
  f <- income_run_fixture(); env <- environment(dina_cmd_run); calls <- list()
  income_run_bind(env, list(
    dina_country_run_load = function() NULL,
    country_run_income_selection = f$env$country_run_income_selection,
    country_run_income_tasks = f$env$country_run_income_tasks,
    country_run_preview = function(task, root, session, countries, force, run_context, planned_tasks) {
      config <- dina_session_config(session, root, expand_env = FALSE)
      calls[[task$id]] <<- list(selection = run_context$selection, config = config)
      chunks <- f$env$country_run_chunks(task, config, countries, run_context$selection)
      list(task = task$id, status = "planned", compute = vapply(chunks, `[[`, "", "key"))
    },
    country_run_begin = function(...) stop("A preview must not start a run"),
    dina_task_status = function(...) stop("Explicit income selection must not skip globally current tasks")
  ))
  output <- capture.output(dina_cmd_run(f$root, c("--from", "06a", "--steps", "pod", "--units", "esn", "--dry-run")))
  expect_length(calls, 6L)
  expect_true(all(vapply(calls, function(call) identical(call$selection, list(steps = "pod", units = "esn")), logical(1))))
  expect_true(all(vapply(calls, function(call) identical(call$config$run$steps, c("natinc", "pon", "pod")), logical(1))))
  expect_true(any(grepl("06a-run-ineqstats--COL--pod--esn", output, fixed = TRUE)))
  expect_false(any(grepl("06a-run-ineqstats--COL--pon", output, fixed = TRUE)))
  expect_true(any(grepl("Other income concepts and units are retained", output, fixed = TRUE)))
  expect_false(dir.exists(file.path(f$root, "output/run_logs")))
  expect_error(dina_cmd_run(f$root, c("03a", "--steps", "pod", "--dry-run")), "06a through 07d only")
  expect_error(dina_cmd_run(f$root, c("06a", "--steps")), "Missing value for --steps")
  expect_error(dina_cmd_run(f$root, c("06a", "--units=")), "Missing value for --units")
  expect_error(dina_cmd_run(f$root, c("06a", "--steps", "typo", "--dry-run")), "Unknown: typo")
  capture.output(dina_cmd_run(f$root, c("stale", "--from", "06a", "--steps=pod", "--units=esn", "--dry-run")))
  capture.output(dina_cmd_tasks(f$root, c("why", "06a", "--steps", "pod", "--units", "esn")))
})

test_that("CLI execution and resume carry the saved selection without changing configuration", {
  source_cli_for_tests()
  f <- income_run_fixture(); env <- environment(dina_cmd_run); runs <- list(); notices <- list()
  original <- readBin(file.path(f$root, "config/dina.yml"), "raw", n = file.info(file.path(f$root, "config/dina.yml"))$size)
  income_run_bind(env, list(
    dina_country_run_load = function() NULL,
    country_run_income_selection = f$env$country_run_income_selection,
    country_run_income_tasks = f$env$country_run_income_tasks,
    country_run_begin = f$env$country_run_begin,
    country_run_read = f$env$country_run_read,
    country_run_finish = f$env$country_run_finish,
    dina_assert_current_config_validation = function(...) invisible(NULL),
    dina_pipeline_input_preflight = function(...) list(admin_trust_snapshot = list(created = FALSE)),
    dina_pushover_status = function(...) list(configured = TRUE, enabled = FALSE),
    dina_notify = function(message, root) { notices[[length(notices) + 1L]] <<- message },
    country_run_preview = function(task, ...) list(task = task$id, status = "planned", issues = character()),
    country_run_execute = function(task, root, session, countries, force, progress, run_context) {
      runs[[length(runs) + 1L]] <<- list(id = run_context$id, task = task$id, selection = run_context$selection,
        configured_steps = unlist(run_context$record$config$run$steps))
      list(task = task$id, status = "succeeded", log_dir = "fixture-logs")
    }
  ))
  capture.output(dina_cmd_run(f$root, c("--from", "06a", "--steps", "pod", "--units", "esn", "--notify")))
  expect_length(runs, 6L)
  id <- runs[[1]]$id
  expected <- list(steps = "pod", units = "esn")
  expect_true(all(vapply(runs, function(run) identical(run$selection, expected), logical(1))))
  expect_true(all(vapply(runs, function(run) identical(unname(run$configured_steps), c("natinc", "pon", "pod")), logical(1))))
  capture.output(dina_cmd_run(f$root, c("--resume", id, "--notify")))
  expect_length(runs, 12L)
  expect_true(all(vapply(runs, function(run) identical(run$selection, expected), logical(1))))
  expect_length(notices, 2L)
  output <- capture.output(dina_cmd_run(f$root, c("--resume", id, "--dry-run")))
  expect_true(any(grepl("Requested income steps: pod", output, fixed = TRUE)))
  expect_error(dina_cmd_run(f$root, c("--resume", id, "--steps", "pon", "--dry-run")), "saved income steps")
  expect_length(runs, 12L)
  expect_identical(readBin(file.path(f$root, "config/dina.yml"), "raw", n = length(original)), original)
})
