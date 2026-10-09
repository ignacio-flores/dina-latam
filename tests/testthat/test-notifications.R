test_that("failed Stata tasks report the error line and script", {
  root <- mini_repo()
  log_dir <- file.path(root, "output", "run_logs", "example")
  dir.create(log_dir, recursive = TRUE)
  writeLines(c(". import delimited example.csv", "variable country not found", "r(111);",
               "end of do-file", "r(111);"), file.path(log_dir, "02e.stata.log"))
  result <- list(task = "02e", log_dir = "output/run_logs/example",
                 stata_log = "output/run_logs/example/02e.stata.log", exit_status = 111L)
  detail <- dina_task_failure_detail(result, root)
  expect_equal(detail, "variable country not found r(111)")
  message <- dina_run_notification_message(FALSE, list(), list(
    stage = "task", task = "02e", script = "code/Stata/02e-format-for-bfm.do",
    detail = detail, log_dir = result$log_dir
  ))
  expect_match(message, "02e")
  expect_match(message, "code/Stata/02e-format-for-bfm.do", fixed = TRUE)
  expect_match(message, "variable country not found r(111)", fixed = TRUE)
  expect_match(message, "output/run_logs/example", fixed = TRUE)
})

test_that("R errors and preflight stops produce useful short notifications", {
  root <- mini_repo()
  log_dir <- file.path(root, "output", "run_logs", "r-example")
  dir.create(log_dir, recursive = TRUE)
  writeLines(c("Error: Required R package is missing: gpinter", "Execution halted"),
             file.path(log_dir, "02d5.err.log"))
  expect_equal(dina_task_failure_detail(list(task = "02d5", log_dir = "output/run_logs/r-example",
                                             exit_status = 1L), root),
               "Error: Required R package is missing: gpinter")
  preflight <- dina_run_notification_message(FALSE, list(), list(
    stage = "preflight", detail = paste(
      "Pipeline has not started because required inputs are missing:",
      "  - 02d :: Included Admin artifact changed after inclusion: COL/raw", sep = "\n")
  ))
  expect_match(preflight, "no script started", fixed = TRUE)
  expect_match(preflight, "02d :: Included Admin artifact changed", fixed = TRUE)
  expect_false(grepl("Pipeline has not started", preflight, fixed = TRUE))
  interrupted <- dina_run_notification_message(FALSE, list(), list(
    stage = "interrupted", task = "03a-run-bfm", script = "code/Stata/03a-run-bfm.do",
    detail = "Run interrupted while this script was active."
  ))
  expect_match(interrupted, "DINA interrupted at 03a-run-bfm", fixed = TRUE)
})

test_that("run sends the preflight reason through the notification path", {
  source_cli_for_tests()
  root <- mini_repo()
  cli_env <- environment(dina_cmd_run)
  test_env <- environment()
  captured <- NULL
  replacements <- list(
    dina_select_tasks = function(...) list(list(id = "02d", script = "code/Stata/02d.do")),
    dina_load_session = function(...) NULL,
    dina_pushover_status = function(...) list(enabled = FALSE, configured = TRUE),
    dina_assert_current_config_validation = function(...) invisible(NULL),
    dina_pipeline_input_preflight = function(...) stop("Included Admin artifact changed after inclusion: COL/raw", call. = FALSE),
    dina_notify = function(message, ...) { captured <<- message; invisible(NULL) }
  )
  for (name in names(replacements)) {
    local({
      binding <- name
      previous <- get(binding, envir = cli_env, inherits = TRUE)
      withr::defer(assign(binding, previous, envir = cli_env), envir = test_env)
    })
    assign(name, replacements[[name]], envir = cli_env)
  }
  expect_error(dina_cmd_run(root, "--notify"), "Included Admin artifact changed")
  expect_match(captured, "DINA preflight failed; no script started", fixed = TRUE)
  expect_match(captured, "COL/raw", fixed = TRUE)

  captured <- NULL
  assign("dina_pipeline_input_preflight", function(...) list(admin_trust_snapshot = list(created = FALSE)),
         envir = cli_env)
  previous_runner <- get("dina_cli_run_task", envir = cli_env, inherits = TRUE)
  withr::defer(assign("dina_cli_run_task", previous_runner, envir = cli_env), envir = test_env)
  assign("dina_cli_run_task", function(...) list(
    task = "02d", status = "failed", log_dir = "output/run_logs/example",
    failure = "variable country not found r(111)"
  ), envir = cli_env)
  expect_error(dina_cmd_run(root, "--notify"), "Task 02d failed")
  expect_match(captured, "Script: code/Stata/02d.do", fixed = TRUE)
  expect_match(captured, "variable country not found r(111)", fixed = TRUE)
})
