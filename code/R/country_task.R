#!/usr/bin/env Rscript
# Bridge used by standalone Stata entry points. Production inputs stay in place.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3L) stop("Expected TASK COUNTRIES COMPLETION_MARKER")
source("code/R/country_run.R")
root <- normalizePath(getwd())
country_run_load_cli(root)
session <- country_run_session(root)
task <- dina_task_map(root)[[args[[1L]]]]
if (is.null(task)) stop("Unknown task")
countries <- if (nzchar(trimws(args[[2L]]))) args[[2L]] else NULL
config <- dina_session_config(session, root, expand_env = FALSE)
context <- country_run_begin(root, list(task), config, countries)
cat("Country run:", context$id, "\n")
cat("Resume: dina run --resume", context$id, "\n")
result <- tryCatch(country_run_execute(task, root, session, countries, run_context = context,
  progress = function(message) { cat(message, "\n"); flush.console() }), error = function(e) {
    country_run_finish(context, "failed")
    stop(e)
  })
ok <- result$status %in% c("succeeded", "skipped", "not_applicable")
country_run_finish(context, if (ok) "succeeded" else "failed")
if (!ok) stop("Task failed; inspect ", result$log_dir)
writeLines(context$id, args[[3L]])
