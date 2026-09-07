#!/usr/bin/env Rscript
# Called only by the export step, before reading its baseline and after success.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3L || !args[1] %in% c("begin", "complete")) stop("Expected phase, date, and request file.")
root <- normalizePath(getwd())
Sys.setenv(DINA_CLI_SOURCE_ONLY = "1", DINA_CLI_SOURCE_FILE = file.path(root, "code", "R", "cli", "dina.R"))
source(Sys.getenv("DINA_CLI_SOURCE_FILE"))
request <- readLines(args[3], warn = FALSE)
if (length(request) != 7L) stop("Incomplete export settings request.")
config <- dina_session_config(dina_load_session(root = root), root, expand_env = FALSE)
config$export_validation$previous_update_file <- request[1]
config$export_validation$unit <- request[2]
config$export_validation$steps <- strsplit(trimws(gsub('"', "", request[3], fixed = TRUE)), "[[:space:]]+")[[1]]
config$export_validation$last_year <- as.integer(request[4])
config$countries <- strsplit(trimws(gsub('"', "", request[5], fixed = TRUE)), "[[:space:]]+")[[1]]
config$years <- list(first = as.integer(request[6]), last = as.integer(request[7]))
if (args[1] == "begin") dina_results_begin(root, args[2], config) else dina_results_complete(root, args[2], config)
