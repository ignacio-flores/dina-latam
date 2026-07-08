#!/usr/bin/env Rscript

# Standalone experimental entrypoint for the isolated admin PIT workflow.
# This avoids wiring into the mainstream `dina` CLI until that boundary is
# explicitly accepted.

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}

admin_pit_cli_root <- function() {
  env_root <- Sys.getenv("DINA_REPO_ROOT", unset = "")
  if (nzchar(env_root)) return(normalizePath(env_root, mustWork = FALSE))
  script <- normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]), mustWork = FALSE)
  if (is.na(script) || !nzchar(script)) return(normalizePath(getwd(), mustWork = FALSE))
  normalizePath(file.path(dirname(script), "..", "..", ".."), mustWork = FALSE)
}

admin_pit_cli_flag <- function(args, flag, default = NULL) {
  hit <- which(args == flag)
  if (!length(hit) || hit[[1L]] >= length(args)) return(default)
  args[[hit[[1L]] + 1L]]
}

admin_pit_cli_has <- function(args, flag) {
  any(args == flag)
}

admin_pit_cli_read_csv <- function(path) {
  if (!file.exists(path)) return(data.frame(stringsAsFactors = FALSE))
  utils::read.csv(path, stringsAsFactors = FALSE)
}

admin_pit_cli_table_path <- function(root, table, run = NULL) {
  candidates <- c()
  if (!is.null(run) && nzchar(run)) {
    run_path <- if (grepl("^/", run)) run else file.path(root, run)
    candidates <- c(candidates, file.path(run_path, "tables", paste0(table, ".csv")))
  }
  candidates <- c(
    candidates,
    file.path(root, "output", "experiments", "admin_pit_explore", "tables", paste0(table, ".csv")),
    file.path(root, "output", "experiments", "admin_pit_include", "tables", paste0(table, ".csv"))
  )
  hit <- candidates[file.exists(candidates)]
  if (!length(hit)) stop("Admin PIT table not found: ", table, call. = FALSE)
  hit[[1L]]
}

admin_pit_cli_print_table <- function(rows, limit = 20L) {
  if (!nrow(rows)) {
    cat("(empty)\n")
    return(invisible(rows))
  }
  rows <- utils::head(rows, limit)
  print(rows, row.names = FALSE)
  invisible(rows)
}

admin_pit_cli_main <- function(args = commandArgs(trailingOnly = TRUE), root = admin_pit_cli_root()) {
  cmd <- args[[1L]] %||% "help"
  rest <- if (length(args) > 1L) args[-1L] else character()
  if (cmd %in% c("help", "-h", "--help")) {
    cat("Usage:\n")
    cat("  Rscript code/R/source-diagnostics/admin_pit_cli.R explore [--country ISO] [--output-dir PATH]\n")
    cat("  Rscript code/R/source-diagnostics/admin_pit_cli.R include --dry-run [--exploration-run PATH] [--output-dir PATH]\n")
    cat("  Rscript code/R/source-diagnostics/admin_pit_cli.R include --confirm --include-run RUN\n")
    cat("  Rscript code/R/source-diagnostics/admin_pit_cli.R include --restore CONFIRM_RUN\n")
    cat("  Rscript code/R/source-diagnostics/admin_pit_cli.R table TABLE [--run PATH] [--limit N]\n")
    return(invisible(NULL))
  }
  if (identical(cmd, "explore")) {
    source(file.path(root, "code", "R", "source-diagnostics", "admin_pit_explorer.R"), local = TRUE)
    result <- run_admin_pit_explorer(
      root = root,
      output_dir = admin_pit_cli_flag(rest, "--output-dir", NULL),
      countries = admin_pit_cli_flag(rest, "--country", NULL),
      write_outputs = TRUE
    )
    status <- result$manifest$value[result$manifest$key == "status"][[1L]]
    cat("PIT Admin Explore\n")
    cat("Status:", status, "\n")
    cat("Output:", result$paths$root, "\n")
    admin_pit_cli_print_table(result$outputs$extension_summary)
    return(invisible(result))
  }
  if (identical(cmd, "include")) {
    source(file.path(root, "code", "R", "source-diagnostics", "admin_pit_include.R"), local = TRUE)
    if (admin_pit_cli_has(rest, "--confirm")) {
      result <- admin_pit_include_confirm_sources(root = root, include_run = admin_pit_cli_flag(rest, "--include-run", NULL))
      cat("PIT Admin Include Confirm\n")
      cat("Output:", result$paths$root, "\n")
      admin_pit_cli_print_table(result$outputs$promote_report)
      return(invisible(result))
    }
    restore <- admin_pit_cli_flag(rest, "--restore", NULL)
    if (!is.null(restore)) {
      result <- admin_pit_include_restore_sources(root = root, confirm_run = restore)
      cat("PIT Admin Include Restore\n")
      cat("Output:", result$paths$root, "\n")
      admin_pit_cli_print_table(result$outputs$restore_report)
      return(invisible(result))
    }
    result <- run_admin_pit_include(
      root = root,
      exploration_run = admin_pit_cli_flag(rest, "--exploration-run", NULL),
      output_dir = admin_pit_cli_flag(rest, "--output-dir", NULL),
      write_outputs = TRUE
    )
    status <- result$manifest$value[result$manifest$key == "status"][[1L]]
    cat("PIT Admin Include\n")
    cat("Status:", status, "\n")
    cat("Output:", result$paths$root, "\n")
    admin_pit_cli_print_table(result$outputs$include_summary)
    return(invisible(result))
  }
  if (identical(cmd, "table")) {
    table <- rest[[1L]] %||% ""
    if (!nzchar(table)) stop("Missing table name.", call. = FALSE)
    rows <- admin_pit_cli_read_csv(admin_pit_cli_table_path(root, table, admin_pit_cli_flag(rest, "--run", NULL)))
    limit <- as.integer(admin_pit_cli_flag(rest, "--limit", "20"))
    cat("PIT Admin Table:", table, "\n")
    admin_pit_cli_print_table(rows, limit = limit)
    return(invisible(rows))
  }
  stop("Unknown admin PIT command: ", cmd, call. = FALSE)
}

if (!identical(Sys.getenv("ADMIN_PIT_CLI_SOURCE_ONLY", unset = ""), "1")) {
  tryCatch(admin_pit_cli_main(), error = function(e) {
    cat("ERROR:", conditionMessage(e), "\n", file = stderr())
    quit(status = 1)
  })
}
