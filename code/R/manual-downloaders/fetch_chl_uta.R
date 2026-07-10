#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

arg_value <- function(flag, default = "") {
  hit <- which(args == flag)
  if (!length(hit) || hit[[1L]] >= length(args)) default else args[[hit[[1L]] + 1L]]
}

target <- arg_value("--target")
repo_root <- arg_value("--repo-root", getwd())
if (!nzchar(target)) {
  stop("Missing --target.", call. = FALSE)
}

helper <- file.path(repo_root, "code", "R", "source-helpers", "chl_uta.R")
if (!file.exists(helper)) {
  stop("Missing Chile UTA helper: ", helper, call. = FALSE)
}
source(helper, local = TRUE)

config_years <- function(root) {
  first <- suppressWarnings(as.integer(Sys.getenv("DINA_FETCH_CHL_UTA_FIRST_YEAR", unset = NA_character_)))
  last <- suppressWarnings(as.integer(Sys.getenv("DINA_FETCH_CHL_UTA_LAST_YEAR", unset = NA_character_)))
  if (!is.na(first) && !is.na(last)) return(seq.int(first, last))
  config_path <- file.path(root, "config", "dina.yml")
  if (requireNamespace("yaml", quietly = TRUE) && file.exists(config_path)) {
    config <- yaml::read_yaml(config_path)
    active <- file.path(root, "output", "updates", ".active_update")
    if (file.exists(active)) {
      update_id <- trimws(readLines(active, warn = FALSE, n = 1L))
      override_path <- file.path(root, "output", "updates", update_id, "config.override.yml")
      if (nzchar(update_id) && file.exists(override_path)) {
        merge_config <- function(base, override) {
          if (!is.list(base) || !is.list(override)) return(override)
          out <- base
          for (name in names(override)) {
            if (!is.null(out[[name]]) && is.list(out[[name]]) && is.list(override[[name]])) {
              out[[name]] <- merge_config(out[[name]], override[[name]])
            } else {
              out[[name]] <- override[[name]]
            }
          }
          out
        }
        config <- merge_config(config, yaml::read_yaml(override_path))
      }
    }
    first <- suppressWarnings(as.integer(config$years$first %||% 2005L))
    last <- suppressWarnings(as.integer(config$years$last %||% as.integer(format(Sys.Date(), "%Y"))))
  }
  if (is.na(first)) first <- 2005L
  if (is.na(last)) last <- as.integer(format(Sys.Date(), "%Y"))
  seq.int(max(2005L, first), last)
}

years <- config_years(repo_root)
out <- chl_uta_scrape(years)
dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(out, target, row.names = FALSE, na = "")
cat(sprintf("wrote %s Chile UTA rows to %s\n", nrow(out), target))
