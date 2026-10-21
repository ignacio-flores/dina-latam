#!/usr/bin/env Rscript
if (any(grepl("^--countries($|=)", commandArgs(trailingOnly = TRUE)))) {
  stop("Country selection is deferred; this script runs the configured country set.", call. = FALSE)
}
source("code/R/functions/read_dina_config.R")
source("code/R/admin_cleaners/gpinter_admin.R")
dina_admin_gpinter_run(config = read_dina_config())
