#!/usr/bin/env Rscript
source("code/R/functions/read_dina_config.R")
source("code/R/admin_cleaners/gpinter_admin.R")
dina_admin_gpinter_run(config = read_dina_config())
