config_test_baseline <- function(root) {
  path <- file.path(root, "input_data", "_new", "previous_series", "dina_latam_3Oct2024.dta")
  haven::write_dta(data.frame(year = 2022L, iso = "CO", p = "p90p100", widcode = "sptinc992j", value = .5), path)
  path
}

test_that("config loads and renders Stata globals", {
  root <- mini_repo()
  cfg <- dina_config(root)
  expect_equal(cfg$years$first, 2000L)
  expect_equal(cfg$run$units, c("ind", "esn"))

  lines <- dina_render_config_do(cfg)
  expect_true(any(grepl("global all_countries", lines, fixed = TRUE)))
  expect_true(any(grepl("global first_y 2000", lines, fixed = TRUE)))
  expect_true(any(grepl("global bfm_replace \"no\"", lines, fixed = TRUE)))
  expect_true(any(grepl("global export_unit \"esn\"", lines, fixed = TRUE)))
  expect_true(any(grepl("global export_last_y 2024", lines, fixed = TRUE)))
  expect_true(any(grepl("global dina_baseline_fingerprint", lines, fixed = TRUE)))
  expect_true(any(grepl("global dina_config_countries", lines, fixed = TRUE)))
  expect_true(any(grepl('global previous_update "input_data/_new/previous_series/dina_latam_3Oct2024.dta"', lines, fixed = TRUE)))
})

test_that("BFM replacing configurations are rejected before a task runs", {
  root <- mini_repo()
  cfg <- dina_config(root)
  cfg$run$bfm_replace <- TRUE
  expect_error(dina_render_config_do(cfg), "only BFM noreplace is supported")

  dina_write_yaml(cfg, file.path(root, "config", "dina.yml"))
  check <- dina_settings_check(root, session = NULL, validate_baseline = FALSE,
    validate_runtime = FALSE)
  expect_true(any(grepl("only BFM noreplace is supported", check$errors, fixed = TRUE)))
})

test_that("export validation config fails without required values", {
  root <- mini_repo()
  cfg <- dina_config(root)
  cfg$export_validation$previous_update_file <- NULL

  expect_error(
    dina_render_config_do(cfg),
    "Missing required export_validation config value\\(s\\): previous_update_file"
  )
})

test_that("runtime config rejects comparison baselines outside the configured directory", {
  root <- mini_repo()
  cfg <- dina_config(root)
  cfg$export_validation$previous_update_file <- "previous_series/dina_latam_3Oct2024.dta"
  expect_error(dina_render_config_do(cfg), "Comparison baseline must be stored in input_data/_new/previous_series")
})

test_that("runtime config files do not contain stale fallback values", {
  stata_07d <- readLines(file.path(repo_root_for_tests, "code", "Stata", "07d-export-results-to-wid.do"), warn = FALSE)
  bootstrap <- readLines(file.path(repo_root_for_tests, "code", "Stata", "auxiliar", "dina_runtime_config.do"), warn = FALSE)

  expect_false(any(grepl("dina_latam_3Oct2024", stata_07d, fixed = TRUE)))
  expect_false(any(grepl("local ly = 2024", stata_07d, fixed = TRUE)))
  expect_true(any(grepl("Missing required DINA config global", stata_07d, fixed = TRUE)))
  expect_true(any(grepl("DINA_CONFIG_DO", stata_07d, fixed = TRUE)))
  # The working repository may have a manually maintained legacy config.
  root <- mini_repo()
  expect_false(file.exists(file.path(root, "_config.do")))
  expect_true(any(grepl("DINA_CONFIG_DO", bootstrap, fixed = TRUE)))
  expect_false(any(grepl("run _config.do", bootstrap, fixed = TRUE)))
})

test_that("nested config set helper parses scalars and vectors", {
  x <- list(run = list(debug = FALSE))
  y <- dina_set_nested(x, "run.debug", "true")
  expect_true(y$run$debug)
  z <- dina_set_nested(x, "run.units", "ind,esn,pch")
  expect_equal(z$run$units, c("ind", "esn", "pch"))
})

test_that("R config helper honors YAML override", {
  root <- mini_repo()
  source(file.path(repo_root_for_tests, "code", "R", "functions", "read_dina_config.R"))
  override <- file.path(root, "override.yml")
  dina_write_yaml(list(
    countries = c("CHL", "BRA"),
    years = list(last = 2024L)
  ), override)
  withr::with_envvar(c(DINA_CONFIG_YML = file.path(root, "config", "dina.yml"), DINA_CONFIG_OVERRIDE_YML = override), {
    cfg <- read_dina_config()
    expect_equal(as.integer(cfg$last_y), 2024L)
    expect_equal(as.integer(cfg$first_y), 2000L)
    expect_equal(read_dina_countries(cfg), c("CHL", "BRA"))
  })
})

test_that("Stata runs receive a temporary runtime config", {
  root <- mini_repo()
  config_test_baseline(root)
  session <- dina_update_start("2026", root = root)
  session <- dina_session_config_set(session, root = root, key = "years.last", value = "2024")
  fake_stata <- file.path(root, "fake-stata")
  writeLines(c(
    "#!/bin/sh",
    "echo \"$DINA_CONFIG_DO\" > output/runtime-config-path.txt",
    "test -f \"$DINA_CONFIG_DO\" || exit 13",
    "cat \"$DINA_CONFIG_DO\" > output/runtime-config-copy.do",
    "exit 0"
  ), fake_stata)
  Sys.chmod(fake_stata, "0755")
  task <- list(id = "runtime-test", type = "stata", script = "code/Stata/01a.do", inputs = list(), outputs = c("output/runtime-test.txt"))

  session <- withr::with_envvar(c(DINA_STATA_CMD = fake_stata), {
    check <- dina_settings_check(root, session, validate_baseline = TRUE, validate_runtime = FALSE)
    dina_record_config_validation(root, session, check)
  })

  result <- withr::with_envvar(c(DINA_STATA_CMD = fake_stata), {
    dina_run_task(task, root = root, session = session, dry_run = FALSE, force = TRUE)
  })

  expect_equal(result$status, "succeeded")
  runtime_path <- readLines(file.path(root, "output", "runtime-config-path.txt"), warn = FALSE)
  expect_false(file.exists(runtime_path))
  copied <- readLines(file.path(root, "output", "runtime-config-copy.do"), warn = FALSE)
  expect_true(any(grepl("global last_y 2024", copied, fixed = TRUE)))
  expect_true(any(grepl('global dina_config_scope "update:2026-update', copied, fixed = TRUE)))
})

test_that("every real task requires a current validation but previews remain non-mutating", {
  root <- mini_repo(); config_test_baseline(root)
  session <- dina_update_start("2026", root = root)
  fake_stata <- file.path(root, "fake-stata")
  marker <- file.path(root, "output", "stata-ran")
  writeLines(c("#!/bin/sh", paste("touch", shQuote(marker))), fake_stata)
  Sys.chmod(fake_stata, "0755")
  task <- list(id = "runtime-test", type = "stata", script = "code/Stata/01a.do", inputs = list(), outputs = character())

  expect_error(withr::with_envvar(c(DINA_STATA_CMD = fake_stata), {
    dina_run_task(task, root = root, session = session, dry_run = FALSE, force = TRUE)
  }), "dina update config check")
  expect_false(file.exists(marker))

  preview <- withr::with_envvar(c(DINA_STATA_CMD = fake_stata), {
    dina_run_task(task, root = root, session = session, dry_run = TRUE, force = TRUE)
  })
  expect_equal(preview$status, "dry_run")
  expect_false(file.exists(marker))
})

test_that("Stata r() errors fail the task even when its command exits zero", {
  skip_if_not_installed("processx")
  root <- mini_repo(); config_test_baseline(root); session <- dina_update_start("2026", root = root)
  fake_stata <- file.path(root, "fake-stata")
  writeLines(c(
    "#!/bin/sh",
    "for arg in \"$@\"; do wrapper=\"$arg\"; done",
    "log=$(echo \"$wrapper\" | sed 's/\\.wrapper\\.do$/.stata.log/')",
    "printf 'r(198);\\n' > \"$log\"",
    "exit 0"
  ), fake_stata)
  Sys.chmod(fake_stata, "0755")
  task <- list(id = "stata-error", type = "stata", script = "code/Stata/01a.do", inputs = character(), outputs = character())
  session <- withr::with_envvar(c(DINA_STATA_CMD = fake_stata), {
    check <- dina_settings_check(root, session, validate_baseline = TRUE, validate_runtime = FALSE)
    dina_record_config_validation(root, session, check)
  })
  result <- withr::with_envvar(c(DINA_STATA_CMD = fake_stata), {
    dina_run_task(task, root = root, session = session, dry_run = FALSE, force = TRUE)
  })
  expect_equal(result$status, "failed")
  expect_true(file.exists(file.path(root, result$stata_log)))
  expect_equal(dina_load_session(root = root)$task_runs[["stata-error"]]$status, "failed")
})

test_that("R pipeline tasks launch with an absolute Rscript path", {
  skip_if_not_installed("processx")
  root <- mini_repo(); config_test_baseline(root); session <- dina_update_start("2026", root = root)
  script <- file.path(root, "code", "R", "r-task.R")
  writeLines('writeLines("ran", "output/r-task.txt")', script)
  task <- list(id = "r-task", type = "r", script = "code/R/r-task.R",
               inputs = character(), outputs = "output/r-task.txt")
  session <- dina_record_config_validation(root, session,
    dina_settings_check(root, session, validate_baseline = TRUE, validate_runtime = FALSE))
  result <- dina_run_task(task, root = root, session = session, dry_run = FALSE, force = TRUE)
  expect_equal(result$status, "succeeded")
  expect_equal(readLines(file.path(root, "output", "r-task.txt")), "ran")
})

test_that("pipeline input preflight blocks before any task process starts", {
  root <- mini_repo(); session <- dina_update_start("2026", root = root)
  task <- list(id = "missing-input", type = "stata", script = "code/Stata/01a.do",
    inputs = "input_data/required.xlsx", outputs = character())
  expect_error(dina_pipeline_input_preflight(list(task), root = root, session = session),
    "required inputs are missing")
})

test_that("Admin gpinter task requires current cleaned inputs for selected countries", {
  root <- mini_repo()
  task <- list(id = "02d5-interpolate-admin-data", script = "code/Stata/01a.do", inputs = character(), outputs = character())
  missing <- dina_task_missing_inputs(task, root = root)
  expect_true(any(grepl("COL/_clean/total-pos-COL.xlsx", missing, fixed = TRUE)))
  expect_false(any(grepl("BRA/_clean", missing, fixed = TRUE)))

  path <- file.path(root, "input_data", "admin_data", "COL", "_clean", "total-pos-COL.xlsx")
  dir.create(dirname(path), recursive = TRUE)
  writeLines("fixture", path)
  expect_false(any(grepl("COL/_clean", dina_task_missing_inputs(task, root = root), fixed = TRUE)))
})

test_that("Admin gpinter preflight tracks Brazil fallback sources and excludes Ecuador", {
  root <- mini_repo()
  config <- dina_read_yaml(file.path(root, "config", "dina.yml"))
  config$countries <- c("BRA", "ECU")
  dina_write_yaml(config, file.path(root, "config", "dina.yml"))
  dina_write_yaml(list(excluded_countries = "ECU"), file.path(root, "config", "admin_gpinter.yml"))
  interpolate <- list(id = "02d5-interpolate-admin-data", inputs = character())
  format <- list(id = "02e-format-for-bfm", inputs = character())
  interpolation_inputs <- dina_task_effective_inputs(interpolate, root)
  format_inputs <- dina_task_effective_inputs(format, root)
  expect_true(any(grepl("BRA/ptot_2002.xlsx", interpolation_inputs, fixed = TRUE)))
  expect_true(any(grepl("BRA/gpinter_output/total-pre-BRA.xlsx", format_inputs, fixed = TRUE)))
  expect_false(any(grepl("/ECU/", c(interpolation_inputs, format_inputs), fixed = TRUE)))
})

test_that("Pushover templates are not treated as configured credentials", {
  root <- mini_repo()
  dina_notify_init(root)
  status <- dina_pushover_status(root)
  expect_true(status$local_file_present)
  expect_false(status$local_configured)
  expect_false(status$configured)
})

test_that("benchmark and update validation receipts are independent scopes", {
  root <- mini_repo(); config_test_baseline(root)
  benchmark_check <- dina_settings_check(root, session = NULL, validate_runtime = FALSE)
  expect_true(benchmark_check$valid)
  dina_record_config_validation(root, session = NULL, benchmark_check)
  expect_equal(dina_config_validation_state(root, session = NULL)$code, "validated")
  benchmark_runtime <- dina_render_config_do(
    dina_session_config(NULL, root, expand_env = FALSE),
    identity = dina_config_validation_identity(root, NULL, dina_config_validation_receipt(root, NULL))
  )
  expect_true(any(grepl('global dina_config_scope "benchmark"', benchmark_runtime, fixed = TRUE)))
  expect_true(any(grepl("global last_y 2023", benchmark_runtime, fixed = TRUE)))

  session <- dina_update_start("2026", root = root)
  session <- dina_session_config_set(session, root = root, key = "years.last", value = "2024")
  expect_equal(dina_config_validation_state(root, session = NULL)$code, "validated")
  expect_equal(dina_config_validation_state(root, session)$code, "not_validated")

  update_check <- dina_settings_check(root, session, validate_runtime = FALSE)
  session <- dina_record_config_validation(root, session, update_check)
  expect_equal(dina_config_validation_state(root, session)$code, "validated")
  update_runtime <- dina_render_config_do(
    dina_session_config(session, root, expand_env = FALSE),
    identity = dina_config_validation_identity(root, session, dina_config_validation_receipt(root, session))
  )
  expect_true(any(grepl('global dina_config_scope "update:2026-update', update_runtime, fixed = TRUE)))
  expect_true(any(grepl("global last_y 2024", update_runtime, fixed = TRUE)))
  session <- dina_session_config_set(session, root = root, key = "years.last", value = "2025")
  expect_equal(dina_config_validation_state(root, session)$code, "out_of_date")
  expect_equal(dina_config_validation_state(root, session = NULL)$code, "validated")
})
