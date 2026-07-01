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

test_that("runtime config files do not contain stale fallback values", {
  stata_07d <- readLines(file.path(repo_root_for_tests, "code", "Stata", "07d-export-results-to-wid.do"), warn = FALSE)
  loader <- readLines(file.path(repo_root_for_tests, "_config.do"), warn = FALSE)

  expect_false(any(grepl("dina_latam_3Oct2024", stata_07d, fixed = TRUE)))
  expect_false(any(grepl("local ly = 2024", stata_07d, fixed = TRUE)))
  expect_true(any(grepl("Missing required DINA config global", stata_07d, fixed = TRUE)))
  expect_true(any(grepl("DINA_CONFIG_DO", loader, fixed = TRUE)))
  expect_false(any(grepl("global last_y", loader, fixed = TRUE)))
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

  result <- withr::with_envvar(c(DINA_STATA_CMD = fake_stata), {
    dina_run_task(task, root = root, session = session, dry_run = FALSE, force = TRUE)
  })

  expect_equal(result$status, "succeeded")
  runtime_path <- readLines(file.path(root, "output", "runtime-config-path.txt"), warn = FALSE)
  expect_false(file.exists(runtime_path))
  copied <- readLines(file.path(root, "output", "runtime-config-copy.do"), warn = FALSE)
  expect_true(any(grepl("global last_y 2024", copied, fixed = TRUE)))
})
