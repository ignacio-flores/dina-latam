test_that("07c Stata bridge preserves selection and propagates R failure", {
  skip_if_not_installed("gpinter")
  skip_if_not_installed("processx")
  stata <- Sys.getenv("DINA_STATA_CMD", "/Applications/Stata/StataMP.app/Contents/MacOS/stata-mp")
  skip_if(!file.exists(stata), "Stata unavailable")
  root <- tempfile("dina-07c-bridge-")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  for (path in c("code/R", "code/Stata/auxiliar", "output/synthetic_microfiles", "output/ineqstats")) {
    dir.create(file.path(root, path), recursive = TRUE)
  }
  files <- c("code/Stata/07c-interpolate_ineqstats_results.do", "code/R/07c_run_interpolation.R",
    "code/R/07c_interpolate_ineqstats.R", "code/R/gpinter_precision.R", "code/R/gpinter_thresholds.R",
    "code/R/gpinter_tail_repair.R", "code/R/gpinter_atoms.R")
  stopifnot(all(file.copy(file.path(repo_root_for_tests, files), file.path(root, files))))
  writeLines('country_run_r_entry <- function(task,args) stopifnot(Sys.getenv("DINA_COUNTRY_WORKER") == task)',
    file.path(root, "code/R/country_run.R"))
  writeLines('* Runtime configuration is independent of the bridge under test.',
    file.path(root, "code/Stata/auxiliar/dina_runtime_config.do"))
  p <- c(seq(0, .99, .01), seq(.991, .999, .001), seq(.9991, .9999, .0001), seq(.99991, .99999, .00001))
  surv <- 1 - p
  integral <- surv * (1 - log(surv))
  input <- data.frame(country = "PER", year = 2001L, step = "nat", unit = "esn", p = p,
    average = 1, thr = -log(surv), avg = (integral - c(integral[-1L], 0)) / (c(p[-1L], 1) - p),
    topavg = 1 - log(surv), topsh = integral)
  other <- input; other$country <- "ARG"; other$year <- 2003L
  source_path <- file.path(root, "output/synthetic_microfiles/_smicrofile_long_detailed.csv")
  readr::write_csv(rbind(input, other), source_path)
  published <- file.path(root, "output/ineqstats/_gpinter_all_topcorrected.csv")
  writeLines("previous publication", published)
  writeLines(c('capture noisily do "code/Stata/07c-interpolate_ineqstats_results.do"',
    'local result = _rc', 'file open result using "worker-rc.txt", write text replace',
    'file write result "`result\'"', 'file close result'), file.path(root, "worker.do"))
  run <- function(name) {
    stage <- file.path(root, name)
    for (path in c("output/ineqstats", "output/data_reports")) dir.create(file.path(stage, path), recursive = TRUE)
    unlink(file.path(root, "worker-rc.txt"))
    processx::run(stata, c("-b", "do", "worker.do"), wd = root, timeout = 90000, error_on_status = FALSE,
      env = c(DINA_COUNTRY_WORKER = "07c-interpolate-ineqstats", DINA_RUN_COUNTRIES = "PER",
        DINA_OUTPUT_ROOT = stage, DINA_RSCRIPT = file.path(R.home("bin"), "Rscript"),
        LC_ALL = "C", LANG = "C"))
    list(stage = stage, status = as.integer(readLines(file.path(root, "worker-rc.txt"), warn = FALSE)),
      log = paste(readLines(file.path(root, "worker.log"), warn = FALSE), collapse = "\n"))
  }
  success <- run("success")
  expect_equal(success$status, 0L, info = success$log)
  result <- read.csv(file.path(success$stage, "output/ineqstats/_gpinter_all_topcorrected.csv"))
  expect_equal(nrow(result), 127L)
  expect_identical(unique(result$country), "PER")
  expect_true(file.exists(file.path(success$stage, "07c-rscript.log")))
  input$average <- NA_real_
  readr::write_csv(rbind(input, other), source_path)
  failure <- run("failure")
  expect_equal(failure$status, 459L)
  expect_match(failure$log, "07c R interpolation failed")
  expect_match(failure$log, "Error")
  expect_false(file.exists(file.path(failure$stage, "output/ineqstats/_gpinter_all_topcorrected.csv")))
  expect_identical(readLines(published), "previous publication")
})
