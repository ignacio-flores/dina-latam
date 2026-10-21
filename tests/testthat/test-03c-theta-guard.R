testthat::test_that("03c retains matching eligible history and rejects stale cases", {
  testthat::skip_if_not_installed("openxlsx")
  testthat::skip_if_not_installed("digest")
  root <- tempfile("dina-03c-fixture-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  env <- new.env(parent = globalenv())
  sys.source(file.path(repo_root_for_tests, "code", "R", "03c_theta_guard.R"), envir = env)

  base <- file.path(root, "output", "bfm_summary")
  dir.create(file.path(base, "bfm_norep_pos"), recursive = TRUE)
  dir.create(file.path(base, "bfm_norep_pre"), recursive = TRUE)
  source_case <- data.frame(p = 0.90, small_t = 1.1, antitonic = 1.2, mpoint = 0.95)
  openxlsx::write.xlsx(list(
    country_years = data.frame(country_year = "AAA_2022"),
    AAA2022 = source_case, BBB2020 = source_case,
    CCC2021 = source_case, DDD2019 = source_case
  ), file.path(base, "bfm_norep_pos", "merging_points.xlsx"))
  openxlsx::write.xlsx(list(dummy = data.frame(x = 1)),
                       file.path(base, "bfm_norep_pre", "merging_points.xlsx"))
  old <- data.frame(country = c("AAA", "BBB", "CCC", "DDD"),
                    year = c(2022L, 2020L, 2021L, 2019L),
                    p = 90, small_t = 1.1, antitonic = 1.2, mpoint = 95)
  openxlsx::write.xlsx(list(panel = old), file.path(base, "all_thetas.xlsx"))

  admin <- file.path(root, "input_data", "admin_data")
  for (country in c("AAA", "BBB", "CCC", "DDD")) {
    dir.create(file.path(admin, country), recursive = TRUE)
    year <- old$year[old$country == country]
    raw <- file.path(root, "intermediary_data", "microdata", "raw", country,
                     sprintf("%s_%s_raw.dta", country, year))
    dir.create(dirname(raw), recursive = TRUE)
    file.create(raw)
    if (country != "CCC") {
      file.create(file.path(admin, country, sprintf("gpinter_%s_%s.xlsx", country, year)))
    }
  }
  trust <- file.path(root, "trust.csv")
  utils::write.csv(data.frame(country = c("AAA", "BBB", "CCC"),
                              year = c(2022L, 2020L, 2021L), trust = 0.9,
                              trust_wages = NA_real_), trust, row.names = FALSE)
  stage <- file.path(root, "dina03c-stage")
  env$theta03c_prepare(root, stage, trust, 2019L, 2022L)
  selected <- utils::read.csv(file.path(stage, "cases.csv"), stringsAsFactors = FALSE)
  testthat::expect_setequal(paste(selected$country, selected$year),
                            c("AAA 2022", "BBB 2020"))
  testthat::expect_identical(selected$provenance[selected$country == "BBB"],
                             "historical_panel")
  published <- file.path(base, "all_thetas.xlsx")
  before <- env$theta03c_sha(published)
  testthat::expect_error(env$theta03c_publish(root, stage), "Missing theta panel")
  testthat::expect_identical(env$theta03c_sha(published), before)

  workbook <- file.path(base, "bfm_norep_pos", "merging_points.xlsx")
  changed <- openxlsx::loadWorkbook(workbook)
  openxlsx::writeData(changed, "BBB2020", data.frame(p = 0.90, small_t = 9,
                       antitonic = 1.2, mpoint = 0.95), startRow = 1L,
                       colNames = TRUE)
  openxlsx::saveWorkbook(changed, workbook, overwrite = TRUE)
  testthat::expect_error(env$theta03c_publish(root, stage),
                         "inputs changed while the staged result was being built")
  testthat::expect_identical(env$theta03c_sha(published), before)
  testthat::expect_error(env$theta03c_prepare(root, file.path(root, "dina03c-changed"),
                                              trust, 2019L, 2022L),
                         "no longer matches the old panel")
})
