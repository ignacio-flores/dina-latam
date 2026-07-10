test_that("source scan detects years from filenames quickly", {
  root <- mini_repo()
  touch(file.path(root, "input_data", "source_2020_2022.xlsx"), "2024-01-01")
  scan <- dina_scan_sources(root)
  expect_equal(scan[["source-a"]]$detected_years, c(2020L, 2022L))
  expect_equal(scan[["source-missing"]]$detected_years, integer())
})

test_that("source scan summarizes broad country coverage for display", {
  root <- mini_repo()
  dina_write_yaml(list(sources = list(
    list(
      id = "multi-source",
      family = "fixture",
      country = "MULTI",
      method = "manual",
      canonical = c("input_data/multi_*.csv"),
      notes = "Fixture broad-country source."
    )
  )), file.path(root, "config", "sources.yml"))

  scan <- dina_scan_sources(root)
  expect_equal(scan[["multi-source"]]$country, "2 countries")
  expect_equal(scan[["multi-source"]]$country_coverage, c("COL", "ARG"))
})

test_that("source diff classifies new years and missing files", {
  root <- mini_repo()
  touch(file.path(root, "input_data", "source_2024.xlsx"), "2024-01-01")
  previous <- dina_scan_sources(root, hash = "all")
  file.remove(file.path(root, "input_data", "source_2024.xlsx"))
  current <- dina_scan_sources(root, hash = "changed", previous = previous)
  diff <- dina_classify_source_changes(current, previous)
  expect_true("missing" %in% diff[["source-a"]]$classes)
  expect_true(diff[["source-a"]]$counts[["missing"]] > 0L)
  expect_true("unchanged" %in% diff[["source-missing"]]$classes)
})

test_that("source scan reuses hashes when metadata is unchanged", {
  root <- mini_repo()
  path <- file.path(root, "input_data", "source_2024.xlsx")
  touch(path, "2024-01-01")
  previous <- dina_scan_sources(root, hash = "all")
  current <- dina_scan_sources(root, hash = "changed", previous = previous)
  file <- current[["source-a"]]$files[[1]]
  expect_equal(file$hash_status, "reused")
  expect_equal(file$sha256, previous[["source-a"]]$files[[1]]$sha256)
})

test_that("source diff distinguishes content changes from timestamp-only changes", {
  root <- mini_repo()
  path <- file.path(root, "input_data", "source_2024.xlsx")
  touch(path, "2024-01-01")
  previous <- dina_scan_sources(root, hash = "all")

  Sys.setFileTime(path, as.POSIXct("2024-01-02", tz = "UTC"))
  current <- dina_scan_sources(root, hash = "changed", previous = previous)
  diff <- dina_classify_source_changes(current, previous)
  expect_true("timestamp_only" %in% diff[["source-a"]]$classes)

  writeLines("changed", path)
  Sys.setFileTime(path, as.POSIXct("2024-01-03", tz = "UTC"))
  current <- dina_scan_sources(root, hash = "changed", previous = previous)
  diff <- dina_classify_source_changes(current, previous)
  expect_true("content_changed" %in% diff[["source-a"]]$classes)
})

test_that("project source registry is a complete readable catalog", {
  registry_text <- paste(readLines(file.path(repo_root_for_tests, "config", "sources.yml"), warn = FALSE), collapse = "\n")
  explorer_text <- paste(readLines(file.path(repo_root_for_tests, "config", "country_sna_explorer.yml"), warn = FALSE), collapse = "\n")
  retired_inboxes <- c(
    "input_data/_new/admin_tax",
    "input_data/_new/admin_aux",
    "input_data/_new/country_sna",
    "input_data/_new/survey_inputs",
    "input_data/_new/macro",
    "input_data/_new/spending",
    "input_data/_new/tax_rates",
    "input_data/_new/validation",
    "input_data/_new/prices",
    "input_data/_new/population",
    "input_data/_new/balance_sheet",
    "input_data/_new/tax_composition",
    "input_data/_new/social_security",
    "input_data/_new/ceq",
    "input_data/_new/public_spending"
  )
  expect_false(any(vapply(retired_inboxes, grepl, logical(1), x = registry_text, fixed = TRUE)))
  expect_false(any(vapply(retired_inboxes, grepl, logical(1), x = explorer_text, fixed = TRUE)))

  registry_inboxes <- regmatches(registry_text, gregexpr("input_data/_new/[^[:space:]\"']+", registry_text, perl = TRUE))[[1]]
  registry_inboxes <- sub("[,)]$", "", registry_inboxes)
  registry_buckets <- unique(sub("^input_data/_new/([^/]+).*$", "\\1", registry_inboxes))
  expect_true(all(registry_buckets %in% c("admin", "sna", "surveys", "other")))
  expect_true(all(c("admin", "sna", "surveys", "other") %in% registry_buckets))

  registry <- dina_sources(repo_root_for_tests)$sources
  ids <- vapply(registry, function(source) source$id, character(1))

  required_fields <- c("id", "family", "country", "method")
  missing_fields <- character()
  missing_locators <- character()
  for (source in registry) {
    for (field in required_fields) {
      if (length(dina_source_values(dina_source_field(source, field))) == 0) {
        missing_fields <- c(missing_fields, paste(source$id %||% "<unknown>", field, sep = ":"))
      }
    }
    has_locator <- any(vapply(
      c("canonical", "url", "urls", "downloader", "notes"),
      function(field) length(dina_source_values(dina_source_field(source, field))) > 0,
      logical(1)
    ))
    if (!has_locator) {
      missing_locators <- c(missing_locators, source$id)
    }
  }
  expect_equal(missing_fields, character())
  expect_equal(missing_locators, character())

  expect_true(all(c(
    "col-pit",
    "col-admin-wealth",
    "chl-uta",
    "bra-admin-thresholds",
    "bra-pit",
    "wid-prices-xrates",
    "wid-export-scaling",
    "wid-export-sptinc-check",
    "wb-xrates",
    "wb-inflation"
  ) %in% ids))
  expect_false(any(c("bra-admin-tax", "col-admin-income") %in% ids))
  expect_false("country-sna-index" %in% ids)
  methods <- unique(vapply(registry, function(source) source$method, character(1)))
  expect_true(all(methods %in% c("url", "zip", "script", "manual", "wid")))
  expect_false(any(methods %in% c("legacy_script", "static_url", "zip_archive", "manual_index", "stata_wid")))

  top_level_url_methods <- vapply(registry, function(source) {
    if (length(dina_source_values(dina_source_field(source, "url"))) > 0) source$method else NA_character_
  }, character(1))
  expect_true(all(na.omit(top_level_url_methods) %in% c("url", "zip")))

  registry_by_id <- stats::setNames(registry, ids)
  expect_match(paste(dina_source_urls(registry_by_id[["col-pit"]]), collapse = "\n"), "TributosDIAN")
  expect_match(paste(dina_source_urls(registry_by_id[["col-admin-wealth"]]), collapse = "\n"), "TributosDIAN")
  expect_match(paste(dina_source_urls(registry_by_id[["chl-uta"]]), collapse = "\n"), "utm\\{year\\}\\.htm")
  bra_urls <- paste(dina_source_urls(registry_by_id[["bra-pit"]]), collapse = "\n")
  expect_match(bra_urls, "dados-abertos")
  expect_false(grepl("legacy direct file|gn-irpf-ac-\\{year\\}", bra_urls))
  expect_match(paste(dina_source_urls(registry_by_id[["bra-minwage"]]), collapse = "\n"), "salario_minimo")
  expect_match(paste(dina_source_urls(registry_by_id[["country-sna-col"]]), collapse = "\n"), "cuentas-economicas-integradas-2019provisional")
  expect_match(paste(dina_source_urls(registry_by_id[["country-sna-ecu"]]), collapse = "\n"), "CEI2007-2019p")
  expect_match(paste(dina_source_values(dina_source_field(registry_by_id[["surveys-cepal"]], "notes")), collapse = "\n"), "private/server")

  inbox_ids <- vapply(dina_sources_inbox_registry(repo_root_for_tests), function(source) source$id, character(1))
  expect_false("col-admin-wealth" %in% inbox_ids)

  moved_downloaders <- c(
    "code/R/manual-downloaders/download-raw-un-sna.R",
    "code/R/manual-downloaders/download-country-sna.R",
    "code/R/manual-downloaders/bra_admin_downloader.R",
    "code/R/manual-downloaders/bra_minwage_downloader.R"
  )
  expect_true(all(file.exists(file.path(repo_root_for_tests, moved_downloaders))))
  manual_downloaders <- list.files(file.path(repo_root_for_tests, "code/R/manual-downloaders"), pattern = "\\.R$")
  expect_false(any(grepl("^[0-9]{2}[a-z]_", manual_downloaders)))

  script_paths <- unlist(lapply(registry, function(source) {
    c(
      dina_source_values(dina_source_field(source, "downloader")),
      dina_source_values(dina_source_field(source, "transformer"))
    )
  }), use.names = FALSE)
  script_paths <- script_paths[nzchar(script_paths)]
  expect_true(all(file.exists(file.path(repo_root_for_tests, script_paths))))
  expect_false(any(grepl("code/R/functions/bra_.*downloader|code/R/.*/[0-9]{2}[a-z]_download|code/R/01b_import|code/R/02b_clean_admin_chl|code/R/03a_interpolate", script_paths)))
})

test_that("active code does not call WID directly outside the source workflow", {
  files <- list.files(file.path(repo_root_for_tests, "code"), recursive = TRUE, full.names = TRUE)
  files <- files[grepl("\\.(R|do|ado|sthlp)$", files)]
  rel <- sub(paste0("^", normalizePath(repo_root_for_tests), "/"), "", normalizePath(files, mustWork = FALSE))
  legacy <- grepl("(^|/)(old|legacy)(/|$)|\\.sthlp$", rel)
  active <- files[!legacy]
  active_rel <- rel[!legacy]
  has_direct_wid <- vapply(active, function(path) {
    text <- paste(readLines(path, warn = FALSE), collapse = "\n")
    stata_call <- grepl("(^|\n)\\s*(qui|quietly|cap|capture)?\\s*wid\\s*,", text, perl = TRUE)
    r_call <- grepl("download_wid\\s*\\(", text, perl = TRUE)
    (stata_call || r_call) && !grepl("code/R/source-diagnostics/wid_common\\.R$", path)
  }, logical(1))
  expect_equal(active_rel[has_direct_wid], character())
})
