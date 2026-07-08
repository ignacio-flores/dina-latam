Sys.unsetenv("LC_ALL")

dina_cli_refinement_state_condition <- get("testthat_state_condition", asNamespace("testthat"))
assignInNamespace("testthat_state_condition", function(before, after, call = NULL) NULL, ns = "testthat")
if (requireNamespace("withr", quietly = TRUE)) {
  withr::defer(
    assignInNamespace("testthat_state_condition", dina_cli_refinement_state_condition, ns = "testthat"),
    envir = testthat:::teardown_env()
  )
}

expect_true <- function(object, info = NULL, label = NULL) {
  value <- force(object)
  if (!isTRUE(value)) testthat::fail(info %||% "Expected TRUE.")
  invisible(value)
}

expect_false <- function(object, info = NULL, label = NULL) {
  value <- force(object)
  if (!identical(value, FALSE)) testthat::fail(info %||% "Expected FALSE.")
  invisible(value)
}

test_that("help describes the new workflow and omits retired command families", {
  main <- run_dina_cli(c("help"))
  expect_equal(main$status, 0L)
  expect_match(main$output, "`sources list \\[SOURCETYPE\\]`")
  expect_match(main$output, "`sources list detail\\|guide`")
  expect_match(main$output, "`sources compare`")
  expect_match(main$output, "`sources explore SOURCETYPE`")
  expect_match(main$output, "`sources include SOURCETYPE`")
  expect_match(main$output, "`sources table SOURCETYPE \\[TABLE\\]`")
  expect_match(main$output, "`update config show\\|edit`")
  expect_match(main$output, "`compress input`")
  expect_match(main$output, "`run list\\|why TASK`")
  expect_match(main$output, "`todo \\[check\\|uncheck\\|reset\\]`")
  expect_match(main$output, "`maintain repo-status\\|repo-diff`")
  expect_match(main$output, "dina run 01a")
  expect_match(main$output, "dina run 01a --dry-run")
  expect_false(grepl("update roadmap", main$output, fixed = TRUE))
  expect_false(grepl("update gate", main$output, fixed = TRUE))
  expect_false(grepl("update mark", main$output, fixed = TRUE))
  expect_false(grepl("sources refresh", main$output, fixed = TRUE))
  expect_false(grepl("`sources show", main$output, fixed = TRUE))
  expect_false(grepl("`sources guide", main$output, fixed = TRUE))
  expect_false(grepl("`sources review`", main$output, fixed = TRUE))
  expect_false(grepl("`sources integrate", main$output, fixed = TRUE))
  expect_false(grepl("`sources diagnose country-sna`", main$output, fixed = TRUE))
  expect_false(grepl("`data check|pack|unpack`", main$output, fixed = TRUE))
  expect_false(grepl("update finalize", main$output, fixed = TRUE))

  workflow <- run_dina_cli(c("help", "workflow"))
  expect_equal(workflow$status, 0L)
  expect_match(workflow$output, "1\\. Start or resume the workspace")
  expect_match(workflow$output, "2\\. Work from sources")
  expect_match(workflow$output, "3\\. Run the pipeline")
  expect_match(workflow$output, "4\\. Keep a loose todo list")
  expect_match(workflow$output, "5\\. Close with a report")
  expect_match(workflow$output, "dina sources compare")
  expect_match(workflow$output, "dina sources list detail ID --urls")
  expect_match(workflow$output, "dina sources list guide ID --urls")
  expect_match(workflow$output, "dina sources explore SOURCETYPE")
  expect_match(workflow$output, "dina sources include SOURCETYPE --dry-run")
  expect_match(workflow$output, "dina sources table SOURCETYPE \\[TABLE\\]")
  expect_match(workflow$output, "dina sources include SOURCETYPE --confirm")
  expect_match(workflow$output, "dina sources include SOURCETYPE --restore")
  expect_false(grepl("dina sources review", workflow$output, fixed = TRUE))
  expect_false(grepl("dina sources integrate", workflow$output, fixed = TRUE))
  expect_false(grepl("dina sources diagnose country-sna", workflow$output, fixed = TRUE))
  expect_false(grepl("dina update gate", workflow$output, fixed = TRUE))

  commands <- run_dina_cli(c("commands"), root = mini_repo())
  expect_equal(commands$status, 0L)
  expect_match(commands$output, "Workflow guide")
  expect_match(commands$output, "Preview input zip")
  expect_match(commands$output, "Dropbox input zip")
  expect_match(commands$output, "Explore source type")
  expect_match(commands$output, "Include source type")
  expect_match(commands$output, "Confirm source type")
  expect_match(commands$output, "Restore source type")
  expect_match(commands$output, "Source type table preview")
  expect_false(grepl("Update recipe", commands$output, fixed = TRUE))
  expect_false(grepl("Country-SNA diagnostic", commands$output, fixed = TRUE))
  expect_false(grepl("Data check", commands$output, fixed = TRUE))

  sources <- run_dina_cli(c("help", "sources"))
  expect_equal(sources$status, 0L)
  expect_match(sources$output, "dina sources list \\[SOURCETYPE\\] \\[--country ISO\\] \\[--urls\\]")
  expect_match(sources$output, "dina sources list detail ID")
  expect_match(sources$output, "dina sources list guide")
  expect_match(sources$output, "dina sources list workflow")
  expect_match(sources$output, "dina sources fetch \\[ID\\|SOURCETYPE\\|--all\\] \\[--dry-run\\]")
  expect_match(sources$output, "dina sources compare \\[--metadata-only\\] \\[--hash-all\\] \\[--deep\\]")
  expect_match(sources$output, "dina sources explore SOURCETYPE")
  expect_match(sources$output, "dina sources include SOURCETYPE")
  expect_match(sources$output, "dina sources table SOURCETYPE")
  expect_match(sources$output, "--confirm")
  expect_match(sources$output, "--restore")
  expect_match(sources$output, "Source registry, incoming `_new` buckets, fetchers, and baseline comparison")
  expect_match(sources$output, "sna, admin, admin-microdata, surveys")
  expect_false(grepl("dina sources show ID", sources$output, fixed = TRUE))
  expect_false(grepl("dina sources guide [ID", sources$output, fixed = TRUE))
  expect_false(grepl("dina sources review", sources$output, fixed = TRUE))
  expect_false(grepl("dina sources integrate", sources$output, fixed = TRUE))
  expect_false(grepl("dina sources diagnose country-sna", sources$output, fixed = TRUE))

  config <- run_dina_cli(c("help", "config"))
  expect_equal(config$status, 0L)
  expect_match(config$output, "dina update config show")
  expect_match(config$output, "dina update config edit")
  expect_false(grepl("dina config propose", config$output, fixed = TRUE))
  expect_false(grepl("dina update config set", config$output, fixed = TRUE))

  compress <- run_dina_cli(c("help", "compress"))
  expect_equal(compress$status, 0L)
  expect_match(compress$output, "dina compress input")
  expect_match(compress$output, "--dropbox")
  expect_match(compress$output, "admin-microdata")

  data_help <- run_dina_cli(c("help", "data"))
  expect_equal(data_help$status, 0L)
  expect_match(data_help$output, "Unknown help topic `data`")

  data_check <- run_dina_cli(c("data", "check"))
  expect_true(data_check$status != 0L)
  expect_match(data_check$output, "Unknown command: data")
})

test_that("sources list is compact by default and exposes richer views", {
  root <- mini_repo()

  compact <- run_dina_cli(c("sources", "list", "--no-menu"), root = root)
  expect_equal(compact$status, 0L)
  expect_match(compact$output, "id[[:space:]]+type[[:space:]]+country[[:space:]]+method[[:space:]]+urls[[:space:]]+bucket[[:space:]]+destination[[:space:]]+transformer[[:space:]]+influence")
  expect_match(compact$output, "source-a")
  expect_match(compact$output, "fixture")
  expect_match(compact$output, "fixture page")
  expect_match(compact$output, "input_data/\\{basename\\}")
  expect_match(compact$output, "01a.do")
  expect_match(compact$output, "More source detail")
  expect_match(compact$output, "dina sources list detail source-a --urls")
  expect_false(grepl("Source List Actions", compact$output, fixed = TRUE))

  workflow <- run_dina_cli(c("sources", "list", "workflow", "--no-menu"), root = root)
  expect_equal(workflow$status, 0L)
  expect_match(workflow$output, "fetch[[:space:]]+bucket[[:space:]]+destination[[:space:]]+transformer[[:space:]]+tasks")

  paths <- run_dina_cli(c("sources", "list", "paths", "--no-menu"), root = root)
  expect_equal(paths$status, 0L)
  expect_match(paths$output, "canonical:")
  expect_match(paths$output, "inbox:")
  expect_match(paths$output, "destination:")

  urls <- run_dina_cli(c("sources", "list", "urls", "--no-menu"), root = root)
  expect_equal(urls$status, 0L)
  expect_match(urls$output, "https://example.test/source-a")
})

test_that("source inbox displays use normalized public buckets", {
  retired_inboxes <- c(
    "input_data/_new/admin_tax",
    "input_data/_new/admin_tax_aux",
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

  guide <- run_dina_cli(c("sources", "inbox", "guide"), root = repo_root_for_tests)
  expect_equal(guide$status, 0L)
  expect_match(guide$output, "input_data/_new/admin")
  expect_match(guide$output, "input_data/_new/sna")
  expect_match(guide$output, "input_data/_new/surveys")
  expect_match(guide$output, "input_data/_new/other")
  expect_false(any(vapply(retired_inboxes, grepl, logical(1), x = guide$output, fixed = TRUE)))

  paths <- run_dina_cli(c("sources", "list", "paths", "--no-menu"), root = repo_root_for_tests)
  expect_equal(paths$status, 0L)
  expect_match(paths$output, "input_data/_new/admin")
  expect_match(paths$output, "input_data/_new/sna")
  expect_match(paths$output, "input_data/_new/surveys")
  expect_match(paths$output, "input_data/_new/other")
  expect_false(any(vapply(retired_inboxes, grepl, logical(1), x = paths$output, fixed = TRUE)))

  inbox_state <- dina_sources_inbox_init(repo_root_for_tests, dry_run = TRUE, migrate = FALSE)
  expect_equal(sort(basename(inbox_state$buckets$bucket)), c("admin", "other", "sna", "surveys"))

  mex_fetch <- run_dina_cli(c("sources", "fetch", "country-sna-mex", "--dry-run"), root = repo_root_for_tests)
  expect_equal(mex_fetch$status, 0L)
  expect_match(mex_fetch$output, "input_data/_new/sna/MEX/tabulados_si.zip")
  expect_match(mex_fetch$output, "dina sources explore sna")
  expect_match(mex_fetch$output, "dina sources include sna --dry-run")
  expect_false(grepl("input_data/_new/country_sna", mex_fetch$output, fixed = TRUE))

  minwage_fetch <- run_dina_cli(c("sources", "fetch", "bra-minwage", "--dry-run"), root = repo_root_for_tests)
  expect_equal(minwage_fetch$status, 0L)
  expect_match(minwage_fetch$output, "input_data/_new/admin/BRA/wiki_minwage.csv")
  expect_match(minwage_fetch$output, "dina sources explore admin")
  expect_match(minwage_fetch$output, "dina sources include admin --dry-run")
  expect_false(grepl("No source validation/preparation workflow is implemented yet", minwage_fetch$output, fixed = TRUE))

  xrates_fetch <- run_dina_cli(c("sources", "fetch", "wb-xrates", "--dry-run"), root = repo_root_for_tests)
  expect_equal(xrates_fetch$status, 0L)
  expect_match(xrates_fetch$output, "input_data/_new/other/prices/wb-xrates.xls")
  expect_false(grepl("input_data/_new/survey_inputs", xrates_fetch$output, fixed = TRUE))
})

test_that("public source type filters aggregate internal families including wid", {
  root <- mini_repo()
  dina_write_yaml(list(sources = list(
    list(id = "sna-macro", family = "macro_sna", method = "manual", notes = "macro SNA"),
    list(id = "sna-country", family = "country_sna", method = "manual", notes = "country SNA"),
    list(id = "admin-tax", family = "admin_tax", method = "manual", notes = "admin"),
    list(id = "admin-aux", family = "admin_tax_aux", method = "manual", notes = "admin aux"),
    list(id = "survey-cepal", family = "surveys", method = "manual", notes = "survey"),
    list(id = "wid-macro", family = "wid", method = "wid", notes = "wid"),
    list(id = "price-index", family = "prices", method = "manual", notes = "other")
  )), file.path(root, "config", "sources.yml"))

  expect_equal(sort(dina_source_resolve_family_filter("sna", root)), c("country_sna", "macro_sna"))
  expect_equal(sort(dina_source_resolve_family_filter("admin", root)), c("admin_tax", "admin_tax_aux"))
  expect_equal(dina_source_resolve_family_filter("surveys", root), "surveys")
  expect_equal(dina_source_resolve_family_filter("wid", root), "wid")
  expect_equal(dina_source_resolve_family_filter("other", root), "prices")

  sna <- run_dina_cli(c("sources", "list", "sna", "--no-menu"), root = root)
  expect_equal(sna$status, 0L)
  expect_match(sna$output, "sna-macro")
  expect_match(sna$output, "sna-country")
  expect_false(grepl("wid-macro", sna$output, fixed = TRUE))

  wid <- run_dina_cli(c("sources", "list", "wid", "--no-menu"), root = root)
  expect_equal(wid$status, 0L)
  expect_match(wid$output, "wid-macro")
  expect_false(grepl("price-index", wid$output, fixed = TRUE))

  wid_urls <- run_dina_cli(c("sources", "list", "urls", "wid", "--no-menu"), root = root)
  expect_equal(wid_urls$status, 0L)
  expect_match(wid_urls$output, "wid-macro")

  wid_guide <- run_dina_cli(c("sources", "list", "guide", "wid", "--urls"), root = root)
  expect_equal(wid_guide$status, 0L)
  expect_match(wid_guide$output, "wid-macro")

  wid_workflow <- run_dina_cli(c("sources", "list", "workflow", "wid", "--no-menu"), root = root)
  expect_equal(wid_workflow$status, 0L)
  expect_match(wid_workflow$output, "wid-macro")

  wid_paths <- run_dina_cli(c("sources", "list", "paths", "wid", "--no-menu"), root = root)
  expect_equal(wid_paths$status, 0L)
  expect_match(wid_paths$output, "wid-macro")

  compat <- run_dina_cli(c("sources", "list", "--family", "wid", "--no-menu"), root = root)
  expect_equal(compat$status, 0L)
  expect_match(compat$output, "wid-macro")

  unsupported <- run_dina_cli(c("sources", "explore", "wid"), root = root)
  expect_true(unsupported$status != 0L)
  expect_match(unsupported$output, "not implemented yet")
  expect_match(unsupported$output, "dina sources list wid")

  wid_fetch <- run_dina_cli(c("sources", "fetch", "wid", "--dry-run"), root = root)
  expect_equal(wid_fetch$status, 0L)
  expect_match(wid_fetch$output, "wid-macro")
  expect_match(wid_fetch$output, "No automatic fetch")
  expect_match(wid_fetch$output, "dina sources list guide wid --urls")
  expect_false(grepl("No bucket fetch rows matched", wid_fetch$output, fixed = TRUE))
})

test_that("country-SNA table preview prints compact explorer tables", {
  root <- mini_repo()
  run <- file.path(root, "output", "experiments", "country_sna_explore")
  dir.create(file.path(run, "tables"), recursive = TRUE)
  utils::write.csv(
    data.frame(country = "AAA", year = 2024L, year_role = "extension", expected_action = "extension_expect_values", stringsAsFactors = FALSE),
    file.path(run, "tables", "year_expectations.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    data.frame(
      country = "AAA",
      year = c(2024L, 2024L),
      year_role = "extension",
      variable = c("D4_cei", "D43_cei"),
      expected_status = "expected_value",
      structure_status = "structure_evidence_available",
      stringsAsFactors = FALSE
    ),
    file.path(run, "tables", "variable_expectations.csv"),
    row.names = FALSE
  )

  years <- run_dina_cli(c("sources", "table", "sna", "year_expectations", "--run", run), root = root)
  expect_equal(years$status, 0L)
  expect_match(years$output, "Country-SNA Table")
  expect_match(years$output, "extension_expect_values")

  variables <- run_dina_cli(c("sources", "table", "sna", "variable_expectations", "--run", run), root = root)
  expect_equal(variables$status, 0L)
  expect_match(variables$output, "variable_expectations summary")
  expect_match(variables$output, "variables")
  expect_false(grepl("D43_cei", variables$output, fixed = TRUE))

  all_tables <- run_dina_cli(c("sources", "table", "sna", "--run", run, "--limit", "1"), root = root)
  expect_equal(all_tables$status, 0L)
  expect_match(all_tables$output, "Country-SNA Tables")
  expect_match(all_tables$output, "Country-SNA Table: year_expectations")
  expect_match(all_tables$output, "Country-SNA Table: variable_expectations summary")

  deprecated <- run_dina_cli(c("sources", "table", "country-sna", "year_expectations", "--run", run), root = root)
  expect_equal(deprecated$status, 0L)
  expect_match(deprecated$output, "deprecated")
})

test_that("source list follow-up menu is dismissible and routes views", {
  source_cli_for_tests()
  root <- mini_repo()
  registry <- dina_source_registry(root)

  con <- textConnection("\n")
  quit_output <- capture.output(dina_source_list_actions_menu(registry, root = root, input = con, is_terminal = TRUE))
  close(con)
  expect_match(paste(quit_output, collapse = "\n"), "Source List Actions")
  expect_false(grepl("canonical:", paste(quit_output, collapse = "\n"), fixed = TRUE))

  con <- textConnection("3\n")
  workflow_output <- capture.output(dina_source_list_actions_menu(registry, root = root, input = con, is_terminal = TRUE))
  close(con)
  expect_match(paste(workflow_output, collapse = "\n"), "fetch[[:space:]]+bucket[[:space:]]+destination")

  con <- textConnection("4\n")
  paths_output <- capture.output(dina_source_list_actions_menu(registry, root = root, input = con, is_terminal = TRUE))
  close(con)
  expect_match(paste(paths_output, collapse = "\n"), "canonical:")

  con <- textConnection("5\n")
  urls_output <- capture.output(dina_source_list_actions_menu(registry, root = root, input = con, is_terminal = TRUE))
  close(con)
  expect_match(paste(urls_output, collapse = "\n"), "https://example.test/source-a")

  con <- textConnection("1\nsource-a\n")
  detail_output <- capture.output(dina_source_list_actions_menu(registry, root = root, input = con, is_terminal = TRUE))
  close(con)
  expect_match(paste(detail_output, collapse = "\n"), "Source Detail")
  expect_match(paste(detail_output, collapse = "\n"), "source type: fixture")
  expect_match(paste(detail_output, collapse = "\n"), "internal family: fixture")
  expect_match(paste(detail_output, collapse = "\n"), "transformer:")
})

test_that("source list detail, guide, compatibility aliases, fields, compare, and retired commands use the source model", {
  root <- mini_repo()
  show <- run_dina_cli(c("sources", "list", "detail", "source-a", "--urls"), root = root)
  expect_equal(show$status, 0L)
  expect_match(show$output, "source type:")
  expect_match(show$output, "internal family:")
  expect_match(show$output, "destination:")
  expect_match(show$output, "transformer:")
  expect_match(show$output, "notes:")
  expect_false(grepl("checks:", show$output, fixed = TRUE))
  expect_match(show$output, "https://example.test/source-a")

  guide <- run_dina_cli(c("sources", "list", "guide", "source-a", "--urls"), root = root)
  expect_equal(guide$status, 0L)
  expect_match(guide$output, "bucket:")
  expect_match(guide$output, "destination:")
  expect_match(guide$output, "tasks:")

  old_show <- run_dina_cli(c("sources", "show", "source-a", "--urls"), root = root)
  expect_equal(old_show$status, 0L)
  expect_match(old_show$output, "deprecated")

  old_guide <- run_dina_cli(c("sources", "guide", "source-a"), root = root)
  expect_equal(old_guide$status, 0L)
  expect_match(old_guide$output, "deprecated")

  fields <- run_dina_cli(c("sources", "fields"), root = root)
  expect_equal(fields$status, 0L)
  expect_match(fields$output, "Views:")
  expect_match(fields$output, "compact[[:space:]]+id, source type, country, method, urls")
  expect_match(fields$output, "Public source type")
  expect_match(fields$output, "Filters:")
  expect_match(fields$output, "Methods:")

  dina_update_start("2026", root = root)
  dir.create(file.path(root, "input_data", "_new", "fixture"), recursive = TRUE, showWarnings = FALSE)
  writeLines("new", file.path(root, "input_data", "_new", "fixture", "source_2025.xlsx"))

  review <- run_dina_cli(c("sources", "review"), root = root)
  expect_true(review$status != 0L)
  expect_match(review$output, "retired")
  expect_match(review$output, "No replacement data validation/preparation process is implemented yet")
  expect_false(grepl("Incoming Source Review", review$output, fixed = TRUE))

  preview <- run_dina_cli(c("sources", "integrate", "source-a"), root = root)
  expect_true(preview$status != 0L)
  expect_match(preview$output, "retired")
  expect_false(file.exists(file.path(root, "input_data", "source_2025.xlsx")))

  diagnose <- run_dina_cli(c("sources", "diagnose", "country-sna"), root = root)
  expect_true(diagnose$status != 0L)
  expect_match(diagnose$output, "retired")
  expect_match(diagnose$output, "dina sources explore sna")

  apply <- run_dina_cli(c("sources", "include", "country-sna", "--apply"), root = root)
  expect_true(apply$status != 0L)
  expect_match(apply$output, "Use `--confirm`")

  compare <- run_dina_cli(c("sources", "compare"), root = root)
  expect_equal(compare$status, 0L)
  expect_match(compare$output, "Source Compare")
  expect_match(compare$output, "Compares configured source files against the update baseline")
  expect_false(grepl("Source Status", compare$output, fixed = TRUE))

  status <- run_dina_cli(c("sources", "status"), root = root)
  expect_equal(status$status, 0L)
  expect_match(status$output, "deprecated")
  expect_match(status$output, "dina sources compare")
  expect_match(status$output, "Source Compare")
})

test_that("sources fetch previews direct URL targets under _new", {
  root <- mini_repo()
  remote <- file.path(root, "remote.csv")
  writeLines("value", remote)
  dina_write_yaml(list(sources = list(
    list(
      id = "direct-source",
      family = "fixture",
      country = "AAA",
      method = "url",
      url = paste0("file://", remote),
      inbox = c("input_data/_new/fixture/*.csv"),
      destination = "input_data/{basename}",
      transformer = "code/Stata/01a.do",
      notes = "Direct URL fixture."
    )
  )), file.path(root, "config", "sources.yml"))

  dry <- run_dina_cli(c("sources", "fetch", "direct-source", "--dry-run"), root = root)
  expect_equal(dry$status, 0L)
  expect_match(dry$output, "direct-source")
  expect_match(dry$output, "would fetch")
  expect_match(dry$output, "input_data/_new/fixture/remote.csv")
  expect_false(file.exists(file.path(root, "input_data", "_new", "fixture", "remote.csv")))

  by_type <- run_dina_cli(c("sources", "fetch", "fixture", "--dry-run"), root = root)
  expect_equal(by_type$status, 0L)
  expect_match(by_type$output, "direct-source")
  expect_match(by_type$output, "would fetch")

  dina_write_yaml(list(sources = list(
    list(
      id = "fixture",
      family = "fixture",
      country = "AAA",
      method = "url",
      url = paste0("file://", remote),
      inbox = c("input_data/_new/fixture/exact.csv"),
      destination = "input_data/{basename}",
      transformer = "code/Stata/01a.do",
      notes = "Exact source id fixture."
    ),
    list(
      id = "other-fixture",
      family = "fixture",
      country = "AAA",
      method = "url",
      url = paste0("file://", remote),
      inbox = c("input_data/_new/fixture/other.csv"),
      destination = "input_data/{basename}",
      transformer = "code/Stata/01a.do",
      notes = "Same source type fixture."
    )
  )), file.path(root, "config", "sources.yml"))

  exact <- run_dina_cli(c("sources", "fetch", "fixture", "--dry-run"), root = root)
  expect_equal(exact$status, 0L)
  expect_match(exact$output, "fixture")
  expect_false(grepl("other-fixture", exact$output, fixed = TRUE))
})

test_that("run executes by default and dry-run must be explicit", {
  root <- mini_repo()
  numbered_pipeline(root)
  dir.create(file.path(root, "input_data"), recursive = TRUE, showWarnings = FALSE)
  writeLines("input", file.path(root, "input_data", "a.txt"))
  fake_stata <- file.path(root, "fake-stata")
  writeLines(c(
    "#!/bin/sh",
    "case \"$3\" in",
    "  *01a.do) mkdir -p output; echo a > output/a.txt ;;",
    "  *01b.do) mkdir -p output; echo b > output/b.txt ;;",
    "  *02a.do) mkdir -p output; echo c > output/c.txt ;;",
    "  *07d.do) mkdir -p output; echo d > output/d.txt ;;",
    "esac",
    "exit 0"
  ), fake_stata)
  Sys.chmod(fake_stata, "0755")

  dry <- run_dina_cli(c("run", "01a", "--dry-run"), root = root, env = sprintf("DINA_STATA_CMD=%s", fake_stata))
  expect_equal(dry$status, 0L)
  expect_match(dry$output, "dry_run")
  expect_false(file.exists(file.path(root, "output", "a.txt")))

  executed <- run_dina_cli(c("run", "01a"), root = root, env = sprintf("DINA_STATA_CMD=%s", fake_stata))
  expect_equal(executed$status, 0L)
  expect_match(executed$output, "succeeded")
  expect_true(file.exists(file.path(root, "output", "a.txt")))

  listed <- run_dina_cli(c("run", "list"), root = root)
  expect_equal(listed$status, 0L)
  expect_match(listed$output, "01a")

  why <- run_dina_cli(c("run", "why", "01a"), root = root)
  expect_equal(why$status, 0L)
  expect_match(why$output, "Why 01a-clean-macro-data")
})

test_that("todo commands only change todo state", {
  root <- mini_repo()
  dina_update_start("2026", root = root)

  listed <- run_dina_cli(c("todo"), root = root)
  expect_equal(listed$status, 0L)
  expect_match(listed$output, "no[[:space:]]+review-config")
  expect_match(listed$output, "Modify this list:")
  expect_match(listed$output, "dina todo check ID")
  expect_match(listed$output, "config/todo.yml")
  expect_match(listed$output, "active update manifest")

  checked <- run_dina_cli(c("todo", "check", "review-config"), root = root)
  expect_equal(checked$status, 0L)
  expect_true("review-config" %in% dina_load_session(root = root)$todo$checked)

  unchecked <- run_dina_cli(c("todo", "uncheck", "review-config"), root = root)
  expect_equal(unchecked$status, 0L)
  expect_false("review-config" %in% dina_load_session(root = root)$todo$checked)

  run_status <- run_dina_cli(c("run", "list"), root = root)
  expect_equal(run_status$status, 0L)

  reset <- run_dina_cli(c("todo", "reset"), root = root)
  expect_equal(reset$status, 0L)
  expect_equal(as.character(dina_load_session(root = root)$todo$checked), character())
})

test_that("config and maintain commands are visible in the new shape", {
  root <- mini_repo()
  dina_update_start("2026", root = root)

  check <- run_dina_cli(c("config", "check"), root = root)
  expect_equal(check$status, 0L)
  expect_match(check$output, "Config Check")

  proposal <- run_dina_cli(c("config", "propose", "years.last", "2024"), root = root)
  expect_true(proposal$status != 0L)
  expect_match(proposal$output, "retired")
  expect_equal(dina_config(root, expand_env = FALSE)$years$last, 2023L)

  config_set <- run_dina_cli(c("config", "set", "years.last", "2024"), root = root)
  expect_true(config_set$status != 0L)
  expect_match(config_set$output, "retired")
  expect_equal(dina_config(root, expand_env = FALSE)$years$last, 2023L)

  update_set <- run_dina_cli(c("update", "config", "set", "years.last", "2024"), root = root)
  expect_true(update_set$status != 0L)
  expect_match(update_set$output, "retired")

  repo_status <- run_dina_cli(c("maintain", "repo-status"), root = root)
  expect_equal(repo_status$status, 0L)
  expect_match(repo_status$output, "Repo Status")
})

test_that("retired update commands fail with explicit replacements", {
  root <- mini_repo()
  dina_update_start("2026", root = root)

  roadmap <- run_dina_cli(c("update", "roadmap"), root = root)
  expect_true(roadmap$status != 0L)
  expect_match(roadmap$output, "was removed")
  expect_match(roadmap$output, "dina todo")

  gate <- run_dina_cli(c("update", "gate", "parameters"), root = root)
  expect_true(gate$status != 0L)
  expect_match(gate$output, "was removed")

  mark <- run_dina_cli(c("update", "mark", "parameters/year-scope"), root = root)
  expect_true(mark$status != 0L)
  expect_match(mark$output, "dina todo check")

  finalize <- run_dina_cli(c("update", "finalize"), root = root)
  expect_true(finalize$status != 0L)
  expect_match(finalize$output, "dina update close")
})
