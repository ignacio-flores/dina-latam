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
  expect_match(main$output, "`sources list \\[--view VIEW\\]`")
  expect_match(main$output, "`sources compare`")
  expect_match(main$output, "`sources explore country-sna`")
  expect_match(main$output, "`sources include country-sna`")
  expect_match(main$output, "`run list\\|why TASK`")
  expect_match(main$output, "`todo \\[check\\|uncheck\\|reset\\]`")
  expect_match(main$output, "`maintain repo-status\\|repo-diff`")
  expect_match(main$output, "dina run 01a")
  expect_match(main$output, "dina run 01a --dry-run")
  expect_false(grepl("update roadmap", main$output, fixed = TRUE))
  expect_false(grepl("update gate", main$output, fixed = TRUE))
  expect_false(grepl("update mark", main$output, fixed = TRUE))
  expect_false(grepl("sources refresh", main$output, fixed = TRUE))
  expect_false(grepl("`sources review`", main$output, fixed = TRUE))
  expect_false(grepl("`sources integrate", main$output, fixed = TRUE))
  expect_false(grepl("`sources diagnose country-sna`", main$output, fixed = TRUE))
  expect_false(grepl("update finalize", main$output, fixed = TRUE))

  workflow <- run_dina_cli(c("help", "workflow"))
  expect_equal(workflow$status, 0L)
  expect_match(workflow$output, "1\\. Start or resume the workspace")
  expect_match(workflow$output, "2\\. Work from sources")
  expect_match(workflow$output, "3\\. Run the pipeline")
  expect_match(workflow$output, "4\\. Keep a loose todo list")
  expect_match(workflow$output, "5\\. Close with a report")
  expect_match(workflow$output, "dina sources compare")
  expect_match(workflow$output, "dina sources explore country-sna")
  expect_match(workflow$output, "dina sources include country-sna --dry-run")
  expect_false(grepl("dina sources review", workflow$output, fixed = TRUE))
  expect_false(grepl("dina sources integrate", workflow$output, fixed = TRUE))
  expect_false(grepl("dina sources diagnose country-sna", workflow$output, fixed = TRUE))
  expect_false(grepl("dina update gate", workflow$output, fixed = TRUE))

  commands <- run_dina_cli(c("commands"), root = mini_repo())
  expect_equal(commands$status, 0L)
  expect_match(commands$output, "Workflow guide")
  expect_match(commands$output, "Country-SNA explore")
  expect_match(commands$output, "Country-SNA include")
  expect_false(grepl("Update recipe", commands$output, fixed = TRUE))
  expect_false(grepl("Country-SNA diagnostic", commands$output, fixed = TRUE))

  sources <- run_dina_cli(c("help", "sources"))
  expect_equal(sources$status, 0L)
  expect_match(sources$output, "dina sources list \\[FILTERS\\] \\[--view compact\\|workflow\\|paths\\|all\\]")
  expect_match(sources$output, "dina sources fetch \\[ID\\|--family FAMILY\\|--all\\] \\[--dry-run\\]")
  expect_match(sources$output, "dina sources compare \\[--metadata-only\\] \\[--hash-all\\] \\[--deep\\]")
  expect_match(sources$output, "dina sources explore country-sna")
  expect_match(sources$output, "dina sources include country-sna")
  expect_match(sources$output, "Source registry, incoming `_new` buckets, fetchers, and baseline comparison")
  expect_false(grepl("dina sources review", sources$output, fixed = TRUE))
  expect_false(grepl("dina sources integrate", sources$output, fixed = TRUE))
  expect_false(grepl("dina sources diagnose country-sna", sources$output, fixed = TRUE))
})

test_that("sources list is compact by default and exposes richer views", {
  root <- mini_repo()

  compact <- run_dina_cli(c("sources", "list", "--no-menu"), root = root)
  expect_equal(compact$status, 0L)
  expect_match(compact$output, "id[[:space:]]+family[[:space:]]+country[[:space:]]+method[[:space:]]+bucket[[:space:]]+destination[[:space:]]+transformer[[:space:]]+notes")
  expect_match(compact$output, "source-a")
  expect_match(compact$output, "fixture")
  expect_match(compact$output, "input_data/\\{basename\\}")
  expect_match(compact$output, "01a.do")
  expect_match(compact$output, "Fixture source")
  expect_false(grepl("Source List Actions", compact$output, fixed = TRUE))

  workflow <- run_dina_cli(c("sources", "list", "--view", "workflow", "--no-menu"), root = root)
  expect_equal(workflow$status, 0L)
  expect_match(workflow$output, "fetch[[:space:]]+bucket[[:space:]]+destination[[:space:]]+transformer[[:space:]]+tasks")

  paths <- run_dina_cli(c("sources", "list", "--view", "paths", "--no-menu"), root = root)
  expect_equal(paths$status, 0L)
  expect_match(paths$output, "canonical:")
  expect_match(paths$output, "inbox:")
  expect_match(paths$output, "destination:")

  urls <- run_dina_cli(c("sources", "list", "--urls", "--no-menu"), root = root)
  expect_equal(urls$status, 0L)
  expect_match(urls$output, "https://example.test/source-a")
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

  con <- textConnection("2\n")
  workflow_output <- capture.output(dina_source_list_actions_menu(registry, root = root, input = con, is_terminal = TRUE))
  close(con)
  expect_match(paste(workflow_output, collapse = "\n"), "fetch[[:space:]]+bucket[[:space:]]+destination")

  con <- textConnection("3\n")
  paths_output <- capture.output(dina_source_list_actions_menu(registry, root = root, input = con, is_terminal = TRUE))
  close(con)
  expect_match(paste(paths_output, collapse = "\n"), "canonical:")

  con <- textConnection("4\n")
  urls_output <- capture.output(dina_source_list_actions_menu(registry, root = root, input = con, is_terminal = TRUE))
  close(con)
  expect_match(paste(urls_output, collapse = "\n"), "https://example.test/source-a")

  con <- textConnection("1\nsource-a\n")
  detail_output <- capture.output(dina_source_list_actions_menu(registry, root = root, input = con, is_terminal = TRUE))
  close(con)
  expect_match(paste(detail_output, collapse = "\n"), "Source Detail")
  expect_match(paste(detail_output, collapse = "\n"), "family: fixture")
  expect_match(paste(detail_output, collapse = "\n"), "transformer:")
})

test_that("source show, guide, fields, compare, and retired commands use the source model", {
  root <- mini_repo()
  show <- run_dina_cli(c("sources", "show", "source-a", "--view", "all", "--urls"), root = root)
  expect_equal(show$status, 0L)
  expect_match(show$output, "family:")
  expect_match(show$output, "destination:")
  expect_match(show$output, "transformer:")
  expect_match(show$output, "notes:")
  expect_false(grepl("checks:", show$output, fixed = TRUE))
  expect_match(show$output, "https://example.test/source-a")

  guide <- run_dina_cli(c("sources", "guide", "source-a", "--urls"), root = root)
  expect_equal(guide$status, 0L)
  expect_match(guide$output, "bucket:")
  expect_match(guide$output, "destination:")
  expect_match(guide$output, "tasks:")

  fields <- run_dina_cli(c("sources", "fields"), root = root)
  expect_equal(fields$status, 0L)
  expect_match(fields$output, "Views:")
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
  expect_match(diagnose$output, "dina sources explore country-sna")

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
})

test_that("run executes by default and dry-run must be explicit", {
  root <- mini_repo()
  numbered_pipeline(root)
  dir.create(file.path(root, "input_data"), recursive = TRUE)
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
  expect_equal(proposal$status, 0L)
  expect_match(proposal$output, "No files were changed")
  expect_equal(dina_config(root, expand_env = FALSE)$years$last, 2023L)

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
