disposable_plot_engine <- function() {
  env <- new.env(parent = globalenv())
  sys.source(file.path(repo_root_for_tests, "code/R/plot-disposable-comparisons.R"), env)
  env
}

disposable_plot_fixture <- function() {
  grid <- expand.grid(country = c("AAA", "BBB"), year = 2023:2025,
    p = c("p0p50", "p50p90", "p90p100", "p99p100", "p99.9p100", "p99.99p100"), stringsAsFactors = FALSE)
  nat <- grid; pod <- grid
  nat$widcode <- "sptinc992j"; pod$widcode <- "scainc992j"
  nat$value <- c(p0p50 = .15, p50p90 = .30, p90p100 = .55, p99p100 = .25, p99.9p100 = .10, p99.99p100 = .04)[nat$p]
  pod$value <- c(p0p50 = .20, p50p90 = .30, p90p100 = .50, p99p100 = .20, p99.9p100 = .08, p99.99p100 = .03)[pod$p]
  nat$pub_source <- ifelse(nat$country == "AAA", "approved-research", "")
  pod$pub_source <- ""
  early <- nat[nat$country == "AAA" & nat$year == 2023, ]; early$year <- 2018
  list(nat = rbind(nat, early), pod = pod,
    config = list(countries = c("AAA", "BBB"), years = list(first = 2023, last = 2024),
      export_validation = list(steps = c("nat", "pod"), last_year = 2025, unit = "esn")))
}

test_that("disposable comparisons use exact published shares and matched years", {
  engine <- disposable_plot_engine(); fixture <- disposable_plot_fixture()
  result <- engine$dina_disposable_comparison_data(fixture$nat, fixture$pod, fixture$config)
  expect_equal(nrow(result), 2 * 3 * 6 * 2)
  expect_setequal(result$year, 2023:2025)
  expect_false(2018 %in% result$year)
  expect_equal(result$value[result$concept == "Disposable" & result$p == "p0p50"], rep(.20, 6))
  expect_true(all(result$pub_source[result$country == "AAA" & result$concept == "Pretax"] == "approved-research"))
  expect_true(all(result$pub_source[result$concept == "Disposable"] == ""))
  bad <- fixture$pod[-1L, ]
  expect_error(engine$dina_disposable_comparison_data(fixture$nat, bad, fixture$config), "income-group")
  bad <- fixture$pod[fixture$pod$year != 2025, ]
  expect_error(engine$dina_disposable_comparison_data(fixture$nat, bad, fixture$config), "unmatched published years")
  bad <- fixture$pod[fixture$pod$country != "AAA", ]
  expect_error(engine$dina_disposable_comparison_data(fixture$nat, bad, fixture$config), "country contribution")
  bad <- rbind(fixture$pod, fixture$pod[1L, ])
  expect_error(engine$dina_disposable_comparison_data(fixture$nat, bad, fixture$config), "Duplicate")
})

test_that("nat-only exports create no disposable comparison files", {
  engine <- disposable_plot_engine(); fixture <- disposable_plot_fixture()
  root <- tempfile("no-pod-comparison-")
  fixture$config$export_validation$steps <- "nat"
  expect_identical(engine$dina_disposable_comparison_export(root, root, "26Oct2026", fixture$config), list())
  expect_false(dir.exists(root))
  fixture$config$export_validation$steps <- "pod"
  expect_error(engine$dina_disposable_comparison_export(root, root, "26Oct2026", fixture$config), "Export nat alongside pod")
  expect_false(dir.exists(root))
})

test_that("export completion requires and records conditional comparison artifacts", {
  skip_if_not_installed("haven")
  source_cli_for_tests(); root <- mini_repo()
  config <- dina_session_config(NULL, root, expand_env = FALSE)
  config$countries <- c("AAA", "BBB")
  config$export_validation$steps <- c("nat", "pod")
  dina_write_yaml(config, file.path(root, "config/dina.yml"))
  haven::write_dta(data.frame(iso = "AA", country = "AAA", year = 2024, p = "p0p100", widcode = "sptinc992j", value = 1),
    file.path(root, config$export_validation$previous_update_file))
  date <- "26Oct2026"
  dina_results_begin(root, date, config)
  folder <- file.path(root, "output/figures/updates")
  old <- file.path(folder, paste0("update-", date, "-sptinc992j-", names(dina_results_graph_names()), ".pdf"))
  series <- file.path(root, "output/latest_wid_series", paste0("dina_latam", c("_wide", "", "", ""),
    "_", date, c("", "", "_amory", "_disposable"), ".dta"))
  for (p in c(old, series)) {dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE); writeLines("fixture", p)}
  expect_error(dina_results_complete(root, date, config), "disposable-versus-pretax")
  expect_false(file.exists(dina_results_metadata_path(root, date)))
  figures <- setNames(file.path("output/figures/updates", paste0("update-", date, "-pod-vs-nat-", c("AAA", "BBB"), ".pdf")), c("AAA", "BBB"))
  receipt <- list(status = "complete", date = date, concepts = c("nat", "pod"), figures = as.list(figures),
    countries = list(AAA = list(years = 2024), BBB = list(years = 2024)))
  dina_write_json(receipt, file.path(folder, paste0("income-comparisons-", date, ".json")))
  for (p in file.path(root, figures)) writeLines("figure fixture", p)
  metadata <- dina_results_complete(root, date, config)
  expect_length(metadata$graphs, 6L)
  expect_length(metadata$income_comparisons, 2L)
  expect_length(metadata$series, 4L)
  expect_true(all(c(figures, metadata$income_comparison_report) %in% names(metadata$artifacts)))
  rows <- dina_results_inventory(root)$graphs
  expect_true(all(c("pod-nat-AAA", "pod-nat-BBB") %in% rows$graph))
  expect_true(all(rows$status == "Matches recorded baseline and settings"))
  unlink(file.path(root, figures[1]))
  expect_true(all(dina_results_inventory(root)$graphs$status == "Generated artifacts changed or missing"))
})
