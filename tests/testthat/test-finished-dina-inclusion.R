finished_include_fixture <- function(root, countries = c("AAA", "BBB")) {
  skip_if_not_installed("haven")
  skip_if_not_installed("digest")
  dir.create(file.path(root, "input_data", "_new", "dina"), recursive = TRUE)
  dir.create(file.path(root, "input_data", "dina"), recursive = TRUE)
  for (module in c("finished_dina_explorer.R", "finished_dina_preparer.R")) {
    stopifnot(file.copy(file.path(repo_root_for_tests, "code", "R", "source-diagnostics", module),
      file.path(root, "code", "R", "source-diagnostics", module)))
  }
  sources <- lapply(countries, function(country) list(
    id = paste0(tolower(country), "-finished"), family = "dina", country = country,
    method = "manual", integration = "prepared_external",
    inbox = paste0("input_data/_new/dina/", country, ".dta"),
    canonical = paste0("input_data/dina/", country, ".dta"),
    prepared = paste0("input_data/dina/", country, "_prepared.dta"),
    finished_series = list(version = "v2", first_year = 2022L, last_year = 2022L,
      steps = "nat", units = "esn", publication_step = "nat", publication_unit = "esn",
      citation = "Fixture authors. Supplied series.", doi = "https://doi.org/fixture",
      population = "unconfirmed", currency = "unconfirmed", price_basis = "unconfirmed")
  ))
  dina_write_yaml(list(sources = sources), file.path(root, "config", "sources.yml"))
  engine <- new.env(parent = environment())
  sys.source(file.path(root, "code", "R", "source-diagnostics", "finished_dina_explorer.R"), engine)
  engine$gpinter_version <- "fixture-1"
  engine$calls <- 0L
  engine$finished_dina_prepare <- function(data, specification) {
    engine$calls <- engine$calls + 1L
    validation <- engine$finished_dina_validate(data, specification)
    data$fitted_gini <- 1 / 6
    list(data = data, validation = validation,
      changes = data.frame(country = data$country[1], year = 2022L, step = "nat",
        unit = "esn", percentile = "p0p1", field = "thr", before = 50, after = 50,
        difference = 0, relative_percent = 0, rule = "fixture", reason = "Fixture identity"),
      summary = data.frame(country = data$country[1], year = 2022L, step = "nat", unit = "esn",
        source_brackets = 127, prepared_brackets = 127, threshold_repairs = 0,
        threshold_nudges = 0, pooled_bins = 0, boundary_changes = 0,
        precision_changes = 0, publication = TRUE, max_mean_preservation_relative_error = 0),
      impact = data.frame(country = data$country[1], year = 2022L, step = "nat", unit = "esn",
        statistic = "average", before = 100, after = 100, difference = 0,
        basis = "unchanged", reason = "Fixture"),
      recipe = list(id = "fixture", version = 1L, gpinter_version = engine$gpinter_version))
  }
  p <- engine$finished_dina_export_grid()
  q <- c(p[-1L], 1)
  label <- function(x) format(x * 100, scientific = FALSE, trim = TRUE, digits = 12)
  for (source in sources) {
    avg <- 50 + 100 * (p + q) / 2
    share <- avg * (q - p) / 100
    top <- rev(cumsum(rev(share)))
    rows <- data.frame(percentile = paste0("p", label(p), "p", label(q)),
      country = source$country, year = 2022L, step = "nat", unit = "esn", gini = 1/6,
      p = p, average = 100, avg = avg, thr = 50 + 100 * p,
      bckt_sh = share, topsh = top, topavg = top * 100 / (1 - p))
    haven::write_dta(rows, file.path(root, source$inbox))
    writeLines(paste("previous original", source$country), file.path(root, source$canonical))
    writeLines(paste("previous prepared", source$country), file.path(root, source$prepared))
  }
  dir.create(file.path(root, "output", "latest_wid_series"))
  writeLines("published sentinel", file.path(root, "output", "latest_wid_series", "sentinel.txt"))
  run <- file.path(root, "output", "experiments", "finished_dina_explore", "runs", "fixture")
  dir.create(file.path(run, "tables"), recursive = TRUE)
  writeLines("raw review remains intact", file.path(run, "tables", "sentinel.txt"))
  inputs <- c(file.path(root, "input_data", "_new", "dina"), file.path(root, "config", "sources.yml"))
  record <- list(family = "dina", status = "needs_review", run = run, review_only = TRUE,
    reviewed_at = dina_now(), inputs = setNames(lapply(sort(inputs), dina_hash_path), sort(inputs)),
    watch = dina_review_watch(inputs))
  dina_write_json(record, dina_review_pointer(root, "dina"))
  list(root = root, record = record, engine = engine, sources = sources)
}

finished_include_input_hashes <- function(root) {
  paths <- list.files(file.path(root, c("input_data", "output/latest_wid_series")),
    recursive = TRUE, full.names = TRUE)
  paths <- sort(paths[!dir.exists(paths)])
  setNames(lapply(paths, dina_hash_file), paths)
}

test_that("finished DINA dry-run prepares every source and preserves scientific files and raw review", {
  source_cli_for_tests()
  root <- mini_repo()
  fixture <- finished_include_fixture(root)
  before <- finished_include_input_hashes(fixture$root)
  raw <- readLines(file.path(fixture$record$run, "tables", "sentinel.txt"))
  output <- capture.output(result <- dina_finished_include(fixture$root,
    flags = list("dry-run" = TRUE, country = "AAA"), is_terminal = FALSE,
    engine = fixture$engine))
  expect_equal(fixture$engine$calls, 2L)
  expect_equal(finished_include_input_hashes(fixture$root), before)
  expect_equal(result$run, fixture$record$run)
  expect_equal(readLines(file.path(result$run, "tables", "sentinel.txt")), raw)
  candidate <- dina_finished_include_candidate(fixture$root, result)
  expect_equal(candidate$status, "ready")
  expect_length(candidate$artifacts, 4L)
  expect_length(candidate$sources, 2L)
  expect_equal(candidate$sources[[1]]$citation, "Fixture authors. Supplied series.")
  expect_true(all(file.exists(unlist(candidate$tables))))
  expect_true(all(c("AAA", "BBB") %in% read.csv(candidate$tables$summary)$country))
  expect_match(paste(output, collapse = "\n"), "Accepted inputs and publications are unchanged")
  canonical_root <- normalizePath(fixture$root, mustWork = TRUE)
  expect_identical(dina_finished_include_identity(fixture$root,
    dina_finished_include_sources(fixture$root), fixture$engine),
    dina_finished_include_identity(canonical_root,
      dina_finished_include_sources(canonical_root), fixture$engine))
  expect_true(dina_finished_include_verify(candidate, canonical_root, result,
    dina_finished_include_sources(canonical_root), fixture$engine))
})

test_that("acceptance is byte-identical for originals, repeatable and safely restorable", {
  source_cli_for_tests()
  root <- mini_repo()
  fixture <- finished_include_fixture(root)
  before <- finished_include_input_hashes(fixture$root)
  capture.output(dry <- dina_finished_include(fixture$root, flags = list("dry-run" = TRUE),
    is_terminal = FALSE, engine = fixture$engine))
  candidate <- dina_finished_include_candidate(fixture$root, dry)
  capture.output(accepted <- dina_finished_include(fixture$root,
    flags = list(confirm = TRUE, "include-run" = candidate$run),
    is_terminal = FALSE, engine = fixture$engine))
  expect_equal(accepted$status, "included")
  expect_equal(accepted$run, fixture$record$run)
  for (source in fixture$sources) {
    expect_equal(dina_hash_file(file.path(fixture$root, source$inbox)),
      dina_hash_file(file.path(fixture$root, source$canonical)))
    prepared <- haven::read_dta(file.path(fixture$root, source$prepared))
    expect_equal(nrow(prepared), 127L)
    expect_true("fitted_gini" %in% names(prepared))
  }
  expect_equal(dina_read_json(file.path(accepted$confirmation, "confirmation.json"))$status, "confirmed")
  hashes <- finished_include_input_hashes(fixture$root)
  capture.output(again <- dina_finished_include(fixture$root, flags = list(confirm = TRUE),
    is_terminal = FALSE, engine = fixture$engine))
  expect_equal(fixture$engine$calls, 2L)
  expect_identical(again$confirmation, accepted$confirmation)
  expect_equal(finished_include_input_hashes(fixture$root), hashes)
  capture.output(restored <- dina_finished_include(fixture$root,
    flags = list(restore = accepted$confirmation), is_terminal = FALSE, engine = fixture$engine))
  expect_equal(finished_include_input_hashes(fixture$root), before)
  expect_equal(restored$status, "needs_review")
  capture.output(dina_finished_include_restore(fixture$root, accepted$confirmation))
  expect_equal(finished_include_input_hashes(fixture$root), before)
})

test_that("failed preparation retains prior accepted inputs and saves precise failure evidence", {
  source_cli_for_tests()
  root <- mini_repo()
  fixture <- finished_include_fixture(root)
  before <- finished_include_input_hashes(fixture$root)
  real_prepare <- fixture$engine$finished_dina_prepare
  fixture$engine$finished_dina_prepare <- function(data, specification) {
    if (data$country[1] == "BBB") stop("BBB 2022 nat/esn: mean reversal exceeds 0.001%")
    real_prepare(data, specification)
  }
  expect_error(capture.output(dina_finished_include(fixture$root,
    flags = list("dry-run" = TRUE), is_terminal = FALSE, engine = fixture$engine)),
    "preparation needs attention")
  record <- dina_review_read(fixture$root, "dina")
  candidate <- dina_finished_include_candidate(fixture$root, record)
  expect_equal(candidate$status, "blocked")
  expect_match(candidate$failures[[1]]$message, "BBB 2022 nat/esn")
  expect_match(paste(read.csv(candidate$tables$problems)$detail, collapse = "\n"), "0.001%")
  expect_equal(finished_include_input_hashes(fixture$root), before)
  expect_equal(record$status, "needs_review")
})

test_that("publication failure rolls back every touched file and the review pointer", {
  source_cli_for_tests()
  root <- mini_repo()
  fixture <- finished_include_fixture(root)
  capture.output(candidate <- dina_finished_include_prepare(fixture$root,
    fixture$record, engine = fixture$engine))
  record <- dina_review_read(fixture$root, "dina")
  before <- finished_include_input_hashes(fixture$root)
  pointer_hash <- dina_hash_file(dina_review_pointer(fixture$root, "dina"))
  count <- 0L
  failing_copy <- function(from, to) {
    count <<- count + 1L
    if (count == 2L) stop("fixture failed second replacement")
    dina_finished_include_copy(from, to)
  }
  expect_error(dina_finished_include_publish(candidate, fixture$root, record,
    dina_finished_include_sources(fixture$root), fixture$engine, copy_file = failing_copy),
    "fixture failed second replacement")
  expect_equal(finished_include_input_hashes(fixture$root), before)
  expect_identical(dina_hash_file(dina_review_pointer(fixture$root, "dina")), pointer_hash)
  manifests <- list.files(file.path(fixture$root, "output", "experiments", "finished_dina_include",
    "confirms"), pattern = "confirmation.json", full.names = TRUE, recursive = TRUE)
  expect_length(manifests, 1L)
  expect_equal(dina_read_json(manifests[[1]])$status, "rolled_back")
  expect_error(dina_finished_include_publish(candidate, fixture$root, record,
    dina_finished_include_sources(fixture$root), fixture$engine,
    save_record = function(record, root) stop("fixture pointer failure")),
    "fixture pointer failure")
  expect_equal(finished_include_input_hashes(fixture$root), before)
  expect_identical(dina_hash_file(dina_review_pointer(fixture$root, "dina")), pointer_hash)
})

test_that("stale candidates, edited evidence and damaged accepted files cannot be silently reused", {
  source_cli_for_tests()
  root <- mini_repo()
  fixture <- finished_include_fixture(root, "AAA")
  capture.output(dry <- dina_finished_include(fixture$root, list("dry-run" = TRUE),
    is_terminal = FALSE, engine = fixture$engine))
  candidate <- dina_finished_include_candidate(fixture$root, dry)
  fixture$engine$gpinter_version <- "fixture-2"
  expect_error(dina_finished_include_verify(candidate, fixture$root, dry,
    dina_finished_include_sources(fixture$root), fixture$engine), "GPinter version changed")
  fixture$engine$gpinter_version <- "fixture-1"
  table <- readBin(candidate$tables$changes, "raw", n = file.info(candidate$tables$changes)$size)
  writeLines("changed evidence", candidate$tables$changes)
  expect_error(dina_finished_include_verify(candidate, fixture$root, dry,
    dina_finished_include_sources(fixture$root), fixture$engine), "change report changed")
  writeBin(table, candidate$tables$changes)
  capture.output(accepted <- dina_finished_include(fixture$root, list(confirm = TRUE),
    is_terminal = FALSE, engine = fixture$engine))
  unlink(file.path(fixture$root, fixture$sources[[1]]$prepared))
  expect_error(capture.output(dina_finished_include(fixture$root, list(confirm = TRUE),
    is_terminal = FALSE, engine = fixture$engine)), "accepted DINA artifact is missing")
  expect_error(dina_finished_include_restore(fixture$root, accepted$confirmation),
    "changed after inclusion")
})

test_that("inclusion flags and destinations enforce the narrow operation boundary", {
  source_cli_for_tests()
  root <- mini_repo()
  fixture <- finished_include_fixture(root, "AAA")
  expect_error(dina_finished_include(fixture$root,
    list("dry-run" = TRUE, confirm = TRUE)), "cannot be combined")
  expect_error(dina_finished_include(fixture$root,
    list(restore = "backup", confirm = TRUE)), "acceptance or restore")
  expect_error(dina_finished_include_path(fixture$root, "input_data/dina/../other.dta",
    "input_data/dina"), "without")
  expect_error(dina_finished_include_path(fixture$root, "output/latest_wid_series/result.dta",
    "input_data/dina"), "must be inside")
  source <- fixture$sources[[1]]
  source$prepared <- source$canonical
  dina_write_yaml(list(sources = list(source)), file.path(fixture$root, "config", "sources.yml"))
  expect_error(dina_finished_include_sources(fixture$root), "distinct original and prepared")
  dina_write_yaml(list(sources = fixture$sources), file.path(fixture$root, "config", "sources.yml"))
  writeLines("changed", file.path(fixture$root, fixture$sources[[1]]$inbox))
  expect_error(dina_finished_include_verify_review(fixture$root, fixture$record,
    dina_finished_include_sources(fixture$root)), "source or registry changed")
})

test_that("a later exploration does not hide the active inclusion from restoration", {
  source_cli_for_tests()
  root <- mini_repo()
  fixture <- finished_include_fixture(root, "AAA")
  before <- finished_include_input_hashes(root)
  capture.output(accepted <- dina_finished_include(root, list(confirm = TRUE),
    is_terminal = FALSE, engine = fixture$engine))
  # Explore replaces its review pointer, while the accepted source generation
  # is still present and recorded in the inclusion journal.
  dina_review_save(fixture$record, root)
  expect_null(dina_review_read(root, "dina")$confirmation)
  # A process can also exit just after writing completion, before dropping its lock.
  lock <- file.path(root, "output", "experiments", "finished_dina_include", ".publishing")
  dir.create(lock)
  dina_write_json(list(pid = .Machine$integer.max, confirmation = accepted$confirmation),
    file.path(lock, "owner.json"))
  capture.output(dina_finished_include_restore(root, accepted$confirmation))
  expect_equal(finished_include_input_hashes(root), before)
  expect_false(dir.exists(lock))
})

test_that("journals prevent restoring an older active generation even after exploration", {
  source_cli_for_tests()
  root <- mini_repo()
  fixture <- finished_include_fixture(root, "AAA")
  capture.output(first <- dina_finished_include(root, list(confirm = TRUE),
    is_terminal = FALSE, engine = fixture$engine))
  capture.output(dina_finished_include(root, list("dry-run" = TRUE),
    is_terminal = FALSE, engine = fixture$engine))
  capture.output(second <- dina_finished_include(root, list(confirm = TRUE),
    is_terminal = FALSE, engine = fixture$engine))
  expect_false(identical(first$confirmation, second$confirmation))
  dina_review_save(fixture$record, root)
  expect_error(dina_finished_include_restore(root, first$confirmation), "newer inclusion is active")
  capture.output(dina_finished_include_restore(root, second$confirmation))
  expect_identical(dina_finished_include_latest_confirmation(root), first$confirmation)
  capture.output(dina_finished_include_restore(root, first$confirmation))
  expect_null(dina_finished_include_latest_confirmation(root))
})

test_that("explicit interrupted-publication recovery refuses live owners and unexpected changes", {
  source_cli_for_tests()
  root <- mini_repo()
  fixture <- finished_include_fixture(root, "AAA")
  unlink(file.path(root, fixture$sources[[1]]$prepared))
  before <- finished_include_input_hashes(root)
  capture.output(candidate <- dina_finished_include_prepare(root,
    fixture$record, engine = fixture$engine))
  record <- dina_review_read(root, "dina")
  count <- 0L
  expect_error(dina_finished_include_publish(candidate, root, record,
    dina_finished_include_sources(root), fixture$engine,
    copy_file = function(from, to) {
      count <<- count + 1L
      if (count == 2L) stop("fixture interrupted write")
      dina_finished_include_copy(from, to)
    }), "fixture interrupted write")
  paths <- list.files(file.path(root, "output", "experiments", "finished_dina_include", "confirms"),
    pattern = "^confirmation.json$", recursive = TRUE, full.names = TRUE)
  expect_length(paths, 1L)
  journal <- dina_read_json(paths[[1]])
  journal$status <- "publishing"
  journal$pid <- .Machine$integer.max
  expect_false(dina_finished_include_pid_alive(journal$pid))
  dina_write_json(journal, paths[[1]])
  dina_finished_include_copy(candidate$artifacts[[1]]$staged, candidate$artifacts[[1]]$destination)
  lock <- file.path(root, "output", "experiments", "finished_dina_include", ".publishing")
  dir.create(lock)
  dina_write_json(list(pid = journal$pid, confirmation = journal$run), file.path(lock, "owner.json"))
  # A live PID must be refused even when the bytes could be restored.
  journal$pid <- Sys.getpid()
  dina_write_json(journal, paths[[1]])
  expect_error(dina_finished_include_restore(root, journal$run), "still running")
  expect_true(dir.exists(lock))
  journal$pid <- .Machine$integer.max
  dina_write_json(journal, paths[[1]])
  # A manual change cannot be mistaken for one of the interrupted writes.
  writeLines("manual scientific change", candidate$artifacts[[1]]$destination)
  expect_error(dina_finished_include_restore(root, journal$run), "outside the interrupted publication")
  expect_true(dir.exists(lock))
  expect_identical(dina_read_json(file.path(lock, "owner.json"))$pid, as.integer(.Machine$integer.max))
  dina_finished_include_copy(candidate$artifacts[[1]]$staged, candidate$artifacts[[1]]$destination)
  capture.output(dina_finished_include_restore(root, journal$run))
  expect_equal(finished_include_input_hashes(root), before)
  expect_false(dir.exists(lock))
  recovered <- dina_read_json(paths[[1]])
  expect_equal(recovered$status, "restored")
  expect_true(recovered$recovered_interrupted_publication)
})
