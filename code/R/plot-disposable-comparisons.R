# Compare published pretax and disposable income shares. No calculations on
# microdata and no replacement of any scientific export take place here.
dina_disposable_comparison_groups <- function() c(
  p0p50 = "Bottom 50%", p50p90 = "Middle 40%", p90p100 = "Top 10%",
  p99p100 = "Top 1%", p99.9p100 = "Top 0.1%", p99.99p100 = "Top 0.01%")

dina_disposable_comparison_data <- function(pretax, disposable, config) {
  groups <- dina_disposable_comparison_groups()
  prepare <- function(data, code, concept) {
    required <- c("country", "year", "p", "widcode", "value")
    if (!all(required %in% names(data))) stop("Missing comparison fields for ", concept, call. = FALSE)
    data <- as.data.frame(data[data$widcode == code & data$p %in% names(groups), , drop = FALSE])
    if (!nrow(data) || anyNA(data[required]) || any(!is.finite(data$value)) ||
        any(data$value < -1e-8 | data$value > 1 + 1e-6)) {
      stop("Missing or invalid published shares for ", concept, call. = FALSE)
    }
    if (anyDuplicated(data[c("country", "year", "p")])) stop("Duplicate published shares for ", concept, call. = FALSE)
    data$concept <- concept
    data$group <- unname(groups[data$p])
    if (!"pub_source" %in% names(data)) data$pub_source <- ""
    data$pub_source[is.na(data$pub_source)] <- ""
    data[c("country", "year", "p", "value", "concept", "group", "pub_source")]
  }
  nat <- prepare(pretax, "sptinc992j", "Pretax")
  pod <- prepare(disposable, "scainc992j", "Disposable")
  countries <- sort(unique(nat$country))
  if (!setequal(countries, unique(pod$country)) || !all(unlist(config$countries) %in% countries)) {
    stop("Pretax/disposable comparison is missing an exported country contribution.", call. = FALSE)
  }
  first <- as.integer(config$years$first)
  last <- as.integer(config$export_validation$last_year)
  # Supplied pretax history may extend further back than the internal disposable
  # series. Compare within the requested horizon, retaining its final carried year.
  nat <- nat[nat$year >= first & nat$year <= last, , drop = FALSE]
  pod <- pod[pod$year >= first & pod$year <= last, , drop = FALSE]
  case <- function(x) paste(x$country, x$year, sep = "/")
  nat_cases <- unique(nat[c("country", "year")])
  pod_cases <- unique(pod[c("country", "year")])
  if (!setequal(case(nat_cases), case(pod_cases))) {
    missing <- union(setdiff(case(nat_cases), case(pod_cases)), setdiff(case(pod_cases), case(nat_cases)))
    stop("Pretax/disposable comparison has unmatched published years: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (!all(countries %in% nat_cases$country)) stop("No common published years for an exported country.", call. = FALSE)
  check_groups <- function(data) {
    pieces <- split(data$p, case(data))
    if (any(!vapply(pieces, function(x) setequal(x, names(groups)) && length(x) == length(groups), logical(1)))) {
      stop("Published comparison is missing an expected income-group share.", call. = FALSE)
    }
  }
  check_groups(nat); check_groups(pod)
  out <- rbind(nat, pod)
  out[order(out$country, out$year, match(out$p, names(groups)), out$concept), , drop = FALSE]
}

dina_disposable_comparison_plot <- function(path, data) {
  groups <- dina_disposable_comparison_groups()
  colors <- c(Pretax = "#0072B2", Disposable = "#D55E00")
  types <- c(Pretax = 1, Disposable = 2)
  grDevices::pdf(path, width = 8.2, height = 8.4, useDingbats = FALSE)
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::layout(rbind(c(1, 1), c(2, 3), c(4, 5), c(6, 7)), heights = c(.5, 3, 3, 3))
  graphics::par(mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0))
  graphics::plot.new()
  graphics::legend("center", names(colors), col = colors, lty = types,
    lwd = 1.8, ncol = 2, bty = "n", cex = .95)
  graphics::par(mar = c(3.5, 4.1, 2.1, .8), mgp = c(2.5, .8, 0),
    cex.axis = .9, cex.lab = .95, cex.main = 1)
  for (p in names(groups)) {
    rows <- data[data$p == p, , drop = FALSE]
    x <- range(rows$year); y <- range(rows$value) * 100
    if (diff(x) == 0) x <- x + c(-.5, .5)
    if (diff(y) == 0) y <- y + c(-1, 1) * .1
    graphics::plot(NA, xlim = x, ylim = y, xlab = "Year", ylab = "Income share (%)",
      main = groups[[p]], xaxt = "n")
    interval <- if (diff(x) > 40) 10 else 5
    ticks <- seq(floor(x[1] / interval) * interval, ceiling(x[2] / interval) * interval, by = interval)
    ticks <- ticks[ticks >= x[1] & ticks <= x[2]]
    if (!length(ticks)) ticks <- unique(rows$year)
    graphics::axis(1, at = ticks)
    for (concept in names(colors)) {
      series <- rows[rows$concept == concept, , drop = FALSE]
      years <- seq.int(min(series$year), max(series$year))
      graphics::lines(years, series$value[match(years, series$year)] * 100,
        col = colors[[concept]], lty = types[[concept]], lwd = 1.8)
      if (nrow(series) == 1L) graphics::points(series$year, series$value * 100,
        col = colors[[concept]], pch = 16, cex = .7)
    }
  }
}

dina_disposable_comparison_export <- function(root, output_root, date, config) {
  steps <- as.character(unlist(config$export_validation$steps))
  if (!"pod" %in% steps) return(invisible(list()))
  if (!"nat" %in% steps) stop("Export nat alongside pod to produce the requested disposable-versus-pretax comparisons.", call. = FALSE)
  if (!grepl("^[A-Za-z0-9]+$", date)) stop("Invalid export date.", call. = FALSE)
  directory <- file.path(output_root, "output", "latest_wid_series")
  path <- file.path(directory, paste0("dina_latam_", date, ".dta"))
  if (!file.exists(path)) stop("Final combined income export is missing; comparisons were not published.", call. = FALSE)
  published <- as.data.frame(haven::read_dta(path, col_select =
    tidyselect::any_of(c("country", "year", "p", "widcode", "value", "pub_source"))))
  data <- dina_disposable_comparison_data(published, published, config)
  folder <- file.path(output_root, "output", "figures", "updates")
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  stage <- tempfile("income-comparison-", tmpdir = folder)
  dir.create(stage); on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  countries <- sort(unique(data$country))
  names <- paste0("update-", date, "-pod-vs-nat-", countries, ".pdf")
  source_notes <- setNames(lapply(countries, function(country) {
    rows <- data[data$country == country, , drop = FALSE]
    supplied_nat <- unique(rows$pub_source[rows$concept == "Pretax" & nzchar(rows$pub_source)])
    supplied_pod <- unique(rows$pub_source[rows$concept == "Disposable" & nzchar(rows$pub_source)])
    list(years = sort(unique(rows$year)), pretax_supplied_sources = supplied_nat,
      disposable_supplied_sources = supplied_pod,
      note = if (length(supplied_nat) && !length(supplied_pod))
        "Pretax includes the supplied research series; disposable uses internal calculations. They are separate source series, not a newly harmonized pretax/post-tax pair."
        else "Shares compare the two published income concepts; differences also reflect ranking people separately by each income concept.")
  }), countries)
  for (i in seq_along(countries)) dina_disposable_comparison_plot(file.path(stage, names[i]),
    data[data$country == countries[i], , drop = FALSE])
  report_name <- paste0("income-comparisons-", date, ".json")
  report <- list(version = 1L, date = date, status = "complete", concepts = c("nat", "pod"),
    series = file.path("output", "latest_wid_series", basename(path)),
    source = "Published aggregate shares, limited to matching published years within the export horizon",
    groups = as.list(dina_disposable_comparison_groups()),
    figures = setNames(as.list(file.path("output", "figures", "updates", names)), countries),
    countries = source_notes)
  jsonlite::write_json(report, file.path(stage, report_name), pretty = TRUE, auto_unbox = TRUE, null = "null")
  for (name in c(names, report_name)) {
    if (!file.rename(file.path(stage, name), file.path(folder, name))) stop("Could not publish comparison figure: ", name, call. = FALSE)
  }
  invisible(report)
}
