#!/usr/bin/env Rscript
# Presentation-only exports from accepted publication data. Optional arguments
# are ISO3 codes; omission plots all active supplied-series countries.
# Example: Rscript --vanilla code/R/plot-finished-dina-for-latex.R ECU BRA

script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1L])
root <- normalizePath(file.path(dirname(script), "..", ".."), mustWork = TRUE)
Sys.setenv(DINA_CLI_SOURCE_ONLY = "1",
  DINA_CLI_SOURCE_FILE = file.path(root, "code", "R", "cli", "dina.R"))
source(Sys.getenv("DINA_CLI_SOURCE_FILE"))

plot_for_latex <- function(path, panels, anchor) {
  labels <- c("Original", "Prepared", "Internal estimates", "Final series",
    "Unadjusted continuation")
  colors <- c("black", "#009E73", "#737373", "#0072B2", "#D55E00")
  types <- c(2, 1, 1, 1, 3)
  widths <- c(1.6, 1.7, 1.2, 1.7, 1.2)
  grDevices::pdf(path, width = 8.2, height = 6.2, useDingbats = FALSE)
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::layout(rbind(c(1, 1), c(2, 3), c(4, 5)), heights = c(.65, 3, 3))
  graphics::par(mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0))
  graphics::plot.new()
  graphics::legend("center", labels, col = colors, lty = types, lwd = widths,
    bty = "n", cex = .82, ncol = 3)
  graphics::par(mar = c(3.6, 4.1, 2.1, .8), mgp = c(2.5, .8, 0),
    cex.axis = .9, cex.lab = .95, cex.main = 1)
  for (panel in panels) {
    series <- panel$series
    xx <- range(unlist(lapply(series, function(x) x$year)), finite = TRUE)
    yy <- range(unlist(lapply(series, function(x) x$value)) * 100, finite = TRUE)
    graphics::plot(NA, xlim = xx, ylim = yy, xlab = "Year",
      ylab = "Income share (%)", main = panel$group, xaxt = "n")
    interval <- if (diff(xx) > 40) 10 else 5
    ticks <- seq(ceiling(xx[1L] / interval) * interval,
      floor(xx[2L] / interval) * interval, by = interval)
    graphics::axis(1, at = ticks)
    graphics::abline(v = anchor, col = "grey55", lty = 3)
    for (j in c(3, 5, 4, 2, 1)) {
      x <- series[[j]]
      if (nrow(x)) {
        # Explicit missing years preserve historical gaps in supplied series.
        years <- seq.int(min(x$year), max(x$year))
        graphics::lines(years, x$value[match(years, x$year)] * 100,
          col = colors[j], lty = types[j], lwd = widths[j])
      }
    }
  }
}

state <- dina_finished_publication_state(root)
if (!length(state$active)) stop("No active supplied-series publication.")
sources <- dina_finished_publication_sources(root, names(state$active))
countries <- toupper(commandArgs(trailingOnly = TRUE))
if (length(countries)) {
  unknown <- setdiff(countries, vapply(sources, function(s) s$country, character(1)))
  if (length(unknown)) stop("No active supplied series for: ", paste(unknown, collapse = ", "))
  sources <- Filter(function(s) s$country %in% countries, sources)
}
inputs <- dina_finished_publication_inputs(root, sources)
outdir <- file.path(root, "output", "figures", "updates")
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
for (source in sources) {
  id <- source$id
  candidate <- dina_finished_publication_candidate(root, state$active[[id]]$candidate)
  dina_finished_publication_verify(root, candidate, ids = id)
  item <- candidate$sources[[id]]
  proposed <- utils::read.csv(item$artifact, stringsAsFactors = FALSE)
  raw <- utils::read.csv(candidate$artifacts[[paste0(id, "_raw")]], stringsAsFactors = FALSE)
  original <- dina_finished_publication_original(root, source)
  if (!nrow(original)) stop("Original series is unavailable for ", id)
  panels <- dina_finished_publication_plot_data(inputs$prepared[[id]],
    inputs$internal[[id]], proposed, raw, original)
  path <- file.path(outdir, paste("finished-dina", item$country, item$step,
    item$unit, "latex.pdf", sep = "-"))
  plot_for_latex(path, panels, item$anchor_year)
  message(path)
}
