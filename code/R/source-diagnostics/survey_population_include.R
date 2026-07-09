# Compatibility wrapper for the renamed surveys source workflow module.
#
# The workflow now reviews/promotes raw survey sources and builds SurveyPop.dta
# as a derived artifact. New references should source survey_sources_include.R.

survey_population_include_file <- local({
  frames <- sys.frames()
  for (i in rev(seq_along(frames))) {
    file <- frames[[i]]$ofile
    if (is.null(file) || !length(file)) file <- ""
    if (nzchar(file) && identical(basename(file), "survey_population_include.R")) {
      return(normalizePath(file, mustWork = FALSE))
    }
  }
  ""
})

survey_population_include_candidates <- character()
if (nzchar(survey_population_include_file)) {
  survey_population_include_candidates <- c(
    survey_population_include_candidates,
    file.path(dirname(survey_population_include_file), "survey_sources_include.R")
  )
}
survey_population_include_root <- normalizePath(getwd(), mustWork = FALSE)
repeat {
  survey_population_include_candidates <- c(
    survey_population_include_candidates,
    file.path(survey_population_include_root, "code", "R", "source-diagnostics", "survey_sources_include.R")
  )
  parent <- dirname(survey_population_include_root)
  if (identical(parent, survey_population_include_root)) break
  survey_population_include_root <- parent
}
survey_population_include_hits <- survey_population_include_candidates[file.exists(survey_population_include_candidates)]
if (!length(survey_population_include_hits)) {
  stop("Could not locate survey_sources_include.R from survey_population_include.R.", call. = FALSE)
}
survey_population_include_target <- survey_population_include_hits[[1L]]
source(survey_population_include_target, local = environment())
