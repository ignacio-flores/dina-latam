testthat::test_that("source registries retain UTF-8 text and later entries in the C locale", {
  path <- tempfile(fileext = ".yml")
  on.exit(unlink(path), add = TRUE)
  note <- enc2utf8("2007\u20132016; informaci\u00f3n")
  contents <- paste0("sources:\n  - id: first\n    notes: ", note,
    "\n  - id: later-source\n    notes: approved\n")
  writeBin(charToRaw(enc2utf8(contents)), path)
  expected <- dina_read_yaml(path)
  previous_locale <- Sys.getlocale("LC_CTYPE")
  on.exit(Sys.setlocale("LC_CTYPE", previous_locale), add = TRUE)
  Sys.setlocale("LC_CTYPE", "C")
  testthat::expect_identical(Sys.getlocale("LC_CTYPE"), "C")
  actual <- NULL
  testthat::expect_warning(actual <- dina_read_yaml(path), NA)
  testthat::expect_identical(actual, expected)
  testthat::expect_identical(actual$sources[[1]]$notes, note)
  testthat::expect_identical(actual$sources[[2]]$id, "later-source")
})
