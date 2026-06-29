test_env <- new.env(parent = globalenv())
test_env$.tests <- list(total = 0L, failed = 0L)

test_env$test_that <- function(desc, code) {
  test_env$.tests$total <- test_env$.tests$total + 1L
  tryCatch(
    {
      force(code)
      cat(sprintf("ok - %s\n", desc))
    },
    error = function(e) {
      test_env$.tests$failed <- test_env$.tests$failed + 1L
      cat(sprintf("not ok - %s\n  %s\n", desc, conditionMessage(e)))
    }
  )
}

test_env$expect_equal <- function(object, expected) {
  if (!isTRUE(all.equal(object, expected, check.attributes = FALSE))) {
    stop(sprintf("Expected %s, got %s", paste(capture.output(str(expected)), collapse = " "), paste(capture.output(str(object)), collapse = " ")), call. = FALSE)
  }
}

test_env$expect_true <- function(object) {
  if (!isTRUE(object)) stop("Expected TRUE", call. = FALSE)
}

test_env$expect_false <- function(object) {
  if (!identical(object, FALSE)) stop("Expected FALSE", call. = FALSE)
}

test_env$expect_match <- function(object, regexp) {
  if (!grepl(regexp, object)) stop(sprintf("Expected `%s` to match `%s`", object, regexp), call. = FALSE)
}

test_env$expect_error <- function(object, regexp = NULL) {
  error <- tryCatch({
    force(object)
    NULL
  }, error = function(e) e)
  if (is.null(error)) {
    stop("Expected an error", call. = FALSE)
  }
  if (!is.null(regexp) && !grepl(regexp, conditionMessage(error))) {
    stop(sprintf("Expected error `%s` to match `%s`", conditionMessage(error), regexp), call. = FALSE)
  }
}

source("tests/testthat/helper-fixtures.R", local = test_env)
for (file in sort(list.files("tests/testthat", pattern = "^test-.*\\.R$", full.names = TRUE))) {
  source(file, local = test_env)
}

cat(sprintf("\n%d tests, %d failures\n", test_env$.tests$total, test_env$.tests$failed))
if (test_env$.tests$failed > 0L) quit(status = 1)
