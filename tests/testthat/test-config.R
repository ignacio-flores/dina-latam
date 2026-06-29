test_that("config loads and renders Stata globals", {
  root <- mini_repo()
  cfg <- dina_config(root)
  expect_equal(cfg$years$first, 2000L)
  expect_equal(cfg$run$units, c("ind", "esn"))

  lines <- dina_render_config_do(cfg)
  expect_true(any(grepl("global all_countries", lines, fixed = TRUE)))
  expect_true(any(grepl("global first_y 2000", lines, fixed = TRUE)))
  expect_true(any(grepl("global bfm_replace \"no\"", lines, fixed = TRUE)))
})

test_that("nested config set helper parses scalars and vectors", {
  x <- list(run = list(debug = FALSE))
  y <- dina_set_nested(x, "run.debug", "true")
  expect_true(y$run$debug)
  z <- dina_set_nested(x, "run.units", "ind,esn,pch")
  expect_equal(z$run$units, c("ind", "esn", "pch"))
})
