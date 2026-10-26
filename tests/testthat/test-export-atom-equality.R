test_that("07d preserves exact population masses and still repairs actual decreases", {
  stata <- Sys.which("stata-mp")
  if (!nzchar(stata)) stata <- "/Applications/Stata/StataMP.app/Contents/MacOS/stata-mp"
  skip_if_not(file.exists(stata), "Stata is required for the bounded export ordering check")
  root <- repo_root_for_tests
  code <- readLines(file.path(root, "code/Stata/07d-export-results-to-wid.do"), warn = FALSE)
  start <- which(code == "//check income variables are non-decreasing ")
  end <- which(code == "*renormalize bracketavg")
  expect_length(start, 1L)
  expect_length(end, 1L)
  block <- code[seq.int(start, end - 1L)]
  grid <- c(seq(0, .99, .01), seq(.991, .999, .001),
            seq(.9991, .9999, .0001), seq(.99991, .99999, .00001))
  make_case <- function(country, atom = FALSE) {
    means <- 1 + grid
    thresholds <- c(0, (head(means, -1L) + tail(means, -1L)) / 2)
    if (atom) {
      means[grid >= .49 & grid < .52] <- 1.5
      thresholds <- c(0, (head(means, -1L) + tail(means, -1L)) / 2)
      thresholds[grid >= .49 & grid < .52] <- 1.5
    }
    width <- diff(c(grid, 1))
    average <- sum(means * width)
    # Match the normalized single-precision columns entering the 07d block.
    float32 <- function(x) readBin(writeBin(as.double(x), raw(), size = 4L),
                                  "numeric", n = length(x), size = 4L)
    data.frame(country = country, year = 2000L, step = "nat", p = round(grid * 1e5),
      bracketavg_norm = float32(means / average), bracketavg = means,
      thr = float32(thresholds / average),
      topavg = float32(rev(cumsum(rev(means * width))) / (1 - grid) / average), check2 = 0L)
  }
  input <- rbind(make_case("ATOM", TRUE), make_case("ORDERED"), make_case("DECREASE"))
  reverse_row <- which(input$country == "DECREASE" & input$p == 63000)
  input$bracketavg_norm[reverse_row] <- input$bracketavg_norm[reverse_row - 1L] * .99995
  temp <- tempfile("dina-export-atom-test-")
  dir.create(temp)
  on.exit(unlink(temp, recursive = TRUE), add = TRUE)
  haven::write_dta(input, file.path(temp, "input.dta"))
  script <- c("clear all", "set more off", "use input.dta, clear", block,
              "sort country year p", "save result.dta, replace", "exit, clear")
  writeLines(script, file.path(temp, "test.do"))
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(temp)
  status <- system2(stata, c("-b", "do", "test.do"), stdout = FALSE, stderr = FALSE)
  expect_equal(status, 0)
  if (!file.exists("result.dta")) {
    fail(paste(readLines("test.log", warn = FALSE), collapse = "\n"))
  }
  result <- haven::read_dta("result.dta")
  key <- paste(input$country, input$p)
  result <- result[match(key, paste(result$country, result$p)), ]
  valid <- input$country != "DECREASE"
  for (v in c("thr", "bracketavg_norm", "topavg"))
    expect_equal(as.numeric(result[[v]][valid]), input[[v]][valid], tolerance = 0)
  atom <- input$country == "ATOM" & input$p >= 49000 & input$p < 52000
  expect_equal(as.numeric(result$bracketavg_norm[atom]), input$bracketavg_norm[atom], tolerance = 0)
  expect_equal(as.numeric(result$thr[atom]), input$thr[atom], tolerance = 0)
  expect_equal(length(unique(result$bracketavg_norm[atom])), 1L)
  expect_gt(result$bracketavg_norm[reverse_row], result$bracketavg_norm[reverse_row - 1L])
})
