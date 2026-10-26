source(file.path(repo_root_for_tests, "code/R/country_outputs.R"))

test_that("income partitions preserve prior concepts, units and countries and retire selected years", {
  old <- expand.grid(country=c("ARG","COL"), step=c("nat","pon","pod"), unit=c("esn","ind"), year=2000:2001, stringsAsFactors=FALSE)
  old$value <- seq_len(nrow(old))
  scope <- data.frame(country="ARG",step="pod",unit="esn")
  new <- data.frame(country="ARG",step="pod",unit="esn",year=2000L,value=999)
  result <- country_output_rows(old,new,"ARG",scope)
  retained <- !(old$country=="ARG" & old$step=="pod" & old$unit=="esn")
  expect_equal(result[seq_len(sum(retained)),],old[retained,],check.attributes=FALSE)
  expect_equal(tail(result,1),new,check.attributes=FALSE)
  expect_equal(nrow(country_output_rows(old,new[FALSE,],"ARG",scope)),sum(retained))
  wrong <- new; wrong$step <- "nat"
  expect_error(country_output_rows(old,wrong,"ARG",scope),"unselected income")
  tax <- old[old$step=="nat",setdiff(names(old),"step")]
  expect_equal(nrow(country_output_rows(tax,tax[FALSE,],"ARG",scope)),nrow(tax)-2L)
})

test_that("synthetic publication commits scoped changes together and rolls back on failure", {
  root <- tempfile("income-publication-"); dir.create(root)
  on.exit(unlink(root,recursive=TRUE))
  stage <- file.path(root,"stage")
  rel <- "output/synthetic_microfiles/_smicrofile_long_detailed.csv"
  dir.create(dirname(file.path(stage,rel)),recursive=TRUE)
  dir.create(dirname(file.path(root,rel)),recursive=TRUE)
  old <- data.frame(country=c("ARG","ARG","COL"),step=c("nat","pod","pod"),unit="esn",year=2024,value=1:3)
  write.csv(old,file.path(root,rel),row.names=FALSE)
  new <- old[2,,drop=FALSE]; new$value <- 50
  write.csv(new,file.path(stage,rel),row.names=FALSE)
  write.csv(new[c("country","step","unit")],file.path(stage,"_income_scope.csv"),row.names=FALSE)
  publish <- function(commit=function() NULL) country_output_publish(stage,root,
    list(id="07a-export-synthetic-microfiles",country_scope="selected"),"ARG",c("ARG","COL"),
    file.path(root,"work/publication.rds"),commit=commit)
  expect_error(publish(function() stop("fixture interrupted")),"fixture interrupted")
  expect_equal(read.csv(file.path(root,rel)),old)
  publish()
  result <- read.csv(file.path(root,rel))
  expect_equal(result$value[result$step=="nat"],1)
  expect_equal(result$value[result$country=="COL"],3)
  expect_equal(result$value[result$country=="ARG" & result$step=="pod"],50)
})
