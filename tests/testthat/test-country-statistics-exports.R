# Bounded worker checks: temporary fixtures only, no production task execution.
country_export_fixture <- function() {
  root <- tempfile("dina-country-export-")
  dir.create(root)
  for (dir in c("code/Stata/auxiliar", "code/Stata/ado/ineqstats_ado", "code/R",
                "intermediary_data/microdata/bfm_norep_pre", "input_data/wid",
                "output/ineqstats", "output/efftax", "output/synthetic_microfiles")) {
    dir.create(file.path(root, dir), recursive = TRUE)
  }
  root
}
country_export_stage <- function(root, name) {
  path <- file.path(root, name)
  for (dir in c("output/ineqstats/log", "output/synthetic_microfiles", "output/efftax", "output/data_reports")) {
    dir.create(file.path(path, dir), recursive = TRUE)
  }
  path
}
country_export_stata <- function(root, script, task, selected, stage, extra = character()) {
  stata <- Sys.getenv("DINA_STATA_CMD", unset = "/Applications/Stata/StataMP.app/Contents/MacOS/stata-mp")
  skip_if(!file.exists(stata), "Stata unavailable")
  driver <- c(sprintf('capture noisily do "%s"', script), "local worker_rc = _rc",
    'file open result using "worker-rc.txt", write text replace', 'file write result "`worker_rc\'"', "file close result")
  writeLines(driver, file.path(root, "worker.do"))
  unlink(file.path(root, "worker-rc.txt"))
  processx::run(stata, c("-b", "do", "worker.do"), wd = root, timeout = 90000,
    env = c(DINA_COUNTRY_WORKER = task, DINA_RUN_COUNTRIES = selected, DINA_OUTPUT_ROOT = stage, extra),
    error_on_status = FALSE)
  if (!file.exists(file.path(root, "worker-rc.txt"))) stop(paste(readLines(file.path(root, "worker.log")), collapse = "\n"))
  as.integer(readLines(file.path(root, "worker-rc.txt"), warn = FALSE))
}
country_export_copy <- function(root, path) {
  stopifnot(file.copy(file.path(repo_root_for_tests, path), file.path(root, path)))
}

test_that("06a country/step/unit chunks report actual coverage and preserve canonical outputs", {
  skip_if_not_installed("haven"); skip_if_not_installed("openxlsx")
  root <- country_export_fixture(); on.exit(unlink(root, recursive = TRUE))
  country_export_copy(root, "code/Stata/06a-run-ineqstats.do")
  country_export_copy(root, "code/Stata/ado/ineqstats_ado/ineqstats.ado")
  writeLines(c('global all_countries "ARG COL"', 'global all_steps "natinc pon"',
    'global units_06a "esn ind"', 'global bfm_replace "no"', 'global ext "_norep"',
    'global first_y 2000', 'global last_y 2000',
    'global esn_nat_norm "esn_pre_wag esn_pre_cap"'), file.path(root, "code/Stata/auxiliar/aux_general.do"))
  i <- seq_len(2500)
  data <- data.frame(edad = 30, `_weight` = 100, `_fep` = 100,
    esn_pre_wag = 100 + i * .71, esn_pre_cap = 10 + i * .13, check.names = FALSE)
  path <- function(c) file.path(root, "intermediary_data/microdata/bfm_norep_pre", paste0(c, "_2000_bfm_norep_pre.dta"))
  haven::write_dta(data, path("ARG")); haven::write_dta(data[setdiff(names(data), "esn_pre_cap")], path("COL"))
  canonical <- file.path(root, "output/ineqstats/ineqstats_natinc_esn.xlsx")
  writeLines("published sentinel", canonical)
  selected <- country_export_stage(root, "selected")
  expect_equal(country_export_stata(root, "code/Stata/06a-run-ineqstats.do", "06a-run-ineqstats", "ARG", selected,
    c(DINA_RUN_STEPS = "natinc", DINA_RUN_UNITS = "esn")), 0L)
  coverage <- read.csv(file.path(selected, "_coverage.csv"))
  expect_identical(coverage$country, "ARG"); expect_identical(coverage$status, "success")
  book <- file.path(selected, "output/ineqstats/ineqstats_natinc_esn.xlsx")
  expect_setequal(openxlsx::getSheetNames(book), c("ARG2000", "Summary", "Composition", "sginis", "gtheils"))
  expect_equal(nrow(openxlsx::read.xlsx(book, "ARG2000")), 127L)
  expect_identical(readLines(canonical), "published sentinel")
  failed <- country_export_stage(root, "failed")
  expect_equal(country_export_stata(root, "code/Stata/06a-run-ineqstats.do", "06a-run-ineqstats", "COL", failed,
    c(DINA_RUN_STEPS = "natinc", DINA_RUN_UNITS = "esn")), 459L)
  expect_identical(read.csv(file.path(failed, "_coverage.csv"))$status, "failed")
  data$esn_pre_cap <- 0
  haven::write_dta(data, path("COL"))
  excluded <- country_export_stage(root, "excluded")
  expect_equal(country_export_stata(root, "code/Stata/06a-run-ineqstats.do", "06a-run-ineqstats", "COL", excluded,
    c(DINA_RUN_STEPS = "natinc", DINA_RUN_UNITS = "esn")), 0L)
  expect_identical(read.csv(file.path(excluded, "_coverage.csv"))$status, "excluded")
  expect_false(file.exists(file.path(excluded, "output/ineqstats/ineqstats_natinc_esn.xlsx")))
})

test_that("06a respects existing upstream exclusions only for their income steps", {
  skip_if_not_installed("haven"); skip_if_not_installed("openxlsx")
  root <- country_export_fixture(); on.exit(unlink(root, recursive = TRUE))
  country_export_copy(root, "code/Stata/06a-run-ineqstats.do")
  country_export_copy(root, "code/Stata/ado/ineqstats_ado/ineqstats.ado")
  configure <- function(first, last = first) {
    writeLines(c('global all_countries "ARG CRI COL"', 'global all_steps "natinc pon"',
      'global units_06a "esn"', 'global bfm_replace "no"', 'global ext "_norep"',
      sprintf('global first_y %d', first), sprintf('global last_y %d', last),
      sprintf('global esn_%s_norm "esn_pre_wag esn_pre_cap"', c("nat", "pod", "pon", "psp", "raw", "bfm", "res", "upr"))),
      file.path(root, "code/Stata/auxiliar/aux_general.do"))
  }
  i <- seq_len(2500)
  data <- data.frame(edad = 30, `_weight` = 100, `_fep` = 100,
    esn_pre_wag = 100 + i * .71, esn_pre_cap = 10 + i * .13, check.names = FALSE)
  write_survey <- function(country, year, contents = data) {
    haven::write_dta(contents, file.path(root, "intermediary_data/microdata/bfm_norep_pre",
      sprintf("%s_%d_bfm_norep_pre.dta", country, year)))
  }
  run <- function(country, steps, name) {
    stage <- country_export_stage(root, name)
    rc <- country_export_stata(root, "code/Stata/06a-run-ineqstats.do", "06a-run-ineqstats", country, stage,
      c(DINA_RUN_STEPS = steps, DINA_RUN_UNITS = "esn"))
    list(rc = rc, coverage = read.csv(file.path(stage, "_coverage.csv")))
  }
  configure(2016)
  write_survey("ARG", 2016)
  arg <- run("ARG", "natinc pod pon psp raw rescaled uprofits", "arg")
  expect_equal(arg$rc, 0L)
  expect_setequal(arg$coverage$step[arg$coverage$status == "success"], c("natinc", "raw", "rescaled", "uprofits"))
  expect_setequal(arg$coverage$step[arg$coverage$status == "excluded"], c("pod", "pon", "psp"))
  expect_true(all(arg$coverage$reason[arg$coverage$status == "excluded"] == "Existing upstream income-step exclusion"))

  configure(2002, 2005)
  for (year in c(2002, 2003, 2005)) write_survey("CRI", year, data[setdiff(names(data), "esn_pre_cap")])
  cri <- run("CRI", "rescaled uprofits natinc pod pon psp", "cri-excluded")
  expect_equal(cri$rc, 0L)
  accepted <- cri$coverage[cri$coverage$year %in% c(2002, 2003, 2005), ]
  expect_equal(nrow(accepted), 18L)
  expect_true(all(accepted$status == "excluded"))
  expect_true(all(accepted$reason == "Existing upstream income-step exclusion"))

  configure(2002)
  write_survey("CRI", 2002)
  early <- run("CRI", "raw bfm_norep_pre rescaled uprofits", "cri-earlier-steps")
  expect_equal(early$rc, 0L)
  expect_setequal(early$coverage$step[early$coverage$status == "success"], c("raw", "bfm_norep_pre"))
  expect_setequal(early$coverage$step[early$coverage$status == "excluded"], c("rescaled", "uprofits"))
  write_survey("COL", 2002, data[setdiff(names(data), "esn_pre_cap")])
  missing <- run("COL", "natinc", "unexpected-missing")
  expect_equal(missing$rc, 459L)
  expect_identical(missing$coverage$status, "failed")
  expect_identical(missing$coverage$reason, "Missing income component")
})

test_that("07a exports only selected partitions and derives effective-tax cases independently", {
  skip_if_not_installed("haven"); skip_if_not_installed("openxlsx")
  root <- country_export_fixture(); on.exit(unlink(root, recursive = TRUE))
  country_export_copy(root, "code/Stata/07a-export-synthetic-microfiles.do")
  country_export_copy(root, "code/Stata/auxiliar/aux_exclude_ctries.do")
  writeLines(c('global all_countries "ARG COL"', 'global all_steps "natinc"', 'global all_units "esn"',
    'global countries_tax "ARG COL"', 'global first_y 2000', 'global last_y 2001'),
    file.path(root, "code/Stata/auxiliar/aux_general.do"))
  haven::write_dta(data.frame(country = c("ARG", "COL"), countrycode = c("AR", "CO"), year = 2000,
    defl_xxxx = 1, xppp_eur = 1), file.path(root, "input_data/wid/prices_deflator_ppp_eur.dta"))
  p <- c(seq(0,.99,.01), seq(.991,.999,.001), seq(.9991,.9999,.0001), seq(.99991,.99999,.00001))
  summary <- data.frame(country=c("ARG","COL"), year=2000, gini=.4, average=100, adpop=1000,
    b50_sh=.2, m40_sh=.4, t10_sh=.4, t1_sh=.1)
  comp <- data.frame(country=c("ARG","COL"), year=2000, tot_wag=1, b50_wag=1, m40_wag=1, t10_wag=1, t1_wag=1)
  detail <- function(c) data.frame(country=c,year=2000,gini=.4,average=100,p=p,thr=1+p,avg=2+p,
    s=1/127,topavg=3+p,topsh=1-p,b=1,wag=1,mbe=0,wmbe=0,hea=0,edu=0,oex=0)
  openxlsx::write.xlsx(list(Summary=summary,Composition=comp,ARG2000=detail("ARG"),COL2000=detail("COL")),
    file.path(root,"output/ineqstats/ineqstats_natinc_esn.xlsx"))
  tax <- data.frame(ftile_sca=c(seq(0,.99,.01),seq(.991,.999,.001))*100,esn_tax=.2,n=seq_len(109))
  openxlsx::write.xlsx(list(ARG2000=tax,ARG2001=tax,COL2000=tax),file.path(root,"output/efftax/efftax_summary_esn.xlsx"))
  sentinel <- file.path(root,"output/synthetic_microfiles/_smicrofile_long_detailed.csv")
  writeLines("unchanged",sentinel)
  stage <- country_export_stage(root,"selected")
  expect_equal(country_export_stata(root,"code/Stata/07a-export-synthetic-microfiles.do","07a-export-synthetic-microfiles","ARG",stage),0L)
  for (name in c("detailed","grouped","efftax")) {
    result <- read.csv(file.path(stage,"output/synthetic_microfiles",paste0("_smicrofile_long_",name,".csv")))
    expect_setequal(result$country,"ARG")
  }
  taxout <- read.csv(file.path(stage,"output/synthetic_microfiles/_smicrofile_long_efftax.csv"))
  expect_setequal(taxout$year,c(2000L,2001L))
  expect_identical(readLines(sentinel),"unchanged")
  workbooks <- list.files(file.path(stage,"output/synthetic_microfiles"),pattern="xlsx$",full.names=TRUE)
  expect_length(workbooks,1)
  expect_setequal(openxlsx::getSheetNames(workbooks[[1]]),c("Summary-estimates","Summary-composition","inflation-xrates","ARG2000"))
  coverage <- read.csv(file.path(stage,"_coverage.csv"))
  expect_equal(sum(coverage$status=="expected"),3L)
  expect_equal(sum(coverage$status=="success"),3L)
  expect_true(all(coverage$country=="ARG"))

  # Absence from Summary is an explicit retirement intent, not a silent no-op.
  openxlsx::write.xlsx(list(Summary=summary[summary$country=="COL",],Composition=comp[comp$country=="COL",],COL2000=detail("COL")),
    file.path(root,"output/ineqstats/ineqstats_natinc_esn.xlsx"),overwrite=TRUE)
  empty <- country_export_stage(root,"empty")
  expect_equal(country_export_stata(root,"code/Stata/07a-export-synthetic-microfiles.do","07a-export-synthetic-microfiles","ARG",empty),0L)
  intents <- read.csv(file.path(empty,"_empty_workbooks.csv"))
  expect_equal(nrow(intents),4L)
  expect_setequal(intents$sheet,c("Summary-estimates","Summary-composition"))
  expect_equal(nrow(read.csv(file.path(empty,"output/synthetic_microfiles/_smicrofile_long_detailed.csv"))),0L)
  expect_identical(readLines(sentinel),"unchanged")
})

test_that("07c fits selected groups and writes complete group inventory without touching publication", {
  skip_if_not_installed("gpinter"); skip_if_not_installed("readr")
  root <- country_export_fixture(); on.exit(unlink(root, recursive=TRUE))
  country_export_copy(root,"code/R/07c_interpolate_ineqstats.R")
  writeLines('country_run_r_entry <- function(task,args) stopifnot(Sys.getenv("DINA_COUNTRY_WORKER") == task)',file.path(root,"code/R/country_run.R"))
  p <- c(seq(0,.99,.01),seq(.991,.999,.001),seq(.9991,.9999,.0001),seq(.99991,.99999,.00001))
  surv <- 1-p; integral <- surv*(1-log(surv)); nextintegral <- c(integral[-1],0); nextp <- c(p[-1],1)
  one <- data.frame(country="ARG",year=2003L,step="nat",unit="esn",p=p,average=1,
    thr=-log(surv),avg=(integral-nextintegral)/(nextp-p),topavg=1-log(surv),topsh=integral)
  two <- one; two$country <- "PER"; two$year <- 2001L
  input <- rbind(one,two)
  readr::write_csv(input,file.path(root,"output/synthetic_microfiles/_smicrofile_long_detailed.csv"))
  published <- file.path(root,"output/ineqstats/_gpinter_all_topcorrected.csv"); writeLines("published",published)
  stage <- country_export_stage(root,"selected")
  run <- function(selected,stage) processx::run(file.path(R.home("bin"),"Rscript"),"code/R/07c_interpolate_ineqstats.R",wd=root,
    env=c(DINA_COUNTRY_WORKER="07c-interpolate-ineqstats",DINA_RUN_COUNTRIES=selected,DINA_OUTPUT_ROOT=stage),timeout=90000,error_on_status=FALSE)
  result <- run("ARG",stage)
  expect_equal(result$status,0L,info=result$stderr)
  output <- read.csv(file.path(stage,"output/ineqstats/_gpinter_all_topcorrected.csv"))
  expect_equal(nrow(output),127L);expect_setequal(output$country,"ARG")
  expect_setequal(read.csv(file.path(stage,"_groups.csv"))$country,c("ARG","PER"))
  expect_identical(read.csv(file.path(stage,"_coverage.csv"))$status,"success")
  expect_identical(readLines(published),"published")
  complete <- country_export_stage(root,"complete"); full <- run("ARG PER",complete)
  expect_equal(full$status,0L,info=full$stderr)
  fullout <- read.csv(file.path(complete,"output/ineqstats/_gpinter_all_topcorrected.csv"))
  expect_equal(output,fullout[fullout$country=="ARG",],ignore_attr=TRUE)
  input$average[input$country=="ARG"] <- NA_real_
  readr::write_csv(input,file.path(root,"output/synthetic_microfiles/_smicrofile_long_detailed.csv"))
  failed <- country_export_stage(root,"failed"); failure <- run("ARG",failed)
  expect_true(failure$status != 0L)
  expect_false(file.exists(file.path(failed,"output/ineqstats/_gpinter_all_topcorrected.csv")))
  expect_identical(readLines(published),"published")
})

test_that("06b pooled products are guarded independently from selected country figures", {
  script <- readLines(file.path(repo_root_for_tests,"code/Stata/06b-graph-ineqstats-countries-variables.do"))
  expect_true(any(grepl("environment DINA_RUN_SHARED",script,fixed=TRUE)))
  start <- grep("if `dina_shared' {",script,fixed=TRUE)
  finish <- grep("PART 2: COMPOSITION",script,fixed=TRUE)
  expect_length(start,1L);expect_length(finish,1L)
  pooled <- grep("output/figures/ineqstats/variables/",script,fixed=TRUE)
  expect_true(all(pooled>start & pooled<finish))
  expect_true(any(grepl('if "`c\'" == "BRA" & `dina_shared\' {',script,fixed=TRUE)))
})

test_that("07d donor failure names a country missing only at a particular year and fractile", {
  skip_if_not_installed("haven")
  root <- country_export_fixture();on.exit(unlink(root,recursive=TRUE))
  countries <- c("COL","ARG","PER","URY","ECU","CHL","BRA","SLV","MEX","DOM")
  data <- expand.grid(country=countries,year=2000:2001,p=c(0,50000),stringsAsFactors=FALSE)
  data <- data[!(data$country=="COL" & data$year==2001 & data$p==50000),]
  data$bracketavg_norm<-1;data$thr<-1;data$bracketpop<-50000;data$step<-"nat"
  haven::write_dta(data,file.path(root,"donors.dta"))
  lines <- readLines(file.path(repo_root_for_tests,"code/Stata/07d-export-results-to-wid.do"))
  start <- grep("^\\*loop over groups$",lines)
  end <- grep("^\\*merge with population data",lines)
  writeLines(c('clear all',paste0('global all_countries "',paste(c(countries,"CRI"),collapse=" "),'"'),
    'local steps "nat"','use "donors.dta", clear',lines[start:(end-1)]),file.path(root,"donor-check.do"))
  stage<-country_export_stage(root,"selected")
  expect_equal(country_export_stata(root,"donor-check.do","fixture-donors","ARG",stage),9L)
  log<-readLines(file.path(root,"worker.log"),warn=FALSE)
  expect_true(any(grepl("missing_countries",log,fixed=TRUE)))
  expect_true(any(grepl("2001 +50000 +9 +COL",log)))
})

test_that("actual generated Stata configuration keeps the full universe for a selected ECU worker", {
  root <- country_export_fixture();on.exit(unlink(root,recursive=TRUE))
  env <- new.env(parent=globalenv())
  sys.source(file.path(repo_root_for_tests,"code/R/cli/dina_lib.R"),envir=env)
  config <- yaml::read_yaml(file.path(repo_root_for_tests,"config/dina.yml"))
  config$countries <- c("COL","ARG","PER","URY","CRI","ECU","CHL","BRA","SLV","MEX","DOM")
  generated <- file.path(root,"generated-config.do")
  env$dina_render_config_do(config,generated,identity=list(scope="fixture",fingerprint="fixture",baseline_fingerprint="fixture",admin_trust_regions="fixture.csv"))
  country_export_copy(root,"code/Stata/auxiliar/dina_runtime_config.do")
  writeLines(c('clear all','global dina_run_countries "COL"',
    'do "code/Stata/auxiliar/dina_runtime_config.do"',
    'assert "$dina_run_countries" == "ECU"',
    'local total : word count $all_countries','assert `total\' == 11',
    'file open scope using "scope.txt", write text replace',
    'file write scope `"$all_countries"\' _n "$dina_run_countries" _n',
    'file close scope'),file.path(root,"runtime-scope.do"))
  stage <- country_export_stage(root,"selected")
  expect_equal(country_export_stata(root,"runtime-scope.do","fixture-runtime","ECU",stage,c(DINA_CONFIG_DO=generated)),0L)
  actual <- readLines(file.path(root,"scope.txt"),warn=FALSE)
  expect_identical(actual[2],"ECU")
  expect_setequal(strsplit(trimws(gsub('"',"",actual[1]))," +")[[1]],config$countries)
  expect_equal(country_export_stata(root,"runtime-scope.do","fixture-runtime","ZZZ",stage,c(DINA_CONFIG_DO=generated)),198L)
})
