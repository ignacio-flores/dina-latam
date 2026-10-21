# The actual lightweight orchestration/publisher with tiny mocked workers only.
country_execution_fixture <- function(config = list(countries=c("ARG","COL"),years=list(first=2000L,last=2001L),
                                                    run=list(steps=c("natinc","pon"),units="esn",debug=FALSE))) {
  root <- tempfile("dina-country-run-"); dir.create(root)
  env <- new.env(parent=globalenv())
  old <- getwd(); setwd(repo_root_for_tests); on.exit(setwd(old))
  sys.source(file.path(repo_root_for_tests,"code/R/country_run.R"),envir=env)
  env$dina_session_config <- function(...) config
  env$dina_load_session <- function(...) list()
  env$dina_relative <- function(path,root) substring(path,nchar(root)+2L)
  env$dina_progress <- function(...) invisible(NULL)
  env$country_run_external <- function(...) character()
  list(root=root,env=env,config=config)
}
country_execution_task <- function(id="06a-run-ineqstats",scope="country") list(id=id,country_scope=scope)
country_execution_book <- function(path,countries,values) {
  dir.create(dirname(path),recursive=TRUE,showWarnings=FALSE)
  sheets <- list(Summary=data.frame(country=countries,year=2000L,average=values),
                 Composition=data.frame(country=countries,year=2000L,tot_wag=1))
  for(i in seq_along(countries)) sheets[[paste0(countries[i],"2000")]] <- data.frame(p=0,avg=values[i])
  openxlsx::write.xlsx(sheets,path)
}
country_execution_coverage <- function(stage,country,step="natinc",unit="esn") {
  write.csv(data.frame(country=country,year=2000L,step=step,unit=unit,status="success"),
            file.path(stage,"_coverage.csv"),row.names=FALSE)
}

test_that("donor progress distinguishes full scope from a restricted run without 05a", {
  f <- country_execution_fixture(); on.exit(unlink(f$root, recursive=TRUE)); e <- f$env
  messages <- character()
  e$dina_progress <- function(progress, format, ...) messages <<- c(messages, sprintf(format, ...))
  e$country_run_donor_snapshot <- function(...) stop("No donor snapshot is expected for these message-only branches")
  context <- list(countries=f$config$countries, record=list(config=f$config, donors=list(),
    chunks=list(`05a-compare-sna-totals--shared`=list(status="succeeded"))))
  task <- country_execution_task("05b-rescale-and-impute")
  e$country_run_donor_check(context, task, f$root, NULL, NULL)
  expect_match(messages[[1L]], "all configured countries are selected")
  expect_false(grepl("has not rebuilt 05a", messages[[1L]], fixed=TRUE))
  context$countries <- "ARG"
  context$record$chunks <- list()
  e$country_run_donor_check(context, task, f$root, NULL, NULL)
  expect_match(messages[[2L]], "this invocation has not rebuilt 05a", fixed=TRUE)
  context$record$donors[[task$id]] <- list(status="unavailable", reason="fixture failure")
  expect_error(e$country_run_donor_check(context, task, f$root, NULL, NULL), "Cannot establish the effect")
})

test_that("standalone invocations retain an explicit benchmark or update configuration", {
  f <- country_execution_fixture(); on.exit(unlink(f$root, recursive=TRUE)); e <- f$env
  e$dina_load_session <- function(update_id="active", root) list(id=update_id)
  e$dina_session_config_override_path <- function(id, root) file.path(root,"output/updates",id,"config.override.yml")
  withr::local_envvar(c(DINA_CONFIG_OVERRIDE_YML=NA, DINA_CONFIG_DO=NA))
  expect_identical(e$country_run_session(f$root)$id,"active")
  Sys.setenv(DINA_CONFIG_OVERRIDE_YML="")
  expect_null(e$country_run_session(f$root))
  Sys.setenv(DINA_CONFIG_OVERRIDE_YML=file.path(f$root,"output/updates/chosen/config.override.yml"))
  expect_identical(e$country_run_session(f$root)$id,"chosen")
  Sys.unsetenv("DINA_CONFIG_OVERRIDE_YML")
  runtime <- file.path(f$root,"runtime.do")
  writeLines('global dina_config_scope "benchmark"',runtime)
  Sys.setenv(DINA_CONFIG_DO=runtime)
  expect_null(e$country_run_session(f$root))
})

test_that("standalone execution records completion and rejects success without outputs", {
  f <- country_execution_fixture(); on.exit(unlink(f$root,recursive=TRUE)); e <- f$env
  task <- country_execution_task("04c-create-raw-postax-units")
  e$country_run_worker <- function(task,root,session,chunk,stage,progress,extra=character()) {
    country_execution_coverage(stage,chunk$country)
    writeLines("changed output",file.path(stage,"output",paste0(chunk$country,"_fixture.txt")))
    list(status="succeeded",log_dir="fixture-log")
  }
  result <- e$country_run_execute(task,f$root,list(),"ARG")
  expect_identical(e$country_run_read(f$root,result$run_id)$status,"succeeded")
  e$country_run_worker <- function(task,root,session,chunk,stage,progress,extra=character()) {
    country_execution_coverage(stage,chunk$country)
    list(status="succeeded",log_dir="fixture-log")
  }
  expect_error(e$country_run_execute(task,f$root,list(),"ARG"),"staged no scientific outputs")
})

test_that("a saved invocation resumes only its exact tasks scope and effective configuration", {
  f <- country_execution_fixture(); on.exit(unlink(f$root,recursive=TRUE))
  e <- f$env; tasks <- list(country_execution_task())
  one <- e$country_run_begin(f$root,tasks,f$config,"arg")
  read <- e$country_run_read(f$root,one$id)
  expect_identical(unlist(read$countries),"ARG")
  resumed <- e$country_run_begin(f$root,tasks,f$config,NULL,resume=one$id)
  expect_identical(resumed$id,one$id);expect_identical(resumed$countries,"ARG")
  expect_error(e$country_run_begin(f$root,tasks,f$config,"COL",resume=one$id),"saved task selection")
  changed <- f$config;changed$years$last <- 2002L
  expect_error(e$country_run_begin(f$root,tasks,changed,"ARG",resume=one$id),"saved task selection")
  expect_error(e$country_run_begin(f$root,list(country_execution_task("07a-export-synthetic-microfiles")),f$config,"ARG",resume=one$id),"saved task selection")
  expect_error(e$country_run_codes("ARG,ZZZ",f$config),"ISO3")
  expect_error(e$country_run_codes("ARG;whoami",f$config),"ISO3")
  expect_identical(e$country_run_codes(NULL,f$config),c("ARG","COL"))
  expect_identical(e$country_run_scope(country_execution_task("07d-export-results-to-wid","shared"),f$config,"ARG"),c("ARG","COL"))
  two <- e$country_run_begin(f$root,tasks,f$config,"COL")
  expect_false(identical(one$id,two$id))
  expect_identical(unlist(e$country_run_read(f$root,one$id)$countries),"ARG")
})

test_that("06a resumes completed chunks and merges only selected workbook contributions", {
  skip_if_not_installed("openxlsx")
  f <- country_execution_fixture(); on.exit(unlink(f$root,recursive=TRUE))
  e <- f$env;task <- country_execution_task()
  for(step in f$config$run$steps) country_execution_book(file.path(f$root,"output/ineqstats",paste0("ineqstats_",step,"_esn.xlsx")),c("ARG","COL"),c(10,20))
  calls <- character();fail <- TRUE
  e$country_run_worker <- function(task,root,session,chunk,stage,progress,extra=character()) {
    calls <<- c(calls,chunk$key)
    if(chunk$step=="pon" && fail) return(list(status="failed",log_dir="fixture-log",failure="deliberate worker error"))
    country_execution_coverage(stage,chunk$country,chunk$step,chunk$unit)
    country_execution_book(file.path(stage,"output/ineqstats",paste0("ineqstats_",chunk$step,"_",chunk$unit,".xlsx")),chunk$country,if(chunk$country=="ARG") 11 else 21)
    list(status="succeeded",log_dir="fixture-log")
  }
  context <- e$country_run_begin(f$root,list(task),f$config,"ARG")
  before_env <- Sys.getenv(c("DINA_COUNTRY_WORKER","DINA_RUN_COUNTRIES","DINA_OUTPUT_ROOT"),unset=NA)
  expect_error(e$country_run_execute(task,f$root,list(),"ARG",run_context=context),"deliberate worker error")
  status <- vapply(context$record$chunks,`[[`,"","status")
  expect_identical(unname(status),c("succeeded","failed"))
  path <- file.path(f$root,"output/ineqstats/ineqstats_natinc_esn.xlsx")
  first <- openxlsx::read.xlsx(path,"Summary")
  expect_equal(first$average[match(c("ARG","COL"),first$country)],c(11,20))
  expect_equal(openxlsx::read.xlsx(path,"COL2000")$avg,20)
  fail <- FALSE
  resumed <- e$country_run_begin(f$root,list(task),f$config,NULL,resume=context$id)
  result <- e$country_run_execute(task,f$root,list(),"ARG",run_context=resumed)
  expect_identical(result$status,"succeeded")
  expect_equal(sum(grepl("--natinc--",calls)),1L)
  expect_equal(sum(grepl("--pon--",calls)),2L)
  expect_identical(Sys.getenv(names(before_env),unset=NA),before_env)
  expect_true(all(vapply(resumed$record$chunks,function(x)x$status=="succeeded",logical(1))))
  allrun <- e$country_run_begin(f$root,list(task),f$config,NULL)
  full <- e$country_run_execute(task,f$root,list(),NULL,run_context=allrun)
  expect_identical(full$status,"succeeded")
  for(step in f$config$run$steps) {
    book <- file.path(f$root,"output/ineqstats",paste0("ineqstats_",step,"_esn.xlsx"))
    data <- openxlsx::read.xlsx(book,"Summary")
    expect_equal(data$average[match(c("ARG","COL"),data$country)],c(11,21))
    expect_false(anyDuplicated(data$country)>0)
  }
})

test_that("a superficially successful worker with missing expected outputs cannot publish", {
  skip_if_not_installed("openxlsx")
  f <- country_execution_fixture();on.exit(unlink(f$root,recursive=TRUE));e<-f$env
  task <- country_execution_task("07a-export-synthetic-microfiles")
  canonical <- file.path(f$root,"output/synthetic_microfiles/_smicrofile_long_detailed.csv")
  dir.create(dirname(canonical),recursive=TRUE);writeLines("published",canonical)
  e$country_run_worker <- function(task,root,session,chunk,stage,progress,extra=character()) {
    write.csv(data.frame(country="ARG",year=2000L,step="nat",unit="esn",status="expected"),file.path(stage,"_coverage.csv"),row.names=FALSE)
    write.csv(data.frame(country="ARG",year=2000,step="nat",unit="esn",p=0),file.path(stage,"output/synthetic_microfiles/_smicrofile_long_detailed.csv"),row.names=FALSE)
    list(status="succeeded",log_dir="fixture-log")
  }
  context<-e$country_run_begin(f$root,list(task),f$config,"ARG")
  expect_error(e$country_run_execute(task,f$root,list(),"ARG",run_context=context),"Incomplete selected outputs")
  expect_identical(readLines(canonical),"published")
  expect_identical(context$record$chunks[[1]]$status,"failed")
})

test_that("07a historical successful dates establish both microfile and effective-tax baselines", {
  skip_if_not_installed("openxlsx")
  f<-country_execution_fixture();on.exit(unlink(f$root,recursive=TRUE));e<-f$env
  task<-country_execution_task("07a-export-synthetic-microfiles")
  context<-e$country_run_begin(f$root,list(task),f$config,"ARG")
  logdir<-file.path(f$root,"output/run_logs/20261014-205615");dir.create(logdir,recursive=TRUE)
  jsonlite::write_json(list(task=task$id,status="succeeded",ended_at="2026-10-14T22:18:31+0200"),file.path(logdir,"run-manifest.json"),auto_unbox=TRUE)
  stage<-e$country_run_stage(f$root,file.path(context$directory,"work/fixture"),task)
  for(folder in c("synthetic_microfiles","efftax")) {
    stem<-if(folder=="efftax") "efftax_esn" else "smicrofile_esn_nat"
    country_execution_book(file.path(f$root,"output",folder,paste0(stem,"_14Oct2026.xlsx")),c("ARG","COL"),c(1,2))
    country_execution_book(file.path(stage,"output",folder,paste0(stem,"_19Oct2026.xlsx")),"ARG",3)
  }
  baselines<-e$country_run_baselines(context,stage)
  expect_setequal(names(baselines),c("smicrofile_esn_nat_19Oct2026.xlsx","efftax_esn_19Oct2026.xlsx"))
  expect_true(all(grepl("14Oct2026.xlsx$",unlist(baselines))))
  country_execution_book(file.path(f$root,"output/synthetic_microfiles/smicrofile_esn_pon_14Oct2026.xlsx"),c("ARG","COL"),c(1,2))
  country_execution_book(file.path(stage,"output/synthetic_microfiles/smicrofile_esn_pon_19Oct2026.xlsx"),"ARG",3)
  expect_true("smicrofile_esn_pon_19Oct2026.xlsx" %in% names(e$country_run_baselines(context,stage)))
  country_execution_book(file.path(f$root,"output/synthetic_microfiles/smicrofile_esn_raw_14Oct2026.xlsx"),c("ARG","COL"),c(1,2))
  write.csv(data.frame(path="output/synthetic_microfiles/smicrofile_esn_raw_19Oct2026.xlsx",sheet="Summary-estimates"),
    file.path(stage,"_empty_workbooks.csv"),row.names=FALSE)
  e$country_output_prepare_empty_workbooks(stage)
  expect_true("smicrofile_esn_raw_19Oct2026.xlsx" %in% names(e$country_run_baselines(context,stage)))
})

test_that("standalone R entry validates scope and delegates without changing process environment", {
  f<-country_execution_fixture();on.exit(unlink(f$root,recursive=TRUE));e<-f$env
  task<-country_execution_task("07c-interpolate-ineqstats")
  e$dina_task_map<-function(...) setNames(list(task),task$id)
  e$quit<-function(status) stop(structure(list(message=paste("exit",status),call=NULL,status=status),class=c("fixture_exit","error","condition")))
  calls<-list()
  e$country_run_execute<-function(task,root,session,countries,...) {
    calls[[length(calls)+1L]]<<-list(countries=countries,root=root)
    list(status="succeeded")
  }
  oldwd<-getwd();setwd(f$root);on.exit(setwd(oldwd),add=TRUE)
  vars<-c("DINA_COUNTRY_WORKER","DINA_RUN_COUNTRIES","DINA_OUTPUT_ROOT","DINA_RUN_STEPS","DINA_RUN_UNITS")
  before<-Sys.getenv(vars,unset=NA)
  expect_error(e$country_run_r_entry(task$id,c("--countries","XXX")),"ISO3")
  expect_length(calls,0)
  expect_error(e$country_run_r_entry(task$id,c("--countries","arg,col")),class="fixture_exit")
  expect_identical(e$country_run_codes(calls[[1]]$countries,f$config),c("ARG","COL"))
  expect_error(e$country_run_r_entry(task$id,character()),class="fixture_exit")
  expect_null(calls[[2]]$countries)
  expect_error(e$country_run_r_entry(task$id,c("--countries=ARG","--countries=COL")),"once")
  expect_error(e$country_run_r_entry(task$id,c("--countries")),"Missing")
  expect_identical(Sys.getenv(vars,unset=NA),before)
})

test_that("07c keeps successful private chunks and retries only a failed country before assembly", {
  f<-country_execution_fixture();on.exit(unlink(f$root,recursive=TRUE));e<-f$env
  task<-country_execution_task("07c-interpolate-ineqstats")
  p<-c(seq(0,.99,.01),seq(.991,.999,.001),seq(.9991,.9999,.0001),seq(.99991,.99999,.00001))
  groups<-data.frame(country=c("ARG","COL"),year=2000L,step="nat",unit="esn")
  path<-file.path(f$root,"output/ineqstats/_gpinter_all_topcorrected.csv")
  dir.create(dirname(path),recursive=TRUE);writeLines("previous generation",path)
  calls<-character(); fail<-TRUE
  e$country_run_worker<-function(task,root,session,chunk,stage,progress,extra=character()) {
    calls<<-c(calls,chunk$country)
    if(chunk$country=="COL" && fail) return(list(status="failed",log_dir="fixture-fit",failure="fit failure"))
    country_execution_coverage(stage,chunk$country,"nat")
    write.csv(groups,file.path(stage,"_groups.csv"),row.names=FALSE)
    write.csv(data.frame(country=chunk$country,year=2000L,step="nat",unit="esn",p=p,thr=1+p,avg=2+p,bckt_sh=1/127,topavg=3+p,topsh=1-p),file.path(stage,"output/ineqstats/_gpinter_all_topcorrected.csv"),row.names=FALSE)
    list(status="succeeded",log_dir="fixture-fit")
  }
  context<-e$country_run_begin(f$root,list(task),f$config,NULL)
  expect_error(e$country_run_execute(task,f$root,list(),NULL,run_context=context),"fit failure")
  expect_identical(readLines(path),"previous generation")
  expect_identical(vapply(context$record$chunks,`[[`,"","status"),setNames(c("cached","failed"),names(context$record$chunks)))
  # A real previous generation would be a valid CSV; this fixture initially
  # uses a sentinel to prove no partial publication, then supplies empty baseline.
  unlink(path);fail<-FALSE
  resumed<-e$country_run_begin(f$root,list(task),f$config,NULL,resume=context$id)
  result<-e$country_run_execute(task,f$root,list(),NULL,run_context=resumed)
  expect_identical(result$status,"succeeded")
  expect_equal(calls,c("ARG","COL","COL"))
  actual<-read.csv(path)
  expect_equal(nrow(actual),254L);expect_setequal(actual$country,c("ARG","COL"))
  expect_true(all(vapply(resumed$record$chunks,function(x)x$status=="succeeded",logical(1))))
})
