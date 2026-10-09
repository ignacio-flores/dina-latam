test_that("Admin inclusion directory hashes do not depend on the launching locale", {
  skip_if_not_installed("digest")
  original_collate <- Sys.getlocale("LC_COLLATE")
  available <- suppressWarnings(Sys.setlocale("LC_COLLATE", "C.UTF-8"))
  on.exit(suppressWarnings(Sys.setlocale("LC_COLLATE", original_collate)), add = TRUE)
  if (is.na(available) || !nzchar(available)) skip("C.UTF-8 locale is unavailable")
  suppressWarnings(Sys.setlocale("LC_COLLATE", "C"))

  source(file.path(repo_root_for_tests, "code", "R", "source-diagnostics", "admin_pit_include.R"), local = TRUE)
  root <- mini_repo()
  directory <- file.path(root, "input_data", "admin_data", "COL", "publisher")
  dir.create(directory, recursive = TRUE)
  for (name in c("1_Cuantiles.xlsx", "10_Cuantiles.xlsx", "2_Cuantiles.xlsx")) {
    writeLines(name, file.path(directory, name))
  }
  expected <- admin_pit_include_hash_path(directory)
  withr::with_envvar(c(LC_ALL = "C"), {
    expect_equal(dina_admin_pit_hash_path(directory), expected)
    expect_equal(admin_pit_include_hash_path(directory), expected)
    expect_equal(Sys.getenv("LC_ALL"), "C")
    expect_equal(Sys.getlocale("LC_COLLATE"), "C")
  })
})
