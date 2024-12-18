
#  ------------------------------------------------------------------------
#
# Title : Package Initialization Script
#    By : Jimmy Briggs
#  Date : 2024-12-15
#
#  ------------------------------------------------------------------------

# The `pkg_init.R` script is meant to serve as the primary script for
# initializing the package. It handles initial setup tasks that are never or
# rarely repeated. Subsequent scripts like `pkg_devt.R` and `pkg_tests.R` can
# focus on more targeted, routine tasks performed over time.

# package initialization -------------------------------------------------
usethis::create_package("gmhcommunities")
usethis::use_namespace()
usethis::use_roxygen_md()


# initialize dev/ ---------------------------------------------------------

usethis::use_directory("dev", TRUE)

c(
  "scripts",
  "templates",
  "R",
  "sandbox",
  "docs"
) |>
  purrr::walk(~ fs::dir_create(fs::path("dev", .x)))

usethis::use_git_ignore(c("archive/", "temp/"), directory = "dev")
fs::file_create("dev/README.md")

c(
  "pkg_init.R",
  "pkg_devt.R",
  "pkg_tests.R",
  "pkg_docs.R",
  "pkg_data.R",
  "pkg_build.R",
  "pkg_test.R",
  "pkg_check.R",
  "pkg_install.R",

  "data_prep.R",
  "data_document.R",

  "app_run.R",
  "app_deploy.R",


)
