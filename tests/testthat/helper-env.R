
#  ------------------------------------------------------------------------
#
# Title : Environment Helpers
#    By : Jimmy Briggs
#  Date : 2024-12-08
#
#  ------------------------------------------------------------------------

require(fs)
require(testthat)
require(shiny)


# testthat ----------------------------------------------------------------

env_in_test <- function() {
  !is.null(Sys.getenv("TEST"))
}

# CRAN --------------------------------------------------------------------

env_in_cran <- function() {
  !is.null(Sys.getenv("NOT_CRAN"))
}

# github ------------------------------------------------------------------

env_in_github <- function() {
  fs::file_exists("/github/workflow/event.json")
}

env_in_cicd <- function() {
  env_in_github() || isTRUE(as.logical(Sys.getenv("CI")))
}

skip_on_local <- function() {
  if (!env_in_cicd()) {
    return(invisible())
  }
  testthat::skip("Skipping test on local machine.")
}

# shiny -------------------------------------------------------------------

env_in_shiny <- function() {
  !is.null(Sys.getenv("SHINY_PORT"))
}

env_in_shiny_session <- function() {
  !is.null(shiny::getDefaultReactiveDomain())
}
