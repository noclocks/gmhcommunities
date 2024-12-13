
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

# github ------------------------------------------------------------------

env_in_github <- function() {
  fs::file_exists("/github/workflow/event.json")
}


# shiny -------------------------------------------------------------------

env_in_shiny <- function() {
  !is.null(shiny::getDefaultReactiveDomain())
}
