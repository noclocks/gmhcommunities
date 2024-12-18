
#  ------------------------------------------------------------------------
#
# Title : Test Options Setup
#    By : Jimmy Briggs
#  Date : 2024-12-13
#
#  ------------------------------------------------------------------------

require(withr)

withr::local_options(
  # httr2 options
  httr2.verbose = TRUE,
  # httptest2 options
  httptest2.verbose = TRUE,
  httptest2.debug.trace = TRUE,
  httptest.debug.trace = TRUE,
  # shiny options
  shiny.fullstacktrace = TRUE,
  shiny.error = function() {
    cat("An error occurred in the Shiny app.\n")
    cat("Please check the logs for more information.\n")
  },
  # warning and partial matching options
  warn = 1,
  warnPartialMatchAttr = TRUE,
  warnPartialMatchDollar = TRUE,
  warnPartialMatchArgs = FALSE
)

# cleanup for CI/CD -------------------------------------------------------

if (env_in_github()) {
  withr::defer(
    {
      file.remove(cfg_decrypted)
      Sys.unsetenv("R_CONFIG_FILE")
    },
    testthat::teardown_env()
  )
}
