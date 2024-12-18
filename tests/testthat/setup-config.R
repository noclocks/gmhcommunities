
#  ------------------------------------------------------------------------
#
# Title : Configuration Setup for Testing
#    By : Jimmy Briggs
#  Date : 2024-12-13
#
#  ------------------------------------------------------------------------


# envvars -----------------------------------------------------------------

Sys.setenv("R_CONFIG_ACTIVE" = "default")
Sys.setenv("R_CONFIG_FILE" = testthat::test_path("../../inst/config/config.yml"))
