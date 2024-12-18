
#  ------------------------------------------------------------------------
#
# Title : Setup httptest2
#    By : Jimmy Briggs
#  Date : 2024-09-19
#
#  ------------------------------------------------------------------------

library(httr2, warn.conflicts = FALSE)
library(httptest2, warn.conflicts = FALSE)

options(
  httptest2.versbose = TRUE,
  httptest.debug.trace = TRUE
)

# mock paths --------------------------------------------------------------

root_mock_path <- testthat::test_path("mocks/gmhcommunities.entrata.com")

# property ids ------------------------------------------------------------

test_prop_ids <- c(
  "739084",
  "641240",
  "676055",
  "952515",
  "518041",
  "518042",
  "833617",
  "1197887",
  "1143679",
  "1311849"
)
