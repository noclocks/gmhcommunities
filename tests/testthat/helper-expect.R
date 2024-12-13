
#  ------------------------------------------------------------------------
#
# Title : Test Expectation Helpers
#    By : Jimmy Briggs
#  Date : 2024-11-29
#
#  ------------------------------------------------------------------------

require(testthat)
require(rlang)
require(httr2)
require(tibble)
require(dplyr)


# httr2 -------------------------------------------------------------------

expect_httr2_request <- function(req) {
  testthat::expect_s3_class(req, "httr2_request")
}

expect_httr2_response <- function(resp) {
  testthat::expect_s3_class(resp, "httr2_response")
}

expect_httr2_headers <- function(headers) {
  testthat::expect_s3_class(headers, "httr2_headers")
}

expect_httr2_resp_headers <- function(resp) {
  check_response(resp)
  headers <- httr2:::resp_headers(resp)
  expect_httr2_headers(headers)
}


# database connections ----------------------------------------------------

expect_conn <- function(x) {
  testthat::expect_s3_class(x, "PqConnection")
}

expect_pool <- function(x) {
  testthat::expect_s3_class(x, "Pool")
}

expect_rstudio_conn <- function(x) {
  testthat::expect_s3_class(x, "connConnection")
}

# data related ------------------------------------------------------------

expect_df <- function(x) {
  testthat::expect_s3_class(x, "data.frame")
}

expect_tibble <- function(x) {
  testthat::expect_s3_class(x, "tbl_df")
}

expect_colnames <- function(x, colnames) {

  if (!is.data.frame(x)) {
    rlang::abort("x must be a data frame")
  }

  if (!is.character(colnames)) {
    rlang::abort("colnames must be a character vector")
  }

  colnames_expected <- colnames
  colnames_actual <- colnames(x)

  testthat::expect_true(
    all(colnames_expected %in% colnames_actual)
  )

}

expect_no_duplicates <- function(x) {

  if (!is.data.frame(x)) {
    rlang::abort("x must be a data frame")
  }

  row_num <- nrow(x)
  row_num_distinct <- nrow(dplyr::distinct(x))

  testthat::expect_equal(row_num, row_num_distinct)
}




