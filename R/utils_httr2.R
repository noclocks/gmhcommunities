
#  ------------------------------------------------------------------------
#
# Title : httr2 Utilities
#    By : Jimmy Briggs
#  Date : 2024-11-24
#
#  ------------------------------------------------------------------------


# is_request / is_response ------------------------------------------------

#' @keywords internal
#' @noRd
is_request <- function(req) {
  inherits(req, "httr2_request")
}

#' @keywords internal
#' @noRd
is_response <- function(resp) {
  inherits(resp, "httr2_response")
}

#' @keywords internal
#' @noRd
is_headers <- function(headers) {
  inherits(headers, "httr2_headers")
}

# checks ------------------------------------------------------------------

#' `httr2` Checks
#'
#' @name httr2_checks
#'
#' @description
#' These functions check the validity of HTTP request and response objects as
#' well as HTTP headers by checking the R object's class against the classes
#' defined in the `httr2` package.
#'
#' @param req The HTTP response object to check.
#' @param resp The HTTP response object to check.
#' @param headers The HTTP headers object to check.
#' @inheritParams rlang::args_error_context
#'
#' @return
#' Invisibly returns `NULL` if the request/response/headers is valid, otherwise
#' will throw an error.
#'
#' @seealso [httr2::request()], [httr2::response()], [httr2::req_headers()],
#'   [httr2::resp_headers()]
NULL

#' @rdname httr2_checks
#' @export
#' @importFrom rlang caller_arg caller_env
check_request <- function(req, arg = rlang::caller_arg(req), call = rlang::caller_env()) {

  if (is_request(req)) { return(invisible(NULL)) }

  stop_input_type(
    req,
    "an HTTP request object",
    allow_null = FALSE,
    arg = arg,
    call = call
  )

}

#' @rdname httr2_checks
#' @export
#' @importFrom rlang caller_arg caller_env
check_response <- function(resp, arg = rlang::caller_arg(resp), call = rlang::caller_env()) {

  if (is_response(resp)) { return(invisible(NULL)) }

  stop_input_type(
    resp,
    "an HTTP response object",
    allow_null = FALSE,
    arg = arg,
    call = call
  )

}

# headers -----------------------------------------------------------------

#' @rdname httr2_checks
#' @export
check_headers <- function(headers, arg = rlang::caller_arg(headers), call = rlang::caller_env()) {

  if (is_headers(headers)) { return(invisible(NULL)) }

  stop_input_type(
    headers,
    "an HTTP headers object",
    allow_null = FALSE,
    arg = arg,
    call = call
  )

}
