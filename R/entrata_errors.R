
#  ------------------------------------------------------------------------
#
# Title : Entrata Errors
#    By : Jimmy Briggs
#  Date : 2024-11-24
#
#  ------------------------------------------------------------------------

#' Entrata Error Handling
#'
#' @name entrata_errors
#'
#' @description
#' Functions for handling errors from the Entrata API.
#'
#' @details
#' - `entrata_req_error()` alters the Entrata request object with a custom error handler.
#' - `entrata_resp_is_error()` checks if the response from the Entrata API is an error.
#' - `entrata_resp_error_body()` extracts the error body and message from the response.
#'
#' @param req `httr2::request` object.
#' @param resp `httr2::response` object.
#' @inheritParams httr2::req_error
#'
#' @return
#' - `entrata_req_error()` returns the altered request object.
#' - `entrata_resp_is_error()` returns a `logical` indicating if the response is an error.
#' - `entrata_resp_error_body()` returns a `list` with the error code and message.
#'
#' @seealso [httr2::req_error()], [entrata_request()]
NULL

#' @rdname entrata_errors
#' @export
#' @importFrom httr2 req_error
entrata_req_error <- function(
    req,
    is_error = entrata_resp_is_error,
    body = entrata_resp_error_body
) {
  check_request(req)
  httr2::req_error(req, is_error = is_error, body = body)
}

#' @rdname entrata_errors
#' @export
#' @importFrom httr2 resp_body_json
#' @importFrom purrr pluck_exists
entrata_resp_is_error <- function(resp) {
  check_response(resp)
  resp_body <- httr2::resp_body_json(resp)
  if (purrr::pluck_exists(resp_body, "response", "error")) {
    return(TRUE)
  }
  return(FALSE)
}

#' @rdname entrata_errors
#' @export
#' @importFrom purrr pluck
#' @importFrom httr2 resp_body_json
#' @importFrom cli cli_alert_info cli_bullets
entrata_resp_error_body <- function(resp) {
  check_response(resp)
  if (!entrata_resp_is_error(resp)) {
    cli::cli_alert_info("Response is not an error.")
    return(NULL)
  }
  resp_body <- httr2::resp_body_json(resp)
  error_body <- purrr::pluck(resp_body, "response", "error")
  error_code <- purrr::pluck(error_body, "code")
  error_message <- purrr::pluck(error_body, "message")

  cli::cli_bullets(
    c(
      "i" = "Error Status Code: {error_code}",
      "i" = "Error Message: {error_message}"
    )
  )

  return(
    list(
      error_code = error_code,
      error_message = error_message
    )
  )

}
