
#  ------------------------------------------------------------------------
#
# Title : Entrata Response
#    By : Jimmy Briggs
#  Date : 2024-11-24
#
#  ------------------------------------------------------------------------

#' Entrata Response Body
#'
#' @description
#' Extract the response body from the Entrata API response.
#'
#' @param resp `httr2::response` object.
#'
#' @returns `list` with the response body.
#'
#' @export
#'
#' @importFrom httr2 resp_body_json
#' @importFrom purrr pluck
entrata_resp_body <- function(resp) {

  check_response(resp)

  return(httr2::resp_body_json(resp))

}

entrata_resp_body_parse <- function(resp) {

  check_response(resp)

  body <- entrata_resp_body(resp)
  endpoint <- basename(resp$url)
  method <- purrr::pluck(resp, "request", "body", "data", "method", "name")

  return(resp)

}

#' Entrata Response Success Body
#'
#' @description
#' Extract the body from a successful response.
#'
#' @param resp `httr2::response` object.
#'
#' @returns the response body.
#'
#' @export
#'
#' @importFrom httr2 resp_body_json
entrata_resp_body_success <- function(resp) {
  check_response(resp)

  httr2::resp_body_json(resp) |>
    purrr::pluck("response", "result")

}

#' Entrata Response Error Body
#'
#' @description
#' Extract the error body and message from the response.
#'
#' @param resp `httr2::response` object.
#'
#' @returns the response error body.
#'
#' @export
#'
#' @importFrom httr2 resp_body_json
#' @importFrom purrr pluck
#' @importFrom cli cli_alert_info cli_bullets
entrata_resp_body_error <- function(resp) {
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

  return(error_body)

}

#' Determine if the Entrata Response is Transient
#'
#' @description
#' Check if the response from Entrata is transient based on the status code
#' or headers.
#'
#' @param resp `httr2::response` object.
#'
#' @returns `logical` indicating if the response is transient.
#'
#' @export
#'
#' @importFrom httr2 resp_status resp_header_exists resp_headers
entrata_resp_is_transient <- function(resp) {

  check_response(resp)

  # check for response error
  if (entrata_resp_is_error(resp)) {
    return(TRUE)
  }

  # check for transient status codes
  transient_status_codes <- c(429, 500, 502, 503, 504)
  status_code_transient <- httr2::resp_status(resp) %in% transient_status_codes

  # check for rate limit headers
  rate_limit_info <- entrata_resp_parse_rate_limit_headers(resp)
  rate_limit_transient <- all(sapply(rate_limit_info$remaining, function(x) x == 0))

  # return if any of the conditions are met
  return(
    any(status_code_transient, rate_limit_transient)
  )
}

#' Entrata Response Retry-After
#'
#' @description
#' Get the Retry-After time from the response headers.
#'
#' @param resp `httr2::response` object.
#'
#' @returns `numeric` indicating the time to wait before retrying,
#'   or `NULL` if not found.
#'
#' @export
#'
#' @importFrom httr2 resp_headers
entrata_resp_retry_after <- function(resp) {

  check_response(resp)
  entrata_resp_parse_retry_after_header(resp)

}


# parsers -----------------------------------------------------------------

# https://httpwg.org/specs/rfc9110.html#field.retry-after
# Retry-After: <http-date>
# Retry-After: <delay-seconds>
# Retry-After: Fri, 31 Dec 1999 23:59:59 GMT
# Retry-After: 120

entrata_resp_parse_retry_after_header <- function(resp) {

  if (!httr2::resp_header_exists(resp, "Retry-After")) {
    return(NA)
  }

  header <- httr2::resp_header(resp, header = "Retry-After")

  # check if the header is a date or an integer
  if (grepl("^[0-9]+$", header)) {
    return(
      tryCatch({
        as.numeric(header) + 1
      }, error = function(e) {
        return(NULL)
      })
    )
  } else {
    return(
      tryCatch({
        as.POSIXct(header, format = "%a, %d %b %Y %H:%M:%S", tz = "GMT")
      }, error = function(e) {
        return(NULL)
      })
    )
  }

}

entrata_resp_parse_rate_limit_headers <- function(resp) {

  headers <- .get_rate_limit_headers(resp)

  limit <- .parse_limits_remaining(headers$`x-ratelimit-limit`)
  remaining <- .parse_limits_remaining(headers$`x-ratelimit-remaining`)

  reset <- as.integer(strsplit(headers$`x-ratelimit-reset`, ";")[[1]])

  expires <- if (!is.null(headers$expires)) {
    as.POSIXct(headers$expires, format = "%a, %d %b %Y %H:%M:%S", tz = "GMT")
  } else {
    NA
  }

  list(
    limit = list(
      day = limit[[1]]$value,
      hour = limit[[2]]$value,
      minute = limit[[3]]$value
    ),
    remaining = list(
      day = remaining[[1]]$value,
      hour = remaining[[2]]$value,
      minute = remaining[[3]]$value
    ),
    reset = list(
      day = reset[1],
      hour = reset[2],
      minute = reset[3]
    ),
    expires = expires
  )
}


.get_rate_limit_headers <- function(resp) {
  httr2::resp_headers(
    resp,
    filter = "x-ratelimit-limit|x-ratelimit-remaining|x-ratelimit-reset|expires"
  )
}

.parse_limits_remaining <- function(header) {
  limits <- strsplit(header, ";")[[1]]
  lapply(limits, function(limit) {
    parts <- strsplit(limit, "/")[[1]]
    list(value = as.integer(parts[1]), period = ifelse(length(parts) > 1, parts[2], NA))
  })
}

