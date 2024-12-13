
#  ------------------------------------------------------------------------
#
# Title : Logging Utilities
#    By : Jimmy Briggs
#  Date : 2024-12-08
#
#  ------------------------------------------------------------------------

#' Intialize Entrata Logger
#'
#' @description
#' Initialize the logger for the Entrata API.
#'
#' By default, the logger will write to a file named `"entrata.log"` in the
#' current working directory and leverage the following logging setup:
#'
#' - Formatter: `logger::formatter_glue`
#' - Appender: `logger::appender_tee`
#' - Layout: `logger::layout_glue_colors`
#' - Threshold: `logger::INFO`
#'
#' @param log_file The name of the log file to write to. Default is `"entrata.log"`.
#' @param ... Additional arguments to pass to the logger. Currently not in use.
#'
#' @return NULL
#'
#' @export
#'
#' @importFrom logger log_formatter log_appender log_layout log_threshold
#' @importFrom logger formatter_glue appender_tee layout_glue_colors
#' @importFrom cli cli_alert_success
entrata_logger_init <- function(
    log_file = "entrata.log",
    ...
) {

  logger::log_formatter(logger::formatter_glue)
  logger::log_appender(logger::appender_tee(file = log_file, append = TRUE))
  logger::log_layout(logger::layout_glue_colors)
  logger::log_threshold(logger::INFO)

  cli::cli_alert_success(
    c(
      "Logger initialized with the following setup:\n",
      "Log File: {.path {log_file}}\n",
      "Formatter: {.field logger::formatter_glue}\n",
      "Appender: {.field logger::appender_tee}\n",
      "Layout: {.field logger::layout_glue_colors}\n"
    )
  )

}

#' Entrata Request Logger
#'
#' @description
#' Log the details of an HTTP request to the Entrata API.
#'
#' @param req The `httr2::request()` object.
#'
#' @return NULL
#'
#' @export
#'
#' @importFrom purrr map
#' @importFrom jsonlite toJSON
#' @importFrom logger log_info
entrata_req_log <- function(req) {

  check_request(req)

  headers_redacted <- headers_redact(
    req$headers,
    redact = TRUE,
    to_redact = c("Authorization")
  ) |>
    purrr::map(remove_ansi) |>
    setNames(names(req$headers))

  headers_json <- jsonlite::toJSON(headers_redacted, pretty = TRUE, auto_unbox = TRUE)

  body <- req$body$data
  body_json <- jsonlite::toJSON(body, pretty = TRUE, auto_unbox = TRUE) |>
    substr(1, 1000)

  logger::log_info("HTTP Request: {req$method} {req$url}")
  logger::log_info("Headers:\n{headers_json}")
  if (!is.null(body_json)) {
    logger::log_info("Request Body:\n{body_json}")
  }

  print(req)

}

#' Entrata Response Logger
#'
#' @description
#' Log the details of an HTTP response from the Entrata API.
#'
#' @param resp The `httr2::response()` object.
#'
#' @return NULL
#'
#' @export
#'
#' @importFrom httr2 resp_status resp_status_desc resp_content_type resp_headers resp_body_json
#' @importFrom purrr map
#' @importFrom jsonlite toJSON
#' @importFrom lobstr obj_size
#' @importFrom logger log_info
entrata_resp_log <- function(resp) {

  check_response(resp)

  method <- resp$method
  url <- resp$url
  status_code <- httr2::resp_status(resp)
  status_desc <- httr2::resp_status_desc(resp)
  status <- paste(status_code, status_desc, sep = " ")
  content_type <- httr2::resp_content_type(resp)
  headers <- httr2::resp_headers(resp)
  header_names <- names(headers)
  headers_redacted <- headers_redact(
    headers,
    redact = TRUE,
    to_redact = c("Authorization")
  ) |>
    purrr::map(remove_ansi) |>
    setNames(header_names)
  headers_json <- jsonlite::toJSON(headers_redacted, pretty = TRUE, auto_unbox = TRUE)
  body <- httr2::resp_body_json(resp)
  body_json <- jsonlite::toJSON(body, pretty = TRUE, auto_unbox = TRUE) |>
    substr(1, 1000)
  body_size <- lobstr::obj_size(body)

  logger::log_info("HTTP Response: {method} {url}")
  logger::log_info("HTTP Status: {status}")
  logger::log_info("Content-Type: {content_type}")
  logger::log_info("Headers:\n{headers_json}")
  if (!is.null(body_json)) {
    logger::log_info("Response Body ({body_size}):\n{body_json}")
  }

  print(resp)

}

log_sysinfo <- function() {
  info <- utils::sessionInfo()

  logger::log_trace("Platform: { info$platform }")
  logger::log_trace("Running under: { info$running }")
  logger::log_trace("{ info$R.version$version.string }")
  logger::log_trace("Base packages: { paste(info$basePkgs, collapse = ' ') }")

  # Paste package names and versions
  pasted_names_and_versions <- paste(paste_pkgs_name_with_version(names(info$otherPkgs)), collapse = ", ")
  logger::log_trace("Other attached packages: { pasted_names_and_versions }")

  pasted_names_and_versions <- paste(paste_pkgs_name_with_version(names(info$loadedOnly)), collapse = ", ")
  logger::log_trace("Loaded packages: { pasted_names_and_versions }")
}

paste_pkgs_name_with_version <- function(names) {
  vapply(
    names,
    FUN = function(name) paste(name, utils::packageVersion(name)),
    FUN.VALUE = character(1),
    USE.NAMES = FALSE
  )
}
