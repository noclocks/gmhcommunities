remove_ansi <- function(text) {
  gsub("\u001b\\[[0-9;]*[a-zA-Z]", "", text)
}

entrata_logger_init <- function(
    log_file = "dev/sandbox/mitmproxy/entrata.log",
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

entrata_req_log <- function(req) {

  httr2:::check_request(req)

  headers_redacted <- httr2:::headers_redact(
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

entrata_resp_log <- function(resp) {

  httr2:::check_response(resp)

  # logger::log_formatter(formatter = logger::formatter_json)

  method <- resp$method
  url <- resp$url
  status_code <- httr2::resp_status(resp)
  status_desc <- httr2::resp_status_desc(resp)
  status <- paste(status_code, status_desc, sep = " ")
  content_type <- httr2::resp_content_type(resp)
  headers <- httr2::resp_headers(resp)
  header_names <- names(headers)
  headers_redacted <- httr2:::headers_redact(
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
