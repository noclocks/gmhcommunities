
#  ------------------------------------------------------------------------
#
# Title : Entrata Validation
#    By : Jimmy Briggs
#  Date : 2024-11-24
#
#  ------------------------------------------------------------------------

validate_entrata_property <- function(property_id = NULL, property_name = NULL) {

  property_ids <- mem_get_entrata_property_ids()
  property_names <- names(property_ids)

  if (!is.null(property_id) && !is.null(property_name)) {
    cli::cli_abort("Only one of 'property_id' or 'property_name' can be provided.")
  }

  if (!is.null(property_id)) {
    property_id <- rlang::arg_match0(property_id, property_ids)
  }

  if (!is.null(property_name)) {
    property_id <- rlang::arg_match0(property_name, property_names)
  }

  return(invisible(property_id))

}

# request validation ------------------------------------------------------

#' Entrata Request and Response Validation
#'
#' @description
#' Entrata API request and response validation functions.
#'
#' @param req A request object.
#' @param ... Additional arguments passed to methods.
#'
#' @return The request object.
#'
#' @export
#'
#' @importFrom cli cli_abort cli_alert_success
#' @importFrom purrr pluck
#' @importFrom jsonvalidate json_validate
#' @importFrom httr2 resp_body_json
entrata_req_validate <- function(req, ...) {

  endpoint <- get_endpoint(req)
  method <- get_method(req)
  version <- get_method_version(req)
  params <- get_method_params(req)

  validate_entrata_endpoint(endpoint)
  validate_entrata_method(endpoint, method)
  validate_entrata_method_version(endpoint, method, version)
  validate_entrata_method_params(endpoint, method, params)

  return(req)

}

validate_entrata_endpoint <- function(endpoint) {

}

validate_entrata_method <- function(endpoint, method) {

}

validate_entrata_method_version <- function(endpoint, method, version) {

}

validate_entrata_method_params <- function(endpoint, method, params) {

}

# response validation -----------------------------------------------------

#' @rdname entrata_req_validate
#' @export
#' @importFrom httr2 resp_body_json
#' @importFrom jsonvalidate json_validate
#' @importFrom cli cli_abort cli_alert_success
#' @importFrom purrr pluck
entrata_resp_validate <- function(
    resp,
    arg = rlang::caller_arg(resp),
    call = rlang::caller_env()
) {

  # check response
  check_response(resp)

  # extract endpoint, method, and response body
  endpoint <- purrr::pluck(resp, "url") |> basename()
  method <- purrr::pluck(resp, "request", "body", "data", "method", "name")
  body <- resp |> httr2::resp_body_json()

  # convert body to JSON
  body_json <- jsonlite::toJSON(body, auto_unbox = TRUE, pretty = TRUE)

  # get response schema
  resp_schema <- get_entrata_resp_schema(endpoint, method)

  # validate response
  validation <- jsonvalidate::json_validate(
    json = body_json,
    schema = resp_schema,
    engine = "ajv",
    verbose = TRUE,
    greedy = TRUE,
    error = FALSE
  )

  # throw error if validation fails
  if (!validation) {
    errors <- attributes(validation)$errors
    n_errors <- nrow(errors)
    error_detail <- paste(
      sprintf(
        "\t- %s (%s): %s",
        errors$instancePath,
        errors$schemaPath,
        errors$message
      ),
      collapse = "\n"
    )
    error_msg <- sprintf(
      "%s %s validating json:\n%s",
      n_errors,
      ngettext(n_errors, "error", "errors"), error_detail
    )
    cli::cli_abort(
      c(
        "Entrata API Response Validation Error detected for {.arg {arg}}:",
        error_msg
      ),
      call = call
    )
  }

  # inform validation passed
  cli::cli_alert_success(
    c(
      "Entrata API Response Validation Passed for {.arg {arg}}.",
      "No schema errors detected."
    )
  )

  # return request back
  return(resp)

}


# get schemas -------------------------------------------------------------

# internal ----------------------------------------------------------------

#' Get Entrata Request Schema
#'
#' @keywords internal
#' @noRd
#' @importFrom rlang .data .env
#' @importFrom dplyr filter pull
#' @importFrom purrr pluck
get_entrata_req_schema <- function(endpoint, method) {

  validate_entrata_endpoint_method(endpoint, method)

  entrata_schemas |>
    dplyr::filter(
      .data$endpoint == .env$endpoint,
      .data$method == .env$method
    ) |>
    dplyr::pull("request_schema") |>
    purrr::pluck(1)

}

#' Get Entrata Response Schema
#'
#' @keywords internal
#' @noRd
#' @importFrom rlang .data .env
#' @importFrom dplyr filter pull
#' @importFrom purrr pluck
get_entrata_resp_schema <- function(endpoint, method) {

  validate_entrata_method(endpoint, method)

  entrata_schemas |>
    dplyr::filter(
      .data$endpoint == .env$endpoint,
      .data$method == .env$method
    ) |>
    dplyr::pull("response_schema") |>
    purrr::pluck(1)

}

get_json_schema <- function(endpoint, method, type = c("request", "response")) {

  type <- match.arg(type)

  root_path <- system.file("extdata/schemas", package = "gmhleasr")
  endpoint_path <- fs::path(root_path, endpoint)
  schema_file <- fs::path(endpoint_path, paste0(method, ".", type, ".schema.json"))
  schema <- readLines(schema_file) |>
    paste(collapse = "\n")

  return(schema)

}
