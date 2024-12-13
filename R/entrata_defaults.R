
#  ------------------------------------------------------------------------
#
# Title : Entrata Defaults
#    By : Jimmy Briggs
#  Date : 2024-11-24
#
#  ------------------------------------------------------------------------

get_default_method <- function(endpoint) {

}

#' Get Default Entrata Method Version
#'
#' @param endpoint The Entrata endpoint
#' @param method The Entrata method
#'
#' @return The default version of the Entrata method
#'
#' @export
#'
#' @importFrom dplyr filter pull
#' @importFrom rlang .data .env
get_default_method_version <- function(endpoint, method) {

  validate_entrata_endpoint(endpoint)
  validate_entrata_method(endpoint, method)

  entrata_method_versions_tbl |>
    dplyr::filter(
      .data$endpoint == .env$endpoint,
      .data$method == .env$method
    ) |>
    dplyr::pull("version")

}

get_default_method_params <- function(endpoint, method) {

  validate_entrata_endpoint(endpoint)
  validate_entrata_method(endpoint, method)

  version <- get_default_method_version(endpoint, method)

  params_tbl <- entrata_params_tbl |>
    dplyr::filter(
      .data$endpoint == .env$endpoint,
      .data$method == .env$method
    ) |>
    dplyr::select(
      "parameter",
      "type",
      "required",
      "multiple",
      "description"
    )

}

