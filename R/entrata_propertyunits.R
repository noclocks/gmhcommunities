
#  ------------------------------------------------------------------------
#
# Title : Entrata /propertyunits Endpoint
#    By : Jimmy Briggs
#  Date : 2024-11-24
#
#  ------------------------------------------------------------------------

#' Call the `/propertyunits` Entrata API Endpoint
#'
#' @description
#' Calls the `/propertyunits` endpoint from the Entrata API.
#'
#' Currently the following "methods" (i.e. operations) are supported:
#'
#' - `getPropertyUnits`
#' - `getAmenities`
#' - `getSpecials`
#' - `getUnitsAvailablityAndPricing`
#' - `getUnitTypes`
#'
#' and unsupported methods are:
#'
#' - `getMitsPropertyUnits`
#' - `sendAmenities`
#' - `sendPropertyUnits`
#' - `updateAmenities`
#'
#' @param method_name The Entrata endpoint method name. Defaults to `"getProperties"`,
#'   but other methods can be used as needed.
#' @param method_params Named list of parameters to pass to the method object
#'   of the request body. Defaults to an empty list, but some methods may
#'   have required method parameters that must be provided.
#' @param request_id (Optional) A unique identifier for the request. Defaults to `NULL`
#'   which will use the current time as an integer for the request ID.
#' @param entrata_config (Optional) An Entrata Configuration list. Defaults to
#'   `get_entrata_config()`.
#' @param verbose (Optional) Logical indicating if verbose output should be printed.
#'   Defaults to `FALSE`.
#' @param progress (Optional) Logical indicating if a progress indicator should be
#'   displayed. Defaults to `FALSE`.
#'
#' @returns
#' A parsed response containing property-unit related data depending on which
#' endpoint method is used.
#'
#' @export
#'
#' @importFrom httr2 req_verbose req_progress req_perform
entrata_propertyunits <- function(
    method_name = c(
      "getPropertyUnits",
      "getAmenities",
      "getSpecials",
      "getUnitsAvailablityAndPricing",
      "getUnitTypes"
    ),
    method_params = list(),
    request_id = NULL,
    entrata_config = get_entrata_config(),
    verbose = FALSE,
    progress = FALSE
) {

  method_name <- rlang::arg_match(method_name)
  validate_entrata_method(endpoint = "propertyunits", method = method_name)
  method_version <- get_default_method_version("propertyunits", method_name)
  validate_entrata_method_params(endpoint = "propertyunits", method = method_name, params = method_params)
  request_id <- request_id %||% as.integer(Sys.time())

  req <- entrata_request(entrata_config) |>
    entrata_req_endpoint("propertyunits") |>
    entrata_req_body(
      id = request_id,
      method = method_name,
      version = method_version,
      params = method_params
    )

  if (verbose) {
    req <- httr2::req_verbose(req)
  }

  if (progress) {
    req <- httr2::req_progress(req)
  }

  resp <- httr2::req_perform(req)

  parser <- switch(
    method_name,
    "getPropertyUnits" = parse_propertyunits_response,
    "getAmenities" = parse_unit_amenities_response,
    "getSpecials" = parse_unit_specials_response,
    "getUnitsAvailablityAndPricing" = parse_units_availablity_and_pricing_response,
    "getUnitTypes" = parse_unit_types_response,
    stop("Method not supported.")
  )

  parser(resp)

}

# getPropertyUnits -----------------------------------------------------------

#' Entrata Property Units: Get Property Units
#'
#' @description
#' Calls the `getPropertyUnits` method from the `/propertyunits` endpoint of the
#' Entrata API.
#'
#' @param property_ids (Optional) Vector of Entrata Property IDs.
#'   If left `NULL` (the default), will return all Entrata properties.
#'   Note that this argument will be merged into a single comma-separated, character
#'   string before being passed to the API request body.
#' @param available_units_only (Optional) Logical indicating if only available units
#'   should be returned. Defaults to `TRUE`. Note that this argument will be coerced
#'   to an integer string before being passed to the API request body (i.e. TRUE -> `"1"`).
#'   If `FALSE`, will return all units.
#' @param use_property_preferences (Optional) Logical indicating if property preferences
#'   should be used. Defaults to `TRUE`. Note that this argument will be coerced to an
#'   integer string before being passed to the API request body (i.e. TRUE -> `"1"`).
#' @param include_disabled_floorplans (Optional) Logical indicating if disabled floorplans
#'   should be included. Defaults to `FALSE`. Note that this argument will be coerced to an
#'   integer string before being passed to the API request body (i.e. TRUE -> `"1"`).
#' @param include_disabled_units (Optional) Logical indicating if disabled units should be
#'   included. Defaults to `FALSE`. Note that this argument will be coerced to an integer
#'   string before being passed to the API request body (i.e. TRUE -> `"1"`).
#' @param request_id (Optional) A unique identifier for the request. If left as `NULL`,
#'   (the default), will set a request ID via `as.integer(Sys.time())`.
#' @param entrata_config (Optional) An Entrata Configuration list. Defaults to
#'   `get_entrata_config()`. See [get_entrata_config()] for more information.
#'
#' @returns
#' A parsed response containing property-unit related data.
#'
#' @export
#'
#' @seealso [entrata_propertyunits()], [parse_propertyunits_response()]
#'
#' @importFrom rlang %||%
#' @importFrom httr2 req_perform
entrata_propertyunits_getPropertyUnits <- function(
    property_ids,
    available_units_only = TRUE,
    use_property_preferences = TRUE,
    include_disabled_floorplans = FALSE,
    include_disabled_units = FALSE,
    request_id = NULL,
    entrata_config = get_entrata_config()
) {

  validate_entrata_config(entrata_config)

  property_ids <- stringr::str_c(property_ids, collapse = ",")
  request_id <- request_id %||% as.integer(Sys.time())

  method_params <- list(
    propertyIds = property_ids,
    availableUnitsOnly = as.integer(available_units_only),
    usePropertyPreferences = as.integer(use_property_preferences),
    includeDisabledFloorplans = as.integer(include_disabled_floorplans),
    includeDisabledUnits = as.integer(include_disabled_units)
  )

  req <- entrata_request(entrata_config) |>
    entrata_req_endpoint("propertyunits") |>
    entrata_req_body(
      id = request_id,
      method = "getPropertyUnits",
      version = get_default_method_version("propertiesunits", "getPropertyUnits"),
      params = method_params
    )

  resp <- httr2::req_perform(req)

  parse_propertyunits_response(resp)

}

parse_propertyunits_response <- function(resp) {

  check_response(resp)

  resp_json <- entrata_resp_body(resp)

  resp_data <- purrr::pluck(resp_json, "response", "result", "properties", "property") |>
    dplyr::bind_rows() |>
    janitor::clean_names()

  unit_pets <- resp_data$pets |>
    purrr::map2_dfr(
      resp_data$property_id,
      function(pet, property_id) {
        pet |>
          dplyr::bind_rows() |>
          janitor::clean_names() |>
          dplyr::mutate(
            property_id = .env$property_id
          )
      }
    ) |>
    dplyr::mutate(
      pet_care = as.logical(pet_care)
    ) |>
    dplyr::select(
      property_id,
      pet_type = type,
      pet_count = count,
      pet_care_available = pet_care,
      pet_description = description
    )

  resp_data |>
    dplyr::transmute(
      property_id = as.integer(property_id),
      property_name = property_name,
      parent_property_id = as.integer(parent_property_id),
      currency_code = currency_code,
      building_count = as.integer(building_count),
      unit_count = as.integer(unit_count),
      property_availability_url = property_availability_url
    ) |>
    dplyr::left_join(
      unit_pets,
      by = "property_id"
    )

}
