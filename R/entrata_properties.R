
#  ------------------------------------------------------------------------
#
# Title : Entrata /properties Endpoint
#    By : Jimmy Briggs
#  Date : 2024-11-24
#
#  ------------------------------------------------------------------------

#' Call the `/properties` Entrata API Endpoint
#'
#' @description
#' Calls the `/properties` endpoint from the Entrata API.
#'
#' Currently the following "methods" (i.e. operations) are supported:
#'
#' - `getProperties`
#' - `getFloorPlans`
#'
#' @param request_id (Optional) A unique identifier for the request. Defaults to `NULL`
#'   which will use the current time as an integer for the request ID.
#' @param entrata_config (Optional) An Entrata Configuration list. Defaults to
#'   `get_entrata_config()`.
#' @param method_name The Entrata endpoint method name. Defaults to `"getProperties"`,
#'   but other methods can be used as needed.
#' @param method_params Named list of parameters to pass to the method object
#'   of the request body. Defaults to an empty list, but some methods may
#'   have required method parameters that must be provided.
#' @param verbose (Optional) Logical indicating if verbose output should be printed.
#'   Defaults to `FALSE`.
#' @param progress (Optional) Logical indicating if a progress indicator should be
#'   displayed. Defaults to `FALSE`.
#'
#' @returns A parsed response containing property related data depending on which
#'   endpoint method is used.
#'
#' @export
#'
#' @importFrom httr2 req_verbose req_progress req_perform
entrata_properties <- function(
  method_name = c("getProperties", "getFloorPlans", "getWebsites"),
  method_params = list(),
  request_id = NULL,
  entrata_config = get_entrata_config(),
  verbose = FALSE,
  progress = FALSE
) {

  method_name <- rlang::arg_match(method_name, c("getProperties", "getFloorPlans", "getWebsites"))
  validate_entrata_method(endpoint = "properties", method = method_name)
  method_version <- get_default_method_version("properties", method_name)
  validate_entrata_method_params(endpoint = "properties", method = method_name, params = method_params)
  request_id <- request_id %||% as.integer(Sys.time())

  req <- entrata_request(entrata_config) |>
    entrata_req_endpoint("properties") |>
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
    "getProperties" = parse_properties_response,
    "getFloorPlans" = parse_floorplans_response,
    "getWebsites" = parse_websites_response,
    stop("Method not supported.")
  )

  parser(resp)

}

# getProperties -----------------------------------------------------------

#' Entrata Properties: Get Properties
#'
#' @description
#' Calls the `getProperties` method from the `/properties` endpoint of the Entrata API.
#'
#' @param property_ids (Optional) Character vector of Entrata Property IDs.
#'   If left `NULL` (the default), will return all Entrata properties.
#'   Note that this argument will be merged into a single comma-separated, character
#'   string before being passed to the API request body.
#' @param property_lookup_code (Optional) Character vector of Entrata Property Lookup Codes.
#'   Not used in this package.
#' @param show_all_status (Optional) Logical indicating if all property statuses should be shown.
#'   Defaults to `TRUE`. Note that this argument will be coerced to an integer string before
#'   being passed to the API request body (i.e. TRUE -> `"1"`).
#' @param request_id (Optional) A unique identifier for the request. If left as `NULL`,
#'   (the default), will set a request ID via `as.integer(Sys.time())`.
#' @param entrata_config (Optional) An Entrata Configuration list. Defaults to
#'   `get_entrata_config()`. See [get_entrata_config()] for more information.
#'
#' @returns A parsed response containing property related datasets.
#'
#' @export
#'
#' @seealso [entrata_properties()]
#'
#' @importFrom rlang %||%
#' @importFrom httr2 req_perform
entrata_properties_getProperties <- function(
  property_ids = NULL,
  property_lookup_code = NULL,
  show_all_status = TRUE,
  request_id = NULL,
  entrata_config = get_entrata_config()
) {

  validate_entrata_config(entrata_config)

  request_id <- request_id %||% as.integer(Sys.time())

  req <- entrata_request(entrata_config) |>
    entrata_req_endpoint("properties") |>
    entrata_req_body(
      id = req_id,
      method = "getProperties",
      version = get_default_method_version("properties", "getProperties"),
      params = list(
        propertyIds = property_ids,
        propertyLookupCode = property_lookup_code,,
        showAllStatus = as.integer(show_all_status)
      )
    )

  resp <- httr2::req_perform(req)

  parse_properties_response(resp)

}

# getFloorPlans -----------------------------------------------------------

#' Entrata Properties: Get Floor Plans
#'
#' @description
#' Calls the `getFloorPlans` method from the `/properties` endpoint of the Entrata API.
#'
#' @param property_id (Required) The Entrata Property ID to retrieve the floor plans
#'   for. Must be a valid Entrata property ID.
#' @param floorplan_ids (Optional) Character or Integer vector of Entrata Floor Plan IDs.
#'   If left `NULL` (the default), will return all floor plans for the specified property.
#'   Note that this argument will be merged into a single comma-separated, character
#'   string before being passed to the API request body.
#' @param use_property_preferences (Optional) Logical indicating if property preferences
#'   should be used. Defaults to `TRUE`. Note that this argument will be coerced to an
#'   integer string before being passed to the API request body (i.e. TRUE -> `"1"`).
#' @param include_disabled_floorplans (Optional) Logical indicating if disabled floor plans
#'   should be included. Defaults to `FALSE`. Note that this argument will be coerced to an
#'   integer string before being passed to the API request body (i.e. FALSE -> `"0"`).
#' @param request_id (Optional) A unique identifier for the request. If left as `NULL`,
#'   (the default), will set a request ID via `as.integer(Sys.time())`.
#' @param entrata_config (Optional) An Entrata Configuration list. Defaults to
#'   `get_entrata_config()`. See [get_entrata_config()] for more information.
#'
#' @returns A parsed response containing floor plan data for the specified property.
#'
#' @export
#'
#' @importFrom httr2 req_perform
#' @importFrom rlang %||%
entrata_properties_getFloorPlans <- function(
  property_id,
  floorplan_ids = NULL,
  use_property_preferences = NULL,
  include_disabled_floorplans = NULL,
  request_id = NULL,
  entrata_config = get_entrata_config()
) {

  validate_entrata_config(entrata_config)

  req_id <- request_id %||% as.integer(Sys.time())

  req <- entrata_request(entrata_config) |>
    entrata_req_endpoint("properties") |>
    entrata_req_body(
      id = req_id,
      method = "getFloorPlans",
      version = get_default_method_version("properties", "getFloorPlans"),
      params = list(
        propertyId = property_id,
        usePropertyPreferences = as.integer(use_property_preferences),
        includeDisabledFloorplans = as.integer(include_disabled_floorplans)
      )
    )

  resp <- httr2::req_perform(req)

  parse_floorplans_response(resp)

}


# getWebsites -------------------------------------------------------------


#' Entrata Properties: Get Websites
#'
#' @description
#' Calls the `getWebsites` method from the `/properties` endpoint of the Entrata API.
#'
#' @param property_ids (Required) Vector of Entrata Property IDs.
#' @param request_id (Optional) A unique identifier for the request. If left as `NULL`,
#'   (the default), will set a request ID via `as.integer(Sys.time())`.
#' @param entrata_config (Optional) An Entrata Configuration list. Defaults to
#'   `get_entrata_config()`. See [get_entrata_config()] for more information.
#'
#' @returns
#' A parsed response containing website data for the specified properties.
#'
#' @export
#'
#' @importFrom httr2 req_perform
#' @importFrom rlang %||%
#' @importFrom dplyr select mutate
#' @importFrom janitor clean_names
entrata_properties_getWebsites <- function(
  property_ids,
  request_id = NULL,
  entrata_config = get_entrata_config()
) {

  validate_entrata_config(entrata_config)

  property_ids <- stringr::str_c(property_ids, collapse = ",")
  req_id <- request_id %||% as.integer(Sys.time())

  req <- entrata_request(entrata_config) |>
    entrata_req_endpoint("properties") |>
    entrata_req_body(
      id = req_id,
      method = "getWebsites",
      version = get_default_method_version("properties", "getWebsites"),
      params = list(
        propertyIds = property_ids
      )
    ) |>
    entrata_req_error()

  resp <- httr2::req_perform(req)

  parse_websites_response(resp)

}

parse_websites_response <- function(resp) {

  check_response(resp)

  resp_json <- entrata_resp_body(resp)

  resp_json |>
    jsonlite::toJSON(auto_unbox = TRUE, pretty = TRUE) |>
    jsonlite::fromJSON(flatten = TRUE) |>
    purrr::pluck("response", "result", "websites", "website") |>
    tidyr::unnest(cols = c("websiteProperties.websiteProperty")) |>
    janitor::clean_names() |>
    dplyr::mutate(
      property_id = as.integer(property_id)
    ) |>
    dplyr::select(
      property_id = property_id,
      website_id = id,
      property_name = property_name,
      website_name = name,
      website_subdomain = subdomain
    )

}

entrata_properties_getPropertyAddOns <- function(
  property_id,
  request_id = NULL,
  entrata_config = get_entrata_config()
) {

  validate_entrata_config(entrata_config)

  req_id <- request_id %||% as.integer(Sys.time())

  req <- entrata_request(entrata_config) |>
    entrata_req_endpoint("properties") |>
    entrata_req_body(
      id = req_id,
      method = "getPropertyAddOns",
      version = get_default_method_version("properties", "getPropertyAddOns"),
      params = list(
        propertyId = property_id
      )
    )

  resp <- httr2::req_perform(req)

  parse_property_addons_response(resp)

}

parse_property_addons_response <- function(resp) {

  check_response(resp)

  # get property id from request
  req_property_id <- purrr::pluck(resp, "request", "body", "data", "method", "params", "propertyId")

  resp_json <- entrata_resp_body(resp)

  resp_data <- resp_json |>
    purrr::pluck("response", "result", "addOns", "addon") |>
    dplyr::bind_rows() |>
    janitor::clean_names()

  addons_rates <- resp_data$rates |>
    purrr::map2_dfr(
      resp_data$id,
      function(rate, addon_id) {
        rate |>
          purrr::pluck(1) |>
          tibble::as_tibble() |>
          janitor::clean_names() |>
          dplyr::mutate(
            addon_id = .env$addon_id
          )
      }
    ) |>
    dplyr::mutate(
      amount = as.numeric(amount)
    ) |>
    dplyr::select(
      addon_id,
      charge_code_id,
      charge_timing_id,
      amount
    )

  resp_data |>
    dplyr::transmute(
      property_id = as.integer(req_property_id),
      addon_id = as.integer(id),
      addon_name = name,
      addon_type_id = as.integer(add_on_type_id),
      addon_category_id = as.integer(add_on_category_id),
      addon_group_id = as.integer(add_on_group_id),
      availablility = availability,
      available_on = lubridate::mdy(add_on_available_on),
      is_inventory = as.logical(is_inventory),
      is_visible_on_website = as.logical(is_web_visible),
      reservation_start_date = lubridate::mdy(reservation_start_date),
      reservation_end_date = lubridate::mdy(reservation_end_date)
    ) |>
    dplyr::left_join(
      addons_rates,
      by = "addon_id"
    ) |>
    dplyr::select(
      property_id,
      addon_id,
      addon_type_id,
      addon_category_id,
      addon_group_id,
      addon_name,
      addon_availablility = availablility,
      available_on,
      is_inventory,
      is_visible_on_website,
      reservation_start_date,
      reservation_end_date,
      addon_rate_charge_code_id = charge_code_id,
      addon_rate_charge_timing_id = charge_timing_id,
      addon_rate_amount = amount
    )

}

get_entrata_property_id <- function(property_name, entrata_config = get_entrata_config()) {

  prop_ids <- get_entrata_property_ids(entrata_config)
  prop_id <- prop_ids[property_name]

  if (is.null(prop_id)) {
    cli::cli_abort("Property Name not found in Entrata properties.")
  }

  return(prop_id)

}


get_entrata_property_ids <- function(entrata_config = get_entrata_config()) {

  req <- entrata_request(
    entrata_config = entrata_config
  ) |>
    entrata_req_endpoint("properties") |>
    entrata_req_body(
      id = 15L,
      method = "getProperties",
      version = get_default_method_version("properties", "getProperties"),
      params = list()
    )

  resp <- httr2::req_perform(req)
  resp_content <- resp |>
    entrata_resp_body() |>
    purrr::pluck("response", "result", "PhysicalProperty", "Property")

  prop_names <- resp_content |>
    purrr::map_chr(
      ~ as.character(purrr::pluck(.x, "MarketingName"))
    )

  resp |>
    entrata_resp_body() |>
    purrr::pluck("response", "result", "PhysicalProperty", "Property") |>
    purrr::map_chr(
      ~ as.character(purrr::pluck(.x, "PropertyID"))
    ) |>
    setNames(prop_names)

}

mem_get_entrata_property_ids <- memoise::memoise(get_entrata_property_ids)


