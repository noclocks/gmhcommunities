
#  ------------------------------------------------------------------------
#
# Title : Entrata /leases Endpoint
#    By : Jimmy Briggs
#  Date : 2024-11-24
#
#  ------------------------------------------------------------------------

#' Entrata Leases
#'
#' @name entrata_leases
#'
#' @description
#' Builds an API request for Entrata's `/leases` endpoint, `getLeases` method.
#'
#' @details
#' The `getLeases` method retrieves a list of leases for a property.
#'
#' @section Additional Parameters:
#'
#' The following additional parameters can be passed to the API request:
#'
#' - `applicationId` (integer) - The ID of the application.
#' - `customerId` (integer) - The ID of the customer.
#' - `leaseStatusTypeIds` (integer) - The ID of the lease status type.
#' - `leaseIds` (integer) - The ID of the lease.
#' - `scheduledArCodeIds` (integer) - The ID of the scheduled AR code.
#' - `unitNumber` (string) - The unit number.
#' - `buildingName` (string) - The building name.
#' - `moveInDateFrom` (string) - The move-in date from.
#' - `moveInDateTo` (string) - The move-in date to.
#' - `leaseExpiringDateFrom` (string) - The lease expiring date from.
#' - `leaseExpiringDateTo` (string) - The lease expiring date to.
#' - `moveOutDateFrom` (string) - The move-out date from.
#' - `moveOutDateTo` (string) - The move-out date to.
#' - `includeOtherIncomeLeases` (boolean) - Include other income leases.
#' - `residentFriendlyMode` (boolean) - Resident friendly mode.
#' - `includeLeaseHistory` (boolean) - Include lease history.
#' - `includeArTransactions` (boolean) - Include AR transactions.
#'
#' Additionally, the following pagination parameters can be passed as `...` to
#' the function and will be included in the API request:
#'
#' - `pageNumber` (integer) - The page number.
#' - `perPage` (integer) - The number of items per page.
#' - `includePaginationLinks` (boolean) - Include pagination links.
#'
#' @param property_id The ID of the property.
#' @param request_id The ID of the request.
#' @param ... Additional parameters passed to the API endpoint.
#'
#' @return API response
#'
#' @export
#'
#' @importFrom cli cli_abort
#' @importFrom httr2 req_perform
#' @importFrom purrr compact
entrata_leases <- function(
  property_id,
  request_id = NULL,
  ...
) {

  # validate params ---------------------------------------------------------
  if (is.null(property_id)) {
    cli::cli_abort(
      "Please provide a `property_id`."
    )
  }

  # construct method params -------------------------------------------------
  method_params <- construct_entrata_leases_request_method_params(property_id, ...)

  # construct request -------------------------------------------------------
  req <- entrata_request() |>
    entrata_req_endpoint("leases") |>
    entrata_req_body(
      id = request_id,
      method = "getLeases",
      version = get_default_method_version("leases", "getLeases"),
      params = method_params
    ) |>
    entrata_req_error()

  # perform request ---------------------------------------------------------
  resp <- req |> httr2::req_perform()

  entrata_resp_parse_leases(resp)

}

# request body ------------------------------------------------------------

#' @rdname entrata_leases
#' @inheritParams rlang::args_dots_empty
#' @export
#' @importFrom dplyr filter pull
#' @importFrom rlang .data .env
#' @importFrom purrr compact keep
#' @importFrom snakecase to_snake_case to_lower_camel_case
construct_entrata_leases_request_method_params <- function(
  property_id,
  ...
) {

  property_id <- as.integer(property_id)

  dots <- list(...)
  dots_len <- length(dots)

  if (dots_len == 0) {
    return(list(propertyId = property_id))
  } else {
    dot_names <- names(dots)

    method_param_names_camel_case <- entrata_params_tbl |>
      dplyr::filter(method == "getLeases") |>
      dplyr::pull(parameter) |>
      unique()

    method_param_names_snakecase <- snakecase::to_snake_case(method_param_names_camel_case)

    method_param_names <- c(
      method_param_names_camel_case,
      method_param_names_snakecase
    )

    # parse any other parameters from dots
    other_params <- dots |>
      purrr::keep(names(dots) %in% method_param_names)

    names(other_params) <- snakecase::to_lower_camel_case(names(other_params))

    return(
      c(
        propertyId = property_id,
        other_params
      ) |>
        as.list()
    )

  }

}

# parse response ----------------------------------------------------------

#' @rdname entrata_leases
#' @export
#' @importFrom httr2 resp_body_json
#' @importFrom purrr pluck map_dfr map map_chr
#' @importFrom janitor clean_names
#' @importFrom dplyr select left_join
#' @importFrom tidyr unnest unnest_longer
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom stringr str_to_title
#' @importFrom tibble as_tibble
#' @importFrom rlang .data
entrata_resp_parse_leases <- function(resp) {

  check_response(resp)

  resp_json <- httr2::resp_body_json(resp)

  resp_content <- purrr::pluck(
    resp_json,
    "response",
    "result",
    "leases",
    "lease"
  )

  resp_parsed <- resp_content |>
    purrr::map_dfr(function(x) {
      tibble::as_tibble(x)
    }) |>
    jsonlite::toJSON() |>
    jsonlite::fromJSON(flatten = TRUE) |>
    tibble::as_tibble() |>
    janitor::clean_names() |>
    dplyr::select(
      "lease_id" = "id",

      "property_id",
      "property_name",
      "lease_status_type_id",
      "lease_interval_status",
      "occupancy_type_id",
      "occupancy_type",
      "is_month_to_month",
      "lease_interval_id",
      "floor_plan_id",
      "floor_plan_name",
      "space_configuration",
      "payment_allowance_type",
      "unit_id",
      "lease_sub_status",
      "notice_date",
      "transfer_lease_id",
      "transfer_lease_property_id",
      "move_out_date",
      "transfer_date",
      "lease_type",
      "original_lease_start_date",
      "parent_lease_id",
      "parent_unit_number_cache",

      # nested data
      "customers",
      "unit_spaces",
      "scheduled_charges",
      "lease_intervals"
    )

  lease_intervals <- entrata_resp_parse_lease_intervals(resp_parsed)
  lease_customers <- entrata_resp_parse_lease_customers(resp_parsed)
  lease_scheduled_charges <- entrata_resp_parse_lease_scheduled_charges(resp_parsed)
  lease_activities <- entrata_resp_parse_lease_activities(resp_parsed)

  resp_parsed |>
    dplyr::select(
      -c(
        "lease_intervals",
        "customers",
        "scheduled_charges",
        "lease_activities"
      )
    ) |>
    dplyr::left_join(
      lease_intervals,
      by = c("lease_id", "lease_interval_id")
    ) |>
    dplyr::left_join(
      lease_customers,
      by = "lease_id"
    ) |>
    dplyr::left_join(
      lease_scheduled_charges,
      by = "lease_id",
      relationship = "many-to-many"
    ) |>
    dplyr::left_join(
      lease_activities,
      by = "lease_id",
      relationship = "many-to-many"
    )
}

#' @rdname entrata_leases
#' @export
#' @importFrom dplyr select
#' @importFrom tidyr unnest
#' @importFrom jsonlite toJSON fromJSON
entrata_resp_parse_lease_intervals <- function(resp_parsed) {

  hold <- resp_parsed |>
    dplyr::select(
      lease_id,
      lease_intervals
    ) |>
    tidyr::unnest(
      cols = c("lease_intervals")
    ) |>
    jsonlite::toJSON(auto_unbox = TRUE) |>
    jsonlite::fromJSON(flatten = TRUE) |>
    dplyr::select(
      "lease_interval_id" = "id",
      "lease_id",
      "lease_interval_start_date" = "startDate",
      "lease_interval_end_date" = "endDate",
      "lease_interval_type_id" = "leaseIntervalTypeId",
      "lease_interval_type" = "leaseIntervalTypeName",
      "lease_interval_status_type_id" = "leaseIntervalStatusTypeId",
      "lease_interval_status_type" = "leaseIntervalStatusTypeName",
      "lease_approved_on" = "leaseApprovedOn",
      "application_completed_on" = "applicationCompletedOn",
      "application_id" = "applicationId"
    )

}

#' @rdname entrata_leases
#' @export
#' @importFrom dplyr select
#' @importFrom tidyr unnest_longer
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom stringr str_to_title
#' @importFrom purrr map_chr
entrata_resp_parse_lease_customers <- function(resp_parsed) {

  resp_parsed |>
    dplyr::select(
      lease_id,
      customers
    ) |>
    tidyr::unnest_longer(
      customers
    ) |>
    jsonlite::toJSON(auto_unbox = TRUE, pretty = TRUE) |>
    jsonlite::fromJSON(flatten = TRUE) |>
    dplyr::select(
      "lease_id",
      "customer_id" = "customers.id",
      "customer_type" = "customers.customerType",
      "customer_name" = "customers.nameFull",
      "lease_customer_status" = "customers.leaseCustomerStatus",
      "customer_relationship" = "customers.relationshipName",
      "customer_move_in_date" = "customers.moveInDate",
      "customer_move_out_date" = "customers.moveOutDate"
    ) |>
    # move_in_date and and move_out_date are still lists, so we need to
    # extract the first element, or if empty, return NA
    dplyr::mutate(
      "customer_move_in_date" = purrr::map_chr(
        .data$customer_move_in_date,
        ~ if (length(.x) > 0) {
          .x[[1]]
        } else {
          NA_character_
        }
      ),
      "customer_move_out_date" = purrr::map_chr(
        .data$customer_move_out_date,
        ~ if (length(.x) > 0) {
          .x[[1]]
        } else {
          NA_character_
        }
      ),
      # make the customer names titlecase
      "customer_name" = stringr::str_to_title(customer_name)
    )

}

#' @rdname entrata_leases
#' @export
#' @importFrom dplyr select distinct mutate distinct
#' @importFrom tidyr unnest_longer
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom janitor clean_names
#' @importFrom purrr map
#' @importFrom tibble as_tibble
entrata_resp_parse_lease_scheduled_charges <- function(resp_parsed) {

  resp_parsed |>
    dplyr::select(
      lease_id,
      scheduled_charges
    ) |>
    jsonlite::toJSON(auto_unbox = TRUE, pretty = TRUE) |>
    jsonlite::fromJSON(flatten = TRUE) |>
    dplyr::mutate(
      "scheduled_charges" = purrr::map(
        .data$scheduled_charges,
        function(x) {

          if (is.list(x$endDate)) {
            x$endDate <- NA_character_
          }

          jsonlite::toJSON(x, auto_unbox = TRUE, pretty = TRUE) |>
            jsonlite::fromJSON(flatten = TRUE) |>
            tibble::as_tibble()
        }
      )
    ) |>
    tidyr::unnest_longer(
      "scheduled_charges"
    ) |>
    jsonlite::toJSON(auto_unbox = TRUE, pretty = TRUE) |>
    jsonlite::fromJSON(flatten = TRUE) |>
    janitor::clean_names() |>
    dplyr::select(
      "lease_id",
      "scheduled_charges_id",
      "scheduled_charges_lease_interval_id",
      "scheduled_charges_charge_code_id",
      "scheduled_charges_start_date",
      "scheduled_charges_end_date",
      "scheduled_charges_frequency",
      "scheduled_charges_charge_type",
      "scheduled_charges_charge_code",
      "scheduled_charges_amount",
      "scheduled_charges_tax_amount",
      "scheduled_charges_tax_rate"
    ) |>
    dplyr::distinct()
}

#' @rdname entrata_leases
#' @export
#' @importFrom dplyr select
#' @importFrom tidyr unnest_longer
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom janitor clean_names
#' @importFrom purrr map_chr
#' @importFrom stringr str_to_title
entrata_resp_parse_lease_activities <- function(resp_parsed) {

  resp_parsed |>
    dplyr::select(
      lease_id,
      lease_activities
    ) |>
    tidyr::unnest_longer(
      lease_activities
    ) |>
    jsonlite::toJSON(auto_unbox = TRUE, pretty = TRUE) |>
    jsonlite::fromJSON(flatten = TRUE) |>
    janitor::clean_names() |>
    # fix cells with empty lists
    dplyr::mutate_all(
      ~ purrr::map_chr(
        .x,
        ~ if (length(.x) > 0) {
          .x[[1]]
        } else {
          NA_character_
        }
      )
    ) |>
    dplyr::select(
      "lease_id",
      "lease_activity_date" = .data$lease_activities_date,
      "lease_activity_event_type" = .data$lease_activities_event_type,
      "lease_activity_description" = .data$lease_activities_description,
      "lease_activity_comment" = .data$lease_activities_comment
    )

}


# summarize ---------------------------------------------------------------

#' Summarize Entrata Lease Data
#'
#' @description
#' Summarizes the lease data by aggregating the scheduled charges.
#'
#' @param lease_data The lease data to summarize.
#'
#' @return A summarized tibble.
#'
#' @export
#'
#' @importFrom dplyr mutate across group_by summarize ungroup
#' @importFrom rlang .data
#' @importFrom purrr compact
#' @importFrom stringr str_to_title
summarize_leases <- function(lease_data) {

  lease_data |>
    dplyr::mutate(
      dplyr::across(
        c(
          "unit_number_space",
          "scheduled_charges_lease_interval_id"
        ),
        as.integer
      )
    ) |> dplyr::mutate(
      dplyr::across(
        c(
          "scheduled_charges_amount",
          "scheduled_charges_tax_amount",
          "scheduled_charges_tax_rate"
        ),
        as.numeric
      )
    ) |>
    dplyr::group_by(
      "lease_interval_status",
      "unit_number_space",
      "lease_interval_type",
      "lease_interval_start_date",
      "lease_interval_end_date",
      "lease_interval_status_type",
      "lease_approved_on",
      "application_completed_on",
      "customer_name",
      "lease_customer_status",
      "scheduled_charges_lease_interval_id",
      "scheduled_charges_charge_type"
    ) |>
    dplyr::summarize(
      "scheduled_charges_amount" = sum(.data$scheduled_charges_amount, na.rm = TRUE),
      "scheduled_charges_tax_amount" = sum(.data$scheduled_charges_tax_amount, na.rm = TRUE),
      "scheduled_charges_tax_rate" = mean(.data$scheduled_charges_tax_rate, na.rm = TRUE)
    ) |>
    dplyr::ungroup()

}
