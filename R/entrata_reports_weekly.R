
#  ------------------------------------------------------------------------
#
# Title : Entrata Weekly (lease_execution_(applicant)) Report
#    By : Jimmy Briggs
#  Date : 2024-11-24
#
#  ------------------------------------------------------------------------

#' Retrieves the summary and details tables for the `pre_lease` report from the
#' Entrata API. The function dynamically constructs the API request and parses
#' the response to return clean summary and details tables.
#'
#' @param property_ids A vector of property IDs to include in the report. Defaults
#'   to all properties.
#' @param report_date The date to use for the report. Defaults to the current date.
#'   This date will be used to derive the target date for the pre-lease season, which
#'   is always `9/1` of the current year or the next year, depending on the current
#'   date's month.
#' @param summarize_by A character value representing the `summarize_by` report filter
#'   parameter for the pre_lease report. Defaults to `"property"`. Other options are
#'   `"unit_type"`, `"floorplan_name"`, or `"do_not_summarize"`.
#' @param group_by A character value representing the `group_by` report filter parameter
#'   for the pre_lease report. Defaults to `"do_not_group"`. Other options are `"unit_type"`,
#'   `"floorplan_name"`, or `"lease_term"`, or `"do_not_group"`.
#' @param consider_pre_leased_on A numeric value representing the `consider_pre_leased_on`
#'   report filter parameter for the pre_lease report. Defaults to `32` which represents
#'   `"Lease:Partially Completed"`. Other options are `33`, `34`, `41`, `42`, `43`, and `44`.
#' @param ... Named parameters to pass as additional pre_lease report filter parameters.
#'   Must be valid parameters for the pre_lease report.
#' @param request_id A unique identifier for the request. Defaults to the current
#'   timestamp.
#' @param max_retries The maximum number of retries to attempt when performing the request to
#'   the `/queue` endpoint. Defaults to `10`.
#' @param entrata_config The Entrata configuration object. Defaults to the global
#'   Entrata configuration object.
#'
#' @returns
#' A list containing:
#' - `summary` - A tibble representing the summary table for the pre_lease report.
#' - `details` - A tibble representing the details table for the pre_lease report.
#' - `parameters` - A list of the parameters used in the request.
#'
#' @export

#' Entrata Weekly Lease Execution Report
#'
#' @description
#' This function retrieves the weekly lease execution report data from Entrata.
#'
#' This data is used in conjunction with the data retrieved via [entrata_pre_lease_report()]
#' to append data for the prior seven days (hence weekly) new leases, renewals,
#' and total by property.
#'
#' @param property_ids Vector of property IDs to include in the report. Defaults to all properties.
#' @param report_date Date to use for the report. Defaults to the current date.
#' @param ... Additional parameters to pass to the report request.
#' @param request_id Request ID for the report. Defaults to the current timestamp
#'   as an integer.
#' @param report_version Version of the report to request. Defaults to "1.8".
#' @param max_retries Maximum number of retries to attempt when performing the request to
#'   the `/queue` endpoint. Defaults to 10.
#' @param entrata_config Entrata configuration object. Defaults to the
#'   global Entrata configuration object.
#'
#' @returns
#' A tibble containing the weekly lease execution report data.
#'
#' @seealso [entrata_pre_lease_report()], [entrata_reports()]
#'
#' @export
#'
#' @importFrom cli cli_alert_info cli_alert_danger
#' @importFrom dplyr select mutate rename across everything
#' @importFrom httr2 req_body_json req_perform resp_body_json req_progress
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom purrr pluck map_chr
#' @importFrom tibble as_tibble
#' @importFrom tidyr pivot_wider replace_na
entrata_lease_execution_report <- function(
    property_ids = NULL,
    report_date = NULL,
    ...,
    request_id = NULL,
    report_version = "1.8",
    max_retries = 10,
    entrata_config = NULL
) {

  if (is.null(entrata_config)) { entrata_config <- get_entrata_config() }
  if (is.null(report_date)) { report_date <- Sys.Date() }
  if (is.null(request_id)) { request_id <- as.integer(Sys.time()) }
  if (is.null(property_ids)) { property_ids <- get_entrata_property_ids() }

  # derive daterange period
  weekly_period <- get_weekly_period() |> format_date_for_entrata()
  req_period <- list(
    daterange = list(
      "daterange-start" = weekly_period[[1]],
      "daterange-end" = weekly_period[[2]]
    ),
    "period_type" = "daterange"
  )

  # create the report request body
  req_body <- list(
    auth = list(type = "basic"),
    requestId = request_id,
    method = list(
      name = "getReportData",
      version = "r3",
      params = list(
        reportName = "lease_execution_(applicant)",
        reportVersion = report_version, # get_latest_report_version("lease_execution_(applicant)"),
        filters = list(
          property_group_ids = property_ids,
          period = req_period,
          results_based_on = "activity",
          lease_type = c("1", "3"),
          summarize_by = "lease_type",
          group_by = "property",
          consolidate_by = "no_consolidation",
          arrange_by_property = "0",
          subtotals = "0"
        )
      )
    )
  )

  req <- entrata_request(entrata_config) |>
    entrata_req_endpoint("reports") |>
    httr2::req_body_json(req_body)

  resp <- httr2::req_perform(req)

  queue_id <- httr2::resp_body_json(resp) |>
    purrr::pluck("response", "result", "queueId")

  cli::cli_alert_info(
    c(
      "Lease Execution Report data has been successfully queued for processing.",
      "The queue ID is: {.field {queue_id}}"
    )
  )

  # call queue endpoint with queue id
  queue_req <- entrata_request(entrata_config) |>
    entrata_req_endpoint("queue") |>
    httr2::req_body_json(
      list(
        auth = list(type = 'basic'),
        request_id = request_id,
        method = list(
          name = "getResponse",
          version = "r1",
          params = list(
            queueId = queue_id,
            serviceName = "getReportData"
          )
        )
      )
    ) |>
    # enable progress
    httr2::req_progress() |>
    # setup retry logic
    entrata_req_retry(max_tries = max_retries, max_seconds = 120)

  # send queue request
  queue_resp <- tryCatch({
    httr2::req_perform(queue_req)
  }, error = function(e) {
    cli::cli_alert_danger(
      c(
        "Failed to retrieve pre_lease report data from Entrata",
        "Error: {.error {e}}"
      )
    )
  })

  # check response
  entrata_resp_check(queue_resp)

  # parse response to get report data
  report_data <- httr2::resp_body_json(queue_resp) |>
    pluck("response", "result", "reportData")

  report_data |>
    jsonlite::toJSON(auto_unbox = TRUE, pretty = TRUE) |>
    jsonlite::fromJSON(flatten = TRUE) |>
    tibble::as_tibble() |>
    dplyr::select("property_name", "lease_type", "signed") |>
    tidyr::pivot_wider(names_from = lease_type, values_from = signed) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ tidyr::replace_na(., 0))) |>
    dplyr::rename(weekly_new = `New Lease`, weekly_renewal = Renewal) |>
    dplyr::mutate(
      # get property id from named vector (names = property names)
      property_id = purrr::map_chr(property_name, ~ purrr::pluck(property_ids, .x)),
      report_date = report_date,
      weekly_total = weekly_new + weekly_renewal
    ) |>
    dplyr::select(
      property_id,
      property_name,
      report_date,
      weekly_new,
      weekly_renewal,
      weekly_total
    )

}
