
#  ------------------------------------------------------------------------
#
# Title : Entrata Pre-Lease Report
#    By : Jimmy Briggs
#  Date : 2024-11-24
#
#  ------------------------------------------------------------------------

#' Entrata Pre-Lease Report
#'
#' @description
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
entrata_pre_lease_report <- function(
    property_ids = NULL,
    report_date = NULL,
    summarize_by = "property",
    group_by = "do_not_group",
    consider_pre_leased_on = 32,
    ...,
    request_id = NULL,
    max_retries = 10,
    entrata_config = NULL
) {

  if (is.null(entrata_config)) { entrata_config <- get_entrata_config() }
  if (is.null(report_date)) { report_date <- Sys.Date() }
  if (is.null(request_id)) { request_id <- as.integer(Sys.time()) }
  if (is.null(property_ids)) { property_ids <- get_entrata_property_ids() }

  # validation
  group_by <- rlang::arg_match0(group_by, c("do_not_group", "unit_type", "floorplan_name", "lease_term"))
  summarize_by <- rlang::arg_match0(summarize_by, c("property", "unit_type", "floorplan_name", "do_not_summarize"))
  consider_pre_leased_on <- as.character(consider_pre_leased_on)
  consider_pre_leased_on <- rlang::arg_match0(consider_pre_leased_on, as.character(c(32, 33, 34, 41, 42, 43, 44)))

  # additional params
  additional_params <- list(...)
  charge_code_detail <- purrr::pluck(additional_params, "charge_code_detail") %||% 1
  space_options <- purrr::pluck(additional_params, "space_options") %||% "do_not_show"
  additional_units_shown <- purrr::pluck(additional_params, "additional_units_shown") %||% "available"
  combine_unit_spaces_with_same_lease <- purrr::pluck(additional_params, "combine_unit_spaces_with_same_lease") %||% 0
  consolidate_by <- purrr::pluck(additional_params, "consolidate_by") %||% "no_consolidation"
  arrange_by_property <- purrr::pluck(additional_params, "arrange_by_property") %||% 0
  subtotals <- purrr::pluck(additional_params, "subtotals") %||% c("summary", "details")
  yoy <- purrr::pluck(additional_params, "yoy") %||% 1

  # determine the correct "period" date for the report to use
  # should represent the next pre-lease season's start date
  period_date <- get_next_pre_lease_period_start_date(report_date) |>
    format_date_for_entrata()

  period <- list(
    date = period_date,
    period_type = "date"
  )

  # derive request body report filter parameters
  report_filter_params <- list(
    property_group_ids = as.list(as.character(property_ids)),
    period = period,
    summarize_by = summarize_by,
    group_by = group_by,
    consider_pre_leased_on = as.character(consider_pre_leased_on),
    charge_code_detail = as.character(charge_code_detail),
    space_options = space_options,
    additional_units_shown = additional_units_shown,
    combine_unit_spaces_with_same_lease = as.character(combine_unit_spaces_with_same_lease),
    consolidate_by = consolidate_by,
    arrange_by_property = as.character(arrange_by_property),
    subtotals = subtotals,
    yoy = as.character(yoy)
  )

  # create the report request body
  req_body <- list(
    auth = list(type = 'basic'),
    request_id = request_id,
    method = list(
      name = "getReportData",
      version = "r3",
      params = list(
        reportName = "pre_lease",
        reportVersion = "3.2",
        filters = report_filter_params
      )
    )
  )

  # build request
  req <- entrata_request(entrata_config) |>
    entrata_req_endpoint("reports") |>
    httr2::req_body_json(req_body)

  # send request
  resp <- httr2::req_perform(req)

  # check response
  entrata_resp_check(resp)

  # parse response to get queue id
  queue_id <- httr2::resp_body_json(resp) |>
    pluck("response", "result", "queueId", 1)

  cli::cli_alert_success(
    c(
      "Pre-Lease Report Request Submitted\n",
      "Queue ID: {.field {queue_id}}"
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

  # summary & details
  summary_data <- report_data |>
    pluck("summary") |>
    dplyr::bind_rows() |>
    tibble::as_tibble()

  details_data <- report_data |>
    pluck("details") |>
    dplyr::bind_rows() |>
    tibble::as_tibble()

  return(
    list(
      summary = summary_data,
      details = details_data,
      parameters = report_filter_params
    )
  )

}

get_entrata_pre_lease_summary <- function(
  property_ids = NULL,
  report_date = NULL,
  request_id = NULL,
  entrata_config = NULL,
  ...
) {

  report_data <- entrata_pre_lease_report()

  report_summary_data <- purrr::pluck(report_data, "summary")

  weeks_left_to_lease <- get_weeks_left_to_lease()

  report_summary_data |>
    dplyr::transmute(
      property_id = property_id,
      property_name = property_name,
      total_beds = units,
      model_beds = 0,
      current_occupied = occupied_count,
      current_occupancy = occupied_count / total_beds,
      current_total_new = approved_new_count + partially_completed_new_count + completed_new_count,
      current_total_renewals = approved_renewal_count + partially_completed_renewal_count + completed_renewal_count,
      current_total_leases = current_total_new + current_total_renewals,
      current_preleased_percent = current_total_leases / total_beds,
      current_preleased_percent_original = preleased_percent,
      prior_total_new = approved_new_count_prior + partially_completed_new_count_prior + completed_new_count_prior,
      prior_total_renewals = approved_renewal_count_prior + partially_completed_renewal_count_prior + completed_renewal_count_prior,
      prior_total_leases = approved_count_prior + partially_completed_count_prior + completed_count_prior,
      prior_preleased_percent = prior_total_leases / total_beds,
      yoy_variance_count = current_total_leases - prior_total_leases,
      yoy_variance_percent = current_preleased_percent - prior_preleased_percent,
      weekly_new = 0,
      weekly_renewal = 0,
      weekly_total = 0,
      weekly_percent_gained = 0,
      beds_left = total_beds - current_total_leases,
      vel_90 = beds_left * .9 / weeks_left_to_lease,
      vel_95 = beds_left * .95 / weeks_left_to_lease,
      vel_100 = beds_left * 1 / weeks_left_to_lease
    )

}



















# # pre_lease_report_request_parameter_specs <- list(
# #   property_ids = mem_get_entrata_property_ids() |> unname() |> unlist(),
# #   summarize_by = c("unit_type", "floorplan_name", "property", "do_not_summarize"),
# #   group_by = c("unit_type", "floorplan_name", "lease_term", "do_not_group"),
# #   consider_pre_leased_on = c("32", "33", "34", "41", "42", "43", "44"),
# #   charge_code_detail = c("0", "1"),
# #   space_options = c("show_preferred", "show_actual", "do_not_show"),
# #   additional_units_shown = c("available", "excluded"),
# #   combine_unit_spaces_with_same_lease = c(0, 1),
# #   consolidate_by = c("no_consolidation", "consolidate_all_properties", "consolidate_by_property_groups"),
# #   arrange_by_property = c(0, 1),
# #   subtotals = c("summary", "details"),
# #   yoy = c(0, 1)
# # )
# #
# # validate_pre_lease_report_param <- function(param_name, param_value, call = rlang::caller_env()) {
# #
# #   allowed_values <- pre_lease_report_request_parameter_specs |> purrr::pluck(param_name)
# #
# #   param_arg <- rlang::caller_arg(param_name)
# #
# #   if (!is.null(allowed_values) && !param_value %in% allowed_values) {
# #     cli::cli_abort(
# #       c(
# #         "Invalid value ({.field {param_value}}) for the {.field {param_arg}} argument.",
# #         "Allowed values are: {.field {allowed_values}}."
# #       ),
# #       call = call
# #     )
# #   }
# #
# #   return(invisible(NULL))
# #
# # }
#
# # if current date is before 9/1, set report date to current year,
# # otherwise next year
#
#
#
#
# entrata_reports_pre_lease <- function(
    #     property_ids = NULL,
#     summarize_by = "property",
#     group_by = "do_not_group",
#     consider_pre_leased_on = 32,
#     charge_code_detail = 1,
#     space_options = "do_not_show",
#     additional_units_shown = "available",
#     combine_unit_spaces_with_same_lease = 0,
#     consolidate_by = "no_consolidation",
#     arrange_by_property = 1,
#     subtotals = c("summary", "details"),
#     yoy = 1,
#     request_id = NULL,
#     report_version = "3.2",
#     entrata_config = get_entrata_config()
# ) {
#
#   if (is.null(property_ids)) {
#     property_ids <- mem_get_entrata_property_ids() |> unname() |> unlist()
#   }
#
#   leasing_period_start_date <- get_next_pre_lease_period_start_date()
#   period_type <- "date"
#
#   period <- list(
#     date = leasing_period_start_date,
#     period_type = period_type
#   )
#
#   # summarize_by <- rlang::arg_match(summarize_by, multiple = FALSE)
#   # group_by <- rlang::arg_match(group_by, multiple = FALSE)
#   # consider_pre_leased_on <- rlang::arg_match(consider_pre_leased_on, multiple = FALSE)
#   # charge_code_detail <- rlang::arg_match(charge_code_detail, multiple = FALSE)
#   # space_options <- rlang::arg_match(space_options, multiple = FALSE)
#   # additional_units_shown <- rlang::arg_match(additional_units_shown, multiple = FALSE)
#   # combine_unit_spaces_with_same_lease <- rlang::arg_match(combine_unit_spaces_with_same_lease, multiple = FALSE)
#   # consolidate_by <- rlang::arg_match(consolidate_by, multiple = FALSE)
#   # arrange_by_property <- rlang::arg_match(arrange_by_property, multiple = FALSE)
#   # yoy <- rlang::arg_match(yoy, multiple = FALSE)
#   # subtotals <- rlang::arg_match(subtotals, multiple = TRUE)
#
#   if (is.null(request_id)) {
#     request_id <- as.integer(Sys.time())
#   }
#
#   req_method_params <- list(
#     reportName = "pre_lease",
#     reportVersion = report_version,
#     filters = list(
#       property_group_ids = property_ids,
#       period = period,
#       summarize_by = summarize_by,
#       group_by = group_by,
#       consider_pre_leased_on = consider_pre_leased_on,
#       charge_code_detail = charge_code_detail,
#       space_options = space_options,
#       additional_units_shown = additional_units_shown,
#       combine_unit_spaces_with_same_lease = combine_unit_spaces_with_same_lease,
#       consolidate_by = consolidate_by,
#       arrange_by_property = arrange_by_property,
#       subtotals = subtotals,
#       yoy = yoy
#     )
#   )
#
#   reports_req <- httr2::request(base_url = entrata_config$base_url) |>
#     httr2::req_url_path_append("reports") |>
#     httr2::req_auth_basic(
#       username = entrata_config$username,
#       password = entrata_config$password
#     ) |>
#     httr2::req_body_json(
#       list(
#         auth = list(type = 'basic'),
#         request_id = request_id,
#         method = list(
#           name = "getReportData",
#           version = "r3",
#           params = req_method_params
#         )
#       )
#     )
#
#
#
#   reports_resp <- httr2::req_perform(reports_req)
#
#   reports_resp_queue_id <- reports_resp |> httr2::resp_body_json() |> purrr::pluck("response", "result", "queueId")
#
#   cli::cli_alert_success(
#     c(
#       "Pre-Lease Report Request Submitted\n",
#       "Queue ID: {.field {reports_resp_queue_id}}"
#     )
#   )
#
#   queue_req <- httr2::request(base_url = entrata_config$base_url) |>
#     httr2::req_url_path_append("queue") |>
#     httr2::req_auth_basic(
#       username = entrata_config$username,
#       password = entrata_config$password
#     ) |>
#     httr2::req_body_json(
#       list(
#         auth = list(type = 'basic'),
#         request_id = request_id,
#         method = list(
#           name = "getResponse",
#           version = "r1",
#           params = list(
#             queueId = reports_resp_queue_id,
#             serviceName = "getReportData"
#           )
#         )
#       )
#     ) |>
#     entrata_req_retry(max_tries = 10)
#
#   queue_resp <- httr2::req_perform(queue_req)
#   queue_resp_data <- queue_resp |> httr2::resp_body_json() |>
#     purrr::pluck("response", "result", "reportData")
#
#   resp_summary <- purrr::pluck(queue_resp_data, "summary") |> dplyr::bind_rows()
#   resp_details <- purrr::pluck(queue_resp_data, "details") |> dplyr::bind_rows()
#
#   list(
#     summary = resp_summary,
#     details = resp_details
#   )
#
# }
#
#
# # list(
# #   auth = list(type = "basic"),
# #   method = list(
# #     name = "getReportData",
# #     version = "r3",
# #     params = list(
# #       reportName = "pre_lease",
# #       reportVersion = "3.2",
# #       filters = list(
# #         property_group_ids = as.list(as.character(property_ids)),
# #         period = list(
# #           date = leasing_period_start_date,
# #           period_type = "date"
# #         ),
# #         summarize_by = "unit_type",
# #         group_by = "unit_type",
# #         consider_pre_leased_on = 32,
# #         charge_code_detail = 0,
# #         space_options = 'do_not_show',
# #         additional_units_shown = 'available',
# #         combine_unit_spaces_with_same_lease = 0,
# #         consolidate_by = "no_consolidation",
# #         arrange_by_property = 0,
# #         subtotals = list("summary", "details"),
# #         yoy = 1
# #       )
# #     )
# #   )
# # )
#
# # process_pre_lease_report_data <- function(pre_lease_data) {
# #
# #   pre_lease_data |>
# #
# #
# #
# # }
# #
# #
# # pre_lease_report_data <- entrata_reports_pre_lease()
# #
# # pre_lease_summary_data <- pre_lease_report_data$summary
# # pre_lease_details_data <- pre_lease_report_data$details
# #
# #
# # process_pre_lease_summary_data <- function(resp_data_summary) {
# #
# #   leasing_season_end_date <- get_leasing_period_end_date() |>
# #     lubridate::mdy()
# #
# #   weeks_left_to_lease <- get_weeks_left_to_lease()
# #
# #   sum_cols <- c(
# #     "approved_count",
# #     "completed_count",
# #     "completed_renewal_count",
# #     "partially_completed_count",
# #     "partially_completed_new_count",
# #     "partially_completed_renewal_count",
# #     "preleased_count",
# #     "preleased_new_count",
# #     "preleased_renewal_count"
# #   )
# #
# #   summary_tbl <- resp_data_summary |>
# #     tibble::as_tibble() |>
# #     dplyr::mutate(
# #       leases_count = rowSums(
# #         dplyr::across(dplyr::all_of(sum_cols)), na.rm = TRUE
# #       ),
# #       total_beds = .data$available_count,
# #       model_beds = 0,
# #       current_occupied = .data$occupied_count,
# #       current_occupency = .data$occupied_count / .data$total_beds,
# #       total_new = .data$approved_new_count + .data$partially_completed_new_count + .data$completed_new_count,
# #       total_renewals = .data$approved_renewal_count + .data$partially_completed_renewal_count + .data$completed_renewal_count,
# #       total_leases = .data$total_new + .data$total_renewals, # leases_count,
# #       prelease_percent = .data$total_leases / .data$total_beds,
# #       prior_total_new = .data$approved_new_count_prior + .data$partially_completed_new_count_prior + .data$completed_new_count_prior,
# #       prior_total_renewals = .data$approved_renewal_count_prior + .data$partially_completed_renewal_count_prior + .data$completed_renewal_count_prior,
# #       prior_total_leases = .data$approved_count_prior + .data$partially_completed_count_prior + .data$completed_count_prior,
# #       prior_prelease_percent = .data$prior_total_leases / .data$total_beds,
# #       yoy_variance_1 = .data$total_leases - .data$prior_total_leases,
# #       yoy_variance_2 = .data$prelease_percent - .data$prior_prelease_percent,
# #       seven_new = 0,
# #       seven_renewal = 0,
# #       seven_total = .data$seven_new + .data$seven_renewal,
# #       seven_percent_gained = .data$seven_total / .data$total_beds,
# #       beds_left = .data$total_beds - .data$total_leases,
# #       leased_this_week = .data$seven_total,
# #       vel_90 = .data$beds_left * .9 / .env$weeks_left_to_lease,
# #       vel_95 = .data$beds_left * .95 / .env$weeks_left_to_lease,
# #       vel_100 = .data$beds_left * 1 / .env$weeks_left_to_lease
# #     ) |>
# #     dplyr::select(
# #       property_id,
# #       property_name,
# #       unit_type = units,
# #       leases_count,
# #       total_beds,
# #       model_beds,
# #       current_occupied,
# #       current_occupency,
# #       total_new,
# #       total_renewals,
# #       total_leases,
# #       prelease_percent,
# #       prior_total_new,
# #       prior_total_renewals,
# #       prior_total_leases,
# #       prior_prelease_percent,
# #       yoy_variance_1,
# #       yoy_variance_2,
# #       seven_new,
# #       seven_renewal,
# #       seven_total,
# #       seven_percent_gained,
# #       beds_left,
# #       leased_this_week,
# #       vel_90,
# #       vel_95,
# #       vel_100
# #     )
# #
# #   if (by_property) {
# #     summary_tbl_ <- summary_tbl |>
# #       dplyr::group_by(.data$property_name) |>
# #       dplyr::summarise(
# #         leases_count = sum(.data$leases_count, na.rm = TRUE),
# #         total_beds = sum(.data$total_beds, na.rm = TRUE),
# #         model_beds = sum(.data$model_beds, na.rm = TRUE),
# #         current_occupied = sum(.data$current_occupied, na.rm = TRUE),
# #         current_occupency = sum(.data$current_occupency, na.rm = TRUE),
# #         total_new = sum(.data$total_new, na.rm = TRUE),
# #         total_renewals = sum(.data$total_renewals, na.rm = TRUE),
# #         total_leases = sum(.data$total_leases, na.rm = TRUE),
# #         prelease_percent = sum(.data$prelease_percent, na.rm = TRUE),
# #         prior_total_new = sum(.data$prior_total_new, na.rm = TRUE),
# #         prior_total_renewals = sum(.data$prior_total_renewals, na.rm = TRUE),
# #         prior_total_leases = sum(.data$prior_total_leases, na.rm = TRUE),
# #         prior_prelease_percent = sum(.data$prior_prelease_percent, na.rm = TRUE),
# #         yoy_variance_1 = sum(.data$yoy_variance_1, na.rm = TRUE),
# #         yoy_variance_2 = sum(.data$yoy_variance_2, na.rm = TRUE),
# #         seven_new = sum(.data$seven_new, na.rm = TRUE),
# #         seven_renewal = sum(.data$seven_renewal, na.rm = TRUE),
# #         seven_total = sum(.data$seven_total, na.rm = TRUE),
# #         seven_percent_gained = sum(.data$seven_percent_gained, na.rm = TRUE),
# #         beds_left = sum(.data$beds_left, na.rm = TRUE),
# #         leased_this_week = sum(.data$leased_this_week, na.rm = TRUE),
# #         vel_90 = sum(.data$vel_90, na.rm = TRUE),
# #         vel_95 = sum(.data$vel_95, na.rm = TRUE),
# #         vel_100 = sum(.data$vel_100, na.rm = TRUE)
# #       ) |>
# #       dplyr::ungroup()
# #
# #   }
# #
# # }
# #
