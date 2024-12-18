
#  ------------------------------------------------------------------------
#
# Title : Entrata Pre-Lease Report
#    By : Jimmy Briggs
#  Date : 2024-11-24
#
#  ------------------------------------------------------------------------

# pre_lease_report_request_parameter_specs <- list(
#   property_ids = mem_get_entrata_property_ids() |> unname() |> unlist(),
#   summarize_by = c("unit_type", "floorplan_name", "property", "do_not_summarize"),
#   group_by = c("unit_type", "floorplan_name", "lease_term", "do_not_group"),
#   consider_pre_leased_on = c("32", "33", "34", "41", "42", "43", "44"),
#   charge_code_detail = c("0", "1"),
#   space_options = c("show_preferred", "show_actual", "do_not_show"),
#   additional_units_shown = c("available", "excluded"),
#   combine_unit_spaces_with_same_lease = c(0, 1),
#   consolidate_by = c("no_consolidation", "consolidate_all_properties", "consolidate_by_property_groups"),
#   arrange_by_property = c(0, 1),
#   subtotals = c("summary", "details"),
#   yoy = c(0, 1)
# )
#
# validate_pre_lease_report_param <- function(param_name, param_value, call = rlang::caller_env()) {
#
#   allowed_values <- pre_lease_report_request_parameter_specs |> purrr::pluck(param_name)
#
#   param_arg <- rlang::caller_arg(param_name)
#
#   if (!is.null(allowed_values) && !param_value %in% allowed_values) {
#     cli::cli_abort(
#       c(
#         "Invalid value ({.field {param_value}}) for the {.field {param_arg}} argument.",
#         "Allowed values are: {.field {allowed_values}}."
#       ),
#       call = call
#     )
#   }
#
#   return(invisible(NULL))
#
# }

# if current date is before 9/1, set report date to current year,
# otherwise next year




entrata_reports_pre_lease <- function(
    property_ids = NULL,
    summarize_by = "property",
    group_by = "do_not_group",
    consider_pre_leased_on = 32,
    charge_code_detail = 1,
    space_options = "do_not_show",
    additional_units_shown = "available",
    combine_unit_spaces_with_same_lease = 0,
    consolidate_by = "no_consolidation",
    arrange_by_property = 0,
    subtotals = c("summary", "details"),
    yoy = 1,
    request_id = NULL,
    report_version = "3.2",
    entrata_config = get_entrata_config()
) {

  if (is.null(property_ids)) {
    property_ids <- mem_get_entrata_property_ids() |> unname() |> unlist()
  }

  leasing_period_start_date <- get_next_pre_lease_period_start_date()
  period_type <- "date"

  period <- list(
    date = leasing_period_start_date,
    period_type = period_type
  )

  # summarize_by <- rlang::arg_match(summarize_by, multiple = FALSE)
  # group_by <- rlang::arg_match(group_by, multiple = FALSE)
  # consider_pre_leased_on <- rlang::arg_match(consider_pre_leased_on, multiple = FALSE)
  # charge_code_detail <- rlang::arg_match(charge_code_detail, multiple = FALSE)
  # space_options <- rlang::arg_match(space_options, multiple = FALSE)
  # additional_units_shown <- rlang::arg_match(additional_units_shown, multiple = FALSE)
  # combine_unit_spaces_with_same_lease <- rlang::arg_match(combine_unit_spaces_with_same_lease, multiple = FALSE)
  # consolidate_by <- rlang::arg_match(consolidate_by, multiple = FALSE)
  # arrange_by_property <- rlang::arg_match(arrange_by_property, multiple = FALSE)
  # yoy <- rlang::arg_match(yoy, multiple = FALSE)
  # subtotals <- rlang::arg_match(subtotals, multiple = TRUE)

  if (is.null(request_id)) {
    request_id <- as.integer(Sys.time())
  }

  req_method_params <- list(
    reportName = "pre_lease",
    reportVersion = report_version,
    filters = list(
      property_group_ids = property_ids,
      period = period,
      summarize_by = summarize_by,
      group_by = group_by,
      consider_pre_leased_on = consider_pre_leased_on,
      charge_code_detail = charge_code_detail,
      space_options = space_options,
      additional_units_shown = additional_units_shown,
      combine_unit_spaces_with_same_lease = combine_unit_spaces_with_same_lease,
      consolidate_by = consolidate_by,
      arrange_by_property = arrange_by_property,
      subtotals = subtotals,
      yoy = yoy
    )
  )

  reports_req <- httr2::request(base_url = entrata_config$base_url) |>
    httr2::req_url_path_append("reports") |>
    httr2::req_auth_basic(
      username = entrata_config$username,
      password = entrata_config$password
    ) |>
    httr2::req_body_json(
      list(
        auth = list(type = 'basic'),
        request_id = request_id,
        method = list(
          name = "getReportData",
          version = "r3",
          params = req_method_params
        )
      )
    )

  reports_resp <- httr2::req_perform(reports_req)

  reports_resp_queue_id <- reports_resp |> httr2::resp_body_json() |> purrr::pluck("response", "result", "queueId")

  cli::cli_alert_success(
    c(
      "Pre-Lease Report Request Submitted\n",
      "Queue ID: {.field {reports_resp_queue_id}}"
    )
  )

  queue_req <- httr2::request(base_url = entrata_config$base_url) |>
    httr2::req_url_path_append("queue") |>
    httr2::req_auth_basic(
      username = entrata_config$username,
      password = entrata_config$password
    ) |>
    httr2::req_body_json(
      list(
        auth = list(type = 'basic'),
        request_id = request_id,
        method = list(
          name = "getResponse",
          version = "r1",
          params = list(
            queueId = reports_resp_queue_id,
            serviceName = "getReportData"
          )
        )
      )
    ) |>
    entrata_req_retry(max_tries = 10)

  queue_resp <- httr2::req_perform(queue_req)
  queue_resp_data <- queue_resp |> httr2::resp_body_json() |>
    purrr::pluck("response", "result", "reportData")

  resp_summary <- purrr::pluck(queue_resp_data, "summary") |> dplyr::bind_rows()
  resp_details <- purrr::pluck(queue_resp_data, "details") |> dplyr::bind_rows()

  list(
    summary = resp_summary,
    details = resp_details
  )

}


# list(
#   auth = list(type = "basic"),
#   method = list(
#     name = "getReportData",
#     version = "r3",
#     params = list(
#       reportName = "pre_lease",
#       reportVersion = "3.2",
#       filters = list(
#         property_group_ids = as.list(as.character(property_ids)),
#         period = list(
#           date = leasing_period_start_date,
#           period_type = "date"
#         ),
#         summarize_by = "unit_type",
#         group_by = "unit_type",
#         consider_pre_leased_on = 32,
#         charge_code_detail = 0,
#         space_options = 'do_not_show',
#         additional_units_shown = 'available',
#         combine_unit_spaces_with_same_lease = 0,
#         consolidate_by = "no_consolidation",
#         arrange_by_property = 0,
#         subtotals = list("summary", "details"),
#         yoy = 1
#       )
#     )
#   )
# )

# process_pre_lease_report_data <- function(pre_lease_data) {
#
#   pre_lease_data |>
#
#
#
# }
#
#
# pre_lease_report_data <- entrata_reports_pre_lease()
#
# pre_lease_summary_data <- pre_lease_report_data$summary
# pre_lease_details_data <- pre_lease_report_data$details
#
#
# process_pre_lease_summary_data <- function(resp_data_summary) {
#
#   leasing_season_end_date <- get_leasing_period_end_date() |>
#     lubridate::mdy()
#
#   weeks_left_to_lease <- get_weeks_left_to_lease()
#
#   sum_cols <- c(
#     "approved_count",
#     "completed_count",
#     "completed_renewal_count",
#     "partially_completed_count",
#     "partially_completed_new_count",
#     "partially_completed_renewal_count",
#     "preleased_count",
#     "preleased_new_count",
#     "preleased_renewal_count"
#   )
#
#   summary_tbl <- resp_data_summary |>
#     tibble::as_tibble() |>
#     dplyr::mutate(
#       leases_count = rowSums(
#         dplyr::across(dplyr::all_of(sum_cols)), na.rm = TRUE
#       ),
#       total_beds = .data$available_count,
#       model_beds = 0,
#       current_occupied = .data$occupied_count,
#       current_occupency = .data$occupied_count / .data$total_beds,
#       total_new = .data$approved_new_count + .data$partially_completed_new_count + .data$completed_new_count,
#       total_renewals = .data$approved_renewal_count + .data$partially_completed_renewal_count + .data$completed_renewal_count,
#       total_leases = .data$total_new + .data$total_renewals, # leases_count,
#       prelease_percent = .data$total_leases / .data$total_beds,
#       prior_total_new = .data$approved_new_count_prior + .data$partially_completed_new_count_prior + .data$completed_new_count_prior,
#       prior_total_renewals = .data$approved_renewal_count_prior + .data$partially_completed_renewal_count_prior + .data$completed_renewal_count_prior,
#       prior_total_leases = .data$approved_count_prior + .data$partially_completed_count_prior + .data$completed_count_prior,
#       prior_prelease_percent = .data$prior_total_leases / .data$total_beds,
#       yoy_variance_1 = .data$total_leases - .data$prior_total_leases,
#       yoy_variance_2 = .data$prelease_percent - .data$prior_prelease_percent,
#       seven_new = 0,
#       seven_renewal = 0,
#       seven_total = .data$seven_new + .data$seven_renewal,
#       seven_percent_gained = .data$seven_total / .data$total_beds,
#       beds_left = .data$total_beds - .data$total_leases,
#       leased_this_week = .data$seven_total,
#       vel_90 = .data$beds_left * .9 / .env$weeks_left_to_lease,
#       vel_95 = .data$beds_left * .95 / .env$weeks_left_to_lease,
#       vel_100 = .data$beds_left * 1 / .env$weeks_left_to_lease
#     ) |>
#     dplyr::select(
#       property_id,
#       property_name,
#       unit_type = units,
#       leases_count,
#       total_beds,
#       model_beds,
#       current_occupied,
#       current_occupency,
#       total_new,
#       total_renewals,
#       total_leases,
#       prelease_percent,
#       prior_total_new,
#       prior_total_renewals,
#       prior_total_leases,
#       prior_prelease_percent,
#       yoy_variance_1,
#       yoy_variance_2,
#       seven_new,
#       seven_renewal,
#       seven_total,
#       seven_percent_gained,
#       beds_left,
#       leased_this_week,
#       vel_90,
#       vel_95,
#       vel_100
#     )
#
#   if (by_property) {
#     summary_tbl_ <- summary_tbl |>
#       dplyr::group_by(.data$property_name) |>
#       dplyr::summarise(
#         leases_count = sum(.data$leases_count, na.rm = TRUE),
#         total_beds = sum(.data$total_beds, na.rm = TRUE),
#         model_beds = sum(.data$model_beds, na.rm = TRUE),
#         current_occupied = sum(.data$current_occupied, na.rm = TRUE),
#         current_occupency = sum(.data$current_occupency, na.rm = TRUE),
#         total_new = sum(.data$total_new, na.rm = TRUE),
#         total_renewals = sum(.data$total_renewals, na.rm = TRUE),
#         total_leases = sum(.data$total_leases, na.rm = TRUE),
#         prelease_percent = sum(.data$prelease_percent, na.rm = TRUE),
#         prior_total_new = sum(.data$prior_total_new, na.rm = TRUE),
#         prior_total_renewals = sum(.data$prior_total_renewals, na.rm = TRUE),
#         prior_total_leases = sum(.data$prior_total_leases, na.rm = TRUE),
#         prior_prelease_percent = sum(.data$prior_prelease_percent, na.rm = TRUE),
#         yoy_variance_1 = sum(.data$yoy_variance_1, na.rm = TRUE),
#         yoy_variance_2 = sum(.data$yoy_variance_2, na.rm = TRUE),
#         seven_new = sum(.data$seven_new, na.rm = TRUE),
#         seven_renewal = sum(.data$seven_renewal, na.rm = TRUE),
#         seven_total = sum(.data$seven_total, na.rm = TRUE),
#         seven_percent_gained = sum(.data$seven_percent_gained, na.rm = TRUE),
#         beds_left = sum(.data$beds_left, na.rm = TRUE),
#         leased_this_week = sum(.data$leased_this_week, na.rm = TRUE),
#         vel_90 = sum(.data$vel_90, na.rm = TRUE),
#         vel_95 = sum(.data$vel_95, na.rm = TRUE),
#         vel_100 = sum(.data$vel_100, na.rm = TRUE)
#       ) |>
#       dplyr::ungroup()
#
#   }
#
# }
#
