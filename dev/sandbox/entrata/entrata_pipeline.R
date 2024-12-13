# Entrata API Data Retrieval Pipeline

# get config
entrata_config <- config::get("entrata")

# base request
entrata_req <- httr2::request(entrata_config$base_url) |>
  httr2::req_method("POST") |>
  httr2::req_auth_basic(
    entrata_config$username,
    entrata_config$password
  ) |>
  httr2::req_headers(
    `Content-Type` = "application/json; charset=utf-8",
    `Accept` = "application/json"
  )

# function to derive request body
# TODO

get_entrata_properties <- function(entrata_config = load_entrata_config()) {
  httr2::request(base_url = entrata_config$base_url) |>
    httr2::req_method("POST") |>
    httr2::req_url_path_append("properties") |>
    httr2::req_headers("Content-Type" = "application/json") |>
    httr2::req_auth_basic(
      user = entrata_config$username,
      password = entrata_config$password
    ) |>
    httr2::req_body_json(
      list(
        auth = list(type = "basic"),
        requestId = 15,
        method = list(
          name = "getProperties",
          version = "r1",
          params = list(NULL)
        )
      )
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_json() |>
    purrr::pluck("response", "result", "PhysicalProperty", "Property") |>
    purrr::map_dfr(
      function(x) {
        tibble::tibble(
          property_id = x$PropertyID,
          property_name = x$MarketingName,
          property_type = x$Type
        )
      }
    )
}

request_pre_lease_report <- function(
    entrata_config = NULL,
    property_ids = NULL,
    report_date = NULL
) {

  if (is.null(entrata_config)) { entrata_config <- load_entrata_config() }
  if (is.null(property_ids)) { property_ids <- get_entrata_properties()$property_id }
  if (is.null(report_date)) {
    now <- lubridate::today()
    current_month <- lubridate::month(now)
    if (current_month < 9) {
      report_date_year <- lubridate::year(now)
    } else {
      report_date_year <- lubridate::year(now) + 1
    }
    report_date <- paste0("09/01/", report_date_year)
  }

  req_reports <- httr2::request(entrata_config$base_url) |>
    httr2::req_method("POST") |>
    httr2::req_url_path_append("reports") |>
    httr2::req_headers("Content-Type" = "application/json") |>
    httr2::req_auth_basic(
      user = entrata_config$username,
      password = entrata_config$password
    ) |>
    httr2::req_body_json(
      list(
        auth = list(type = "basic"),
        requestId = 15,
        method = list(
          name = "getReportData",
          version = "r3",
          params = list(
            reportName = "pre_lease",
            reportVersion = "3.2",
            filters = list(
              property_group_ids = as.list(as.character(property_ids)),
              period = list(date = report_date, period_type = "date"),
              summarize_by = "property",
              group_by = "do_not_group",
              consider_pre_leased_on = "332",
              charge_code_detail = 1,
              space_options = 'do_not_show',
              additional_units_shown = 'available',
              combine_unit_spaces_with_same_lease = 0,
              consolidate_by = 'no_consolidation',
              arrange_by_property = 0,
              subtotals = list("summary", "details"),
              yoy = 1
            )
          )
        )
      )
    )

  resp_reports <- httr2::req_perform(req_reports)
  resp_queue_id <- httr2::resp_body_json(resp_reports) |>
    purrr::pluck("response", "result", "queueId")

  cli::cli_alert_info(
    c(
      "The report data has been successfully queued for processing.",
      "The queue ID is: {.field {resp_queue_id}}"
    )
  )

  return(resp_queue_id)

}

report_data_loaded <- jsonlite::fromJSON("tests/testthat/mocks/gmhcommunities.entrata.com/queue/queue-7107ca-POST.json")
report_data_extracted <- report_data_loaded$response$result
pre_lease_report_tspec_guess <- tibblify::guess_tspec(report_data_extracted)
print(pre_lease_report_tspec_guess)

queue_times <- c(
  report_data_extracted$queueStartedOn,
  report_data_extracted$queueCompletedOn
)

pre_lease_report_data <- report_data_extracted$reportData

summary_dat <- pre_lease_report_data$summary



# tspec_object(
#   tib_row(
#     "reportData",
#     tib_df(
#       "summary",
#       tib_chr("property_id"),
#       tib_chr("property_name"),
#       tib_chr("property"),
#       tib_chr("property_subtotal"),
#       tib_unspecified("lookup_code"),
#       tib_unspecified("unit_type"),
#       tib_unspecified("unit_type_subtotal"),
#       tib_unspecified("floorplan_name"),
#       tib_unspecified("number_of_bedrooms"),
#       tib_unspecified("number_of_bathrooms"),
#       tib_unspecified("floorplan_name_subtotal"),
#       tib_dbl("avg_sqft"),
#       tib_int("avg_advertised_rate"),
#       tib_unspecified("space_option"),
#       tib_int("units"),
#       tib_int("excluded_unit_count"),
#       tib_int("rentable_unit_count"),
#       tib_int("avg_scheduled_rent"),
#       tib_int("occupied_count"),
#       tib_int("started_new_count_prior"),
#       tib_int("started_new_count"),
#       tib_int("started_renewal_count_prior"),
#       tib_int("started_renewal_count"),
#       tib_int("started_count_prior"),
#       tib_int("started_count"),
#       tib_dbl("started_percent"),
#       tib_int("partially_completed_new_count_prior"),
#       tib_int("partially_completed_new_count"),
#       tib_int("partially_completed_renewal_count_prior"),
#       tib_int("partially_completed_renewal_count"),
#       tib_int("partially_completed_count_prior"),
#       tib_int("partially_completed_count"),
#       tib_dbl("partially_completed_percent"),
#       tib_int("completed_new_count_prior"),
#       tib_int("completed_new_count"),
#       tib_int("completed_renewal_count_prior"),
#       tib_int("completed_renewal_count"),
#       tib_int("completed_count_prior"),
#       tib_int("completed_count"),
#       tib_dbl("completed_percent"),
#       tib_int("approved_new_count_prior"),
#       tib_int("approved_new_count"),
#       tib_int("approved_renewal_count_prior"),
#       tib_int("approved_renewal_count"),
#       tib_int("approved_count_prior"),
#       tib_int("approved_count"),
#       tib_dbl("approved_percent"),
#       tib_int("preleased_new_count_prior"),
#       tib_int("preleased_new_count"),
#       tib_int("preleased_renewal_count_prior"),
#       tib_int("preleased_renewal_count"),
#       tib_int("preleased_count_prior"),
#       tib_int("preleased_count"),
#       tib_int("preleased_percent_prior"),
#       tib_int("preleased_percent"),
#       tib_int("variance"),
#       tib_int("available_count"),
#       tib_int("scheduled_rent_total"),
#     ),
#     tib_df(
#       "details",
#       tib_chr("property_id"),
#       tib_chr("property_name"),
#       tib_chr("property"),
#       tib_unspecified("lookup_code"),
#       tib_chr("bldg_unit"),
#       tib_chr("bldg"),
#       tib_chr("number_of_bedrooms"),
#       tib_chr("number_of_bathrooms"),
#       tib_chr("unit"),
#       tib_chr("unit_type"),
#       tib_chr("floorplan_name"),
#       tib_chr("unit_status"),
#       tib_int("sqft"),
#       tib_chr("resident"),
#       tib_chr("resident_id"),
#       tib_chr("lease_id_display"),
#       tib_chr("student_id_number"),
#       tib_chr("email"),
#       tib_chr("phone_number"),
#       tib_chr("gender"),
#       tib_chr("leasing_agent"),
#       tib_chr("lease_status"),
#       tib_chr("lease_sub_status"),
#       tib_chr("lease_occupancy_type"),
#       tib_chr("lease_term_name"),
#       tib_int("lease_term"),
#       tib_chr("space_option_preferred"),
#       tib_chr("space_option"),
#       tib_chr("lease_start"),
#       tib_chr("lease_end"),
#       tib_chr("lease_started_on"),
#       tib_chr("lease_partially_completed_on"),
#       tib_chr("lease_completed_on"),
#       tib_chr("lease_approved_on"),
#       tib_chr("move_in_date"),
#       tib_dbl("deposit_charged"),
#       tib_dbl("deposit_held"),
#       tib_dbl("market_rent"),
#       tib_dbl("budgeted_rent"),
#       tib_dbl("advertised_rate"),
#       tib_chr("ledger_name"),
#       tib_chr("charge_code"),
#       tib_dbl("scheduled_rent"),
#       tib_dbl("actual_charges"),
#       tib_dbl("scheduled_rent_total"),
#     ),
#   ),
#   tib_chr("queueStartedOn"),
#   tib_chr("queueCompletedOn"),
# )
