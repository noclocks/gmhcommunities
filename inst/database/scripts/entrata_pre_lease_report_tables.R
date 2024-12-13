
entrata_config <- get_entrata_config()
property_ids <- mem_get_entrata_property_ids() |> unname() |> unlist()
leasing_period_start_date <- get_leasing_period_start_date()
leasing_period_type <- "date"

period <- list(
  date = leasing_period_start_date,
  period_type = leasing_period_type
)

summarize_by <- "property" # "unit_type", "floorplan_name", "do_not_summarize"
group_by <- "do_not_group" # "do_not_group", "unit_type", "floorplan_name", "lease_term"),
consider_pre_leased_on <- "33" # "33", "32", "34", "41", "42", "43", "44"
charge_code_detail <- "0"
space_options <- "do_not_show" # "do_not_show", "show_preferred", "show_actual"
additional_units_shown <- "available" # "available", "excluded"
combine_unit_spaces_with_same_lease <- "0"
consolidate_by <- "no_consolidation" # "no_consolidation", "consolidate_all_properties", "consolidate_by_property_groups"
arrange_by_property <- "0"
subtotals <- c("summary", "details")
yoy <- "1"
request_id <- 99
report_version <- "3.2"

reports_req <- httr2::request(base_url = entrata_config$base_url) |>
  httr2::req_url_path_append("reports") |>
  httr2::req_auth_basic(
    username = entrata_config$username,
    password = entrata_config$password
  ) |>
  httr2::req_body_json(
    list(
      auth = list(type = "basic"),
      method = list(
        name = "getReportData",
        version = "r3",
        params = list(
          reportName = "pre_lease",
          reportVersion = "3.2",
          filters = list(
            property_group_ids = as.list(as.character(property_ids)),
            period = list(
              date = leasing_period_start_date,
              period_type = "date"
            ),
            summarize_by = "unit_type",
            group_by = "unit_type",
            consider_pre_leased_on = 32,
            charge_code_detail = 0,
            space_options = 'do_not_show',
            additional_units_shown = 'available',
            combine_unit_spaces_with_same_lease = 0,
            consolidate_by = "no_consolidation",
            arrange_by_property = 0,
            subtotals = list("summary", "details"),
            yoy = 1
          )
        )
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

httptest2::capture_requests({
  queue_resp <- httr2::req_perform(queue_req)
})


queue_resp_data <- queue_resp |> httr2::resp_body_json() |>
  purrr::pluck("response", "result", "reportData")

report_summary_data <- queue_resp_data |>
  purrr::pluck("summary") |>
  dplyr::bind_rows()

readr::write_csv(report_summary_data, "data-raw/data/working/entrata_pre_lease_report_summary.csv")

report_details_data <- queue_resp_data |>
  purrr::pluck("details") |>
  dplyr::bind_rows()

readr::write_csv(report_details_data, "data-raw/data/working/entrata_pre_lease_report_details.csv")

dplyr::glimpse(report_details_data)

commonwealth_report_details_data <- dplyr::filter(
  report_details_data,
  property_id == "739085"
)

pre_lease_details_tbl <- report_details_data |>
  dplyr::transmute(
    property_id = property_id,
    property_name = property_name,
    floorplan = floorplan_name,
    unit_type = unit_type,
    unit_status = unit_status,
    total_square_feet = sqft,
    building_unit = bldg_unit,



  )
