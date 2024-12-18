
entrata_config <- get_entrata_config()
property_ids <- mem_get_entrata_property_ids() |> unname() |> unlist()
leasing_period_start_date <- get_next_pre_lease_period_start_date()
leasing_period_type <- "date"
weeks_left_to_lease <- get_weeks_left_to_lease()

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

report_details_data <- queue_resp_data |>
  purrr::pluck("details") |>
  dplyr::bind_rows()

dplyr::glimpse(report_summary_data)
dplyr::glimpse(report_details_data)

readr::write_csv(report_summary_data, "data-raw/data/working/2024-12-17-entrata_pre_lease_report_summary.csv")
readr::write_csv(report_details_data, "data-raw/data/working/2024-12-17-entrata_pre_lease_report_details.csv")

#######################

pre_lease_report_data_lst <- entrata_reports_pre_lease()

dplyr::glimpse(pre_lease_report_data_lst$summary)
dplyr::glimpse(pre_lease_report_data_lst$details)

pre_lease_report_summary_data <- pre_lease_report_data_lst |>
  purrr::pluck("summary") |>
  # drop unused fields
  dplyr::select(
    -property,
    -tidyselect::ends_with("_subtotal")
  ) |>
  # rename fields for clarity
  dplyr::rename(
    average_square_footage = avg_sqft,
    average_advertised_rent = avg_advertised_rate,
    total_scheduled_rent = scheduled_rent_total
  ) |>
  # replace blanks
  dplyr::mutate(
    number_of_bedrooms = ifelse(number_of_bedrooms == 0, as.numeric(NA), as.integer(number_of_bedrooms)),
    number_of_bathrooms = ifelse(number_of_bathrooms == 0, as.numeric(NA), as.integer(number_of_bathrooms)),
    available_count = ifelse(available_count < 0, 0, available_count)
  ) |>
  # re-order logically
  dplyr::select(
    # property details
    property_id,
    property_name,
    # Unit Details
    unit_type,
    average_square_footage,
    average_advertised_rent,
    total_scheduled_rent,
    units,
    excluded_unit_count,
    rentable_unit_count,
    # Occupancy Metrics
    occupied_count,
    started_new_count,
    started_renewal_count,
    started_count,
    started_percent,
    partially_completed_new_count,
    partially_completed_renewal_count,
    partially_completed_count,
    partially_completed_percent,
    completed_new_count,
    completed_renewal_count,
    completed_count,
    completed_percent,
    approved_new_count,
    approved_renewal_count,
    approved_count,
    approved_percent,
    preleased_new_count,
    preleased_renewal_count,
    preleased_count,
    preleased_percent,
    variance,
    available_count,
    # Unit Attributes
    number_of_bedrooms,
    number_of_bathrooms
  ) |>
  dplyr::mutate(property_id = as.integer(property_id))


pre_lease_report_details_data <- pre_lease_report_data_lst |>
  purrr::pluck("details") |>
  # drop unused fields
  dplyr::select(
    -property,
    -unit,
    -student_id_number,
    -bldg
  ) |>
  # rename fields for clarity and consistency
  dplyr::rename(
    property_name = property_name,
    unit_number = bldg_unit,
    floorplan = floorplan_name,
    occupancy_status = unit_status,
    square_footage = sqft,
    resident_name = resident,
    resident_email = email,
    resident_phone_number = phone_number,
    resident_gender = gender,
    lease_id = lease_id_display,
    lease_term_duration = lease_term_name,
    lease_term_months = lease_term,
    lease_start_date = lease_start,
    lease_end_date = lease_end,
    advertised_rent = advertised_rate,
    scheduled_rent_per_month = scheduled_rent,
    total_scheduled_rent = scheduled_rent_total,
    leasing_agent_name = leasing_agent
  ) |>
  # convert dates
  dplyr::mutate(
    lease_start_date = lubridate::mdy(lease_start_date),
    lease_end_date = lubridate::mdy(lease_end_date),
    lease_started_on = lubridate::mdy(lease_started_on),
    lease_partially_completed_on = lubridate::mdy(lease_partially_completed_on),
    lease_completed_on = lubridate::mdy(lease_completed_on),
    lease_approved_on = lubridate::mdy(lease_approved_on),
    move_in_date = lubridate::mdy(move_in_date)
  ) |>
  # reorder fields logically
  dplyr::select(
    # property details
    property_id,
    property_name,
    square_footage,
    # unit details
    unit_number,
    floorplan,
    occupancy_status,
    number_of_bedrooms,
    number_of_bathrooms,
    # resident details
    resident_id,
    resident_name,
    resident_email,
    resident_phone_number,
    resident_gender,
    # lease details
    lease_id,
    lease_status,
    lease_sub_status,
    lease_term_months,
    lease_term_duration,
    lease_start_date,
    lease_end_date,
    move_in_date,
    leasing_agent_name,
    # financial details
    deposit_charged,
    deposit_held,
    budgeted_rent,
    advertised_rent,
    scheduled_rent_per_month,
    total_scheduled_rent,
    market_rent,
    actual_charges,
    # additional tracking
    lease_started_on, lease_partially_completed_on, lease_completed_on, lease_approved_on
  ) |>
  # replace blanks with NA
  dplyr::mutate(square_footage = ifelse(square_footage == 0, as.numeric(NA), as.numeric(square_footage))) |>
  dplyr::mutate_if(is.character, ~dplyr::na_if(.x, "")) |>
  dplyr::mutate(property_id = as.integer(property_id))


# check for duplicates
pre_lease_report_details_detected_duplicates <- pre_lease_report_details_data |>
  dplyr::distinct(
    lease_id,
    resident_id,
    .keep_all = TRUE
  ) |>
  dplyr::group_by(
    lease_id,
    resident_id
  ) |>
  dplyr::filter(dplyr::n() > 1)


pre_lease_report_details_grouped <- pre_lease_report_details_data |>
  dplyr::group_by(
    property_id,
    unit_number,
    lease_id
  ) |>
  dplyr::summarise(
    square_footage = max(square_footage, na.rm = TRUE),
    floorplan = dplyr::first(floorplan),
    occupancy_status = dplyr::first(occupancy_status),
    number_of_bedrooms = max(number_of_bedrooms, na.rm = TRUE),
    number_of_bathrooms = max(number_of_bathrooms, na.rm = TRUE),
    deposit_charged = sum(deposit_charged, na.rm = TRUE),
    deposit_held = sum(deposit_held, na.rm = TRUE),
    scheduled_rent_per_month = sum(scheduled_rent_per_month, na.rm = TRUE),
    total_scheduled_rent = sum(total_scheduled_rent, na.rm = TRUE),
    advertised_rent = max(advertised_rent, na.rm = TRUE),
    resident_count = n_distinct(resident_id),
    lease_start_date = min(as.Date(lease_start_date, "%Y-%m-%d")),
    lease_end_date = max(as.Date(lease_end_date, "%Y-%m-%d"))
  ) |>
  dplyr::ungroup()


aggregated_summary <- pre_lease_report_details_grouped |>
  dplyr::group_by(property_id, floorplan) |>
  dplyr::summarise(
    total_units = dplyr::n(),
    total_scheduled_rent = sum(total_scheduled_rent, na.rm = TRUE)
  ) |>
  dplyr::ungroup()

comparison <- pre_lease_report_summary_data |>
  dplyr::select(property_id, unit_type, units, total_scheduled_rent) |>
  dplyr::left_join(
    aggregated_summary,
    by = c("property_id", "unit_type" = "floorplan")
  )

mismatched_totals <- comparison |>
  dplyr::filter(
    units != total_units,
    abs(total_scheduled_rent.x - total_scheduled_rent.y) > 1
  )

mismatched_records <- pre_lease_report_details_data |>
  dplyr::filter(
    property_id %in% mismatched_totals$property_id
  )

for (i in 1:nrow(mismatched_totals)) {
  property_id <- mismatched_totals$property_id[i]
  unit_type <- mismatched_totals$unit_type[i]

  cat("\n### Investigating Property:", property_id, " | Unit Type:", unit_type, "###\n")

  # Filter details for the specific property and unit type
  details_subset <- mismatched_records %>%
    filter(property_id == !!property_id, floorplan == !!unit_type)

  # Summarize detailed view
  details_summary <- details_subset %>%
    group_by(property_id, unit_number, lease_id) %>%
    summarise(
      resident_count = n_distinct(resident_id),
      total_scheduled_rent = sum(scheduled_rent_per_month, na.rm = TRUE),
      .groups = "drop"
    )

  print(details_summary)
}

# Step 3: Flag potential duplicates or missing units
potential_issues <- mismatched_records %>%
  group_by(property_id, lease_id, resident_id) %>%
  summarise(count = n(), .groups = "drop") %>%
  filter(count > 1)

print("Potential Duplicates:")
print(potential_issues)

# Step 4: Adjust duplicates by keeping distinct rows
pre_lease_details_cleaned <- pre_lease_report_details_data %>%
  distinct(property_id, unit_number, lease_id, resident_id, .keep_all = TRUE)

# Step 5: Re-aggregate and validate
pre_lease_report_details_grouped <- pre_lease_details_cleaned %>%
  group_by(property_id, floorplan) %>%
  summarise(
    total_units = n_distinct(unit_number),
    total_scheduled_rent = sum(scheduled_rent_per_month, na.rm = TRUE),
    .groups = "drop"
  )

# Compare again with the summary table
comparison_fixed <- pre_lease_report_summary_data %>%
  select(property_id, unit_type, units, total_scheduled_rent) %>%
  left_join(pre_lease_report_details_grouped,
            by = c("property_id", "unit_type" = "floorplan"))

mismatched_totals_fixed <- comparison_fixed %>%
  filter(
    units != total_units |
      abs(total_scheduled_rent.x - total_scheduled_rent.y) > 1
  )

print("Remaining Mismatches After Cleaning:")
print(mismatched_totals_fixed)


pre_lease_report_summary_data |> readr::write_csv(
  "data-raw/data/working/2024-12-17-entrata_pre_lease_report_summary_by_unit.csv"
)

pre_lease_report_details_data |> readr::write_csv(
  "data-raw/data/working/2024-12-17-entrata_pre_lease_report_details_by_unit.csv"
)

# database

check_db_conn(conn)

DBI::dbExecute(
  conn,
  "DROP TABLE IF EXISTS \"entrata\".\"pre_lease_summary\";"
)

# create tables
DBI::dbExecute(
  conn,
  "CREATE TABLE IF NOT EXISTS \"entrata\".\"pre_lease_summary\" (
    property_id INT NOT NULL,
    property_name VARCHAR(255) NOT NULL,
    unit_type VARCHAR(50) NOT NULL,
    average_square_footage DECIMAL(10,2),
    average_advertised_rent DECIMAL(10,2),
    total_scheduled_rent DECIMAL(12,2),
    units INT,
    excluded_unit_count INT,
    rentable_unit_count INT,
    occupied_count INT,
    started_new_count INT,
    started_renewal_count INT,
    started_count INT,
    started_percent DECIMAL(5,2),
    partially_completed_new_count INT,
    partially_completed_renewal_count INT,
    partially_completed_count INT,
    partially_completed_percent DECIMAL(5,2),
    completed_new_count INT,
    completed_renewal_count INT,
    completed_count INT,
    completed_percent DECIMAL(5,2),
    approved_new_count INT,
    approved_renewal_count INT,
    approved_count INT,
    approved_percent DECIMAL(5,2),
    preleased_new_count INT,
    preleased_renewal_count INT,
    preleased_count INT,
    preleased_percent DECIMAL(5,2),
    variance DECIMAL(10,2),
    available_count INT,
    number_of_bedrooms DECIMAL(5,2),
    number_of_bathrooms DECIMAL(5,2),
    PRIMARY KEY (property_id, unit_type)
  )"
)

DBI::dbWriteTable(
  conn = conn,
  name = DBI::SQL("entrata.pre_lease_summary"),
  value = pre_lease_report_summary_data,
  append = TRUE
)

DBI::dbExecute(
  conn,
  "DROP TABLE IF EXISTS \"entrata\".\"pre_lease_details\";"
)

DBI::dbExecute(
  conn,
  "CREATE TABLE IF NOT EXISTS \"entrata\".\"pre_lease_details\" (
    property_id INT NOT NULL,
    property_name VARCHAR(255) NOT NULL,
    square_footage DECIMAL(10,2),
    unit_number VARCHAR(50),
    floorplan VARCHAR(50),
    occupancy_status VARCHAR(50),
    number_of_bedrooms DECIMAL(5,2),
    number_of_bathrooms DECIMAL(5,2),
    resident_id INT,
    resident_name VARCHAR(255),
    resident_email VARCHAR(255),
    resident_phone_number VARCHAR(20),
    resident_gender VARCHAR(10),
    lease_id VARCHAR(50) NOT NULL,
    lease_status VARCHAR(50),
    lease_sub_status VARCHAR(50),
    lease_term_months INT,
    lease_term_duration VARCHAR(100),
    lease_start_date DATE,
    lease_end_date DATE,
    move_in_date DATE,
    leasing_agent_name VARCHAR(255),
    deposit_charged DECIMAL(10,2),
    deposit_held DECIMAL(10,2),
    budgeted_rent DECIMAL(10,2),
    advertised_rent DECIMAL(10,2),
    scheduled_rent_per_month DECIMAL(10,2),
    total_scheduled_rent DECIMAL(12,2),
    market_rent DECIMAL(10,2),
    actual_charges DECIMAL(10,2),
    lease_started_on DATE,
    lease_partially_completed_on DATE,
    lease_completed_on DATE,
    lease_approved_on DATE
  )"
)

DBI::dbWriteTable(
  conn = conn,
  name = DBI::SQL("entrata.pre_lease_details"),
  value = pre_lease_report_details_data,
  append = TRUE
)

# current / old summary report table

old_db_summary_report <- dplyr::tbl(conn, I("public.summary_report")) |>
  dplyr::collect() |>
  setNames(
    c("property_name",
      "total_beds",
      "model_beds",
      "current_occupency",
      "total_new",
      "total_renewals",
      "total_leases",
      "prelease_percent",
      "prelease_percent",
      "prior_total_new",
      "prior_total_renewals",
      "prior_total_leases",
      "prior_prelease_percent",
      "yoy_variance_1",
      "yoy_variance_2",
      "seven_new",
      "seven_renewal",
      "seven_total",
      "seven_percent_gained",
      "beds_left",
      "vel_90",
      "vel_95",
      "vel_100")
  )

dplyr::glimpse(old_db_summary_report)

dplyr::glimpse(pre_lease_report_summary_data)

# create a function that processes the "raw" pre_lease report's summary table into
# necessary structure summarized over each property:

dplyr::glimpse(report_summary_data)

# Rows: 351
# Columns: 53
# $ property_id                             <chr> "739076", "739076", "739076", "739076", "739076", "739076", "739076", "739076", "739076", "739076", "7390…
# $ property_name                           <chr> "1008 S. 4th", "1008 S. 4th", "1008 S. 4th", "1008 S. 4th", "1008 S. 4th", "1008 S. 4th", "1008 S. 4th", …
# $ property                                <chr> "1008 S. 4th", "1008 S. 4th", "1008 S. 4th", "1008 S. 4th", "1008 S. 4th", "1008 S. 4th", "1008 S. 4th", …
# $ unit_type                               <chr> "B1", "B2", "B3", "B4", "B5", "C1", "D1", "D1 Balcony", "D2", "D3 Balcony", "M1 Murphy", "M1 Murphy Balco…
# $ unit_type_subtotal                      <chr> "B1", "B2", "B3", "B4", "B5", "C1", "D1", "D1 Balcony", "D2", "D3 Balcony", "M1 Murphy", "M1 Murphy Balco…
# $ avg_sqft                                <dbl> 316.0000, 315.0000, 377.0000, 462.0000, 464.0000, 328.0000, 276.0000, 276.0000, 306.0000, 345.0000, 321.0…
# $ avg_advertised_rate                     <dbl> 1000.0000, 1000.0000, 1161.3636, 1140.0000, 1150.0000, 937.5000, 858.3333, 880.0000, 899.2708, 983.7500, …
# $ units                                   <int> 8, 8, 10, 2, 2, 12, 16, 16, 48, 12, 4, 12, 4, 4, 0, 25, 12, 51, 8, 10, 29, 4, 11, 5, 4, 24, 40, 2, 2, 12,…
# $ excluded_unit_count                     <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
# $ rentable_unit_count                     <int> 8, 8, 10, 2, 2, 12, 16, 16, 48, 12, 4, 12, 4, 4, 0, 25, 12, 51, 8, 10, 29, 4, 11, 5, 4, 24, 40, 2, 2, 12,…
# $ avg_scheduled_rent                      <dbl> 1006.8750, 1025.0000, 1164.0909, 1187.5000, 1197.5000, 940.4167, 869.6667, 875.0000, 926.9792, 1006.2500,…
# $ occupied_count                          <int> 8, 8, 9, 2, 2, 12, 16, 16, 48, 10, 4, 12, 4, 4, 0, 24, 11, 46, 8, 9, 29, 3, 4, 4, 2, 23, 40, 2, 2, 12, 12…
# $ started_new_count_prior                 <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
# $ started_new_count                       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
# $ started_renewal_count_prior             <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
# $ started_renewal_count                   <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, …
# $ started_count_prior                     <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
# $ started_count                           <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, …
# $ started_percent                         <dbl> 0.00000000, 0.00000000, 0.00000000, 0.00000000, 0.00000000, 0.00000000, 0.00000000, 0.00000000, 0.0000000…
# $ partially_completed_new_count_prior     <int> 1, 2, 2, 1, 0, 4, 3, 3, 9, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 2, 1, 1, 4, 1, 2, …
# $ partially_completed_new_count           <int> 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, …
# $ partially_completed_renewal_count_prior <int> 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
# $ partially_completed_renewal_count       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
# $ partially_completed_count_prior         <int> 1, 2, 2, 1, 0, 4, 3, 3, 10, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 2, 1, 1, 4, 1, 2,…
# $ partially_completed_count               <int> 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, …
# $ partially_completed_percent             <dbl> 0.00000000, 0.12500000, 0.10000000, 0.00000000, 0.00000000, 0.00000000, 0.00000000, 0.00000000, 0.0208333…
# $ completed_new_count_prior               <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, …
# $ completed_new_count                     <int> 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
# $ completed_renewal_count_prior           <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
# $ completed_renewal_count                 <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
# $ completed_count_prior                   <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, …
# $ completed_count                         <int> 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
# $ completed_percent                       <dbl> 0.00000000, 0.00000000, 0.00000000, 0.00000000, 0.00000000, 0.00000000, 0.00000000, 0.00000000, 0.0208333…
# $ approved_new_count_prior                <int> 3, 1, 4, 0, 2, 5, 4, 7, 22, 7, 2, 8, 2, 2, 0, 3, 0, 0, 1, 0, 1, 0, 0, 0, 0, 4, 17, 0, 0, 7, 8, 2, 4, 3, 5…
# $ approved_new_count                      <int> 5, 7, 5, 2, 2, 3, 10, 12, 25, 12, 2, 7, 3, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 25, 2, 1, 8, 12, 13, 10…
# $ approved_renewal_count_prior            <int> 3, 3, 4, 1, 0, 3, 9, 4, 14, 2, 0, 2, 1, 2, 0, 8, 4, 10, 2, 3, 7, 1, 2, 0, 1, 3, 15, 2, 2, 0, 3, 13, 7, 0,…
# $ approved_renewal_count                  <int> 3, 0, 5, 0, 0, 9, 5, 4, 21, 0, 2, 4, 1, 1, 0, 4, 2, 7, 0, 1, 4, 0, 1, 1, 0, 5, 10, 0, 1, 1, 0, 3, 6, 4, 0…
# $ approved_count_prior                    <int> 6, 4, 8, 1, 2, 8, 13, 11, 36, 9, 2, 10, 3, 4, 0, 11, 4, 10, 3, 3, 8, 1, 2, 0, 1, 7, 32, 2, 2, 7, 11, 15, …
# $ approved_count                          <int> 8, 7, 10, 2, 2, 12, 15, 16, 46, 12, 4, 11, 4, 4, 0, 4, 2, 7, 0, 1, 4, 0, 1, 1, 0, 9, 35, 2, 2, 9, 12, 16,…
# $ approved_percent                        <dbl> 1.00000000, 0.87500000, 1.00000000, 1.00000000, 1.00000000, 1.00000000, 0.93750000, 1.00000000, 0.9583333…
# $ preleased_new_count_prior               <int> 4, 3, 6, 1, 2, 9, 7, 10, 31, 8, 3, 10, 3, 2, 0, 5, 0, 2, 1, 0, 1, 0, 0, 0, 0, 4, 23, 0, 0, 10, 9, 3, 8, 4…
# $ preleased_new_count                     <int> 5, 8, 6, 2, 2, 3, 10, 12, 27, 12, 2, 7, 3, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 26, 2, 1, 9, 12, 13, 10…
# $ preleased_renewal_count_prior           <int> 3, 3, 4, 1, 0, 3, 9, 4, 15, 2, 0, 2, 1, 2, 0, 8, 4, 10, 2, 3, 7, 1, 2, 0, 1, 3, 15, 2, 2, 0, 3, 13, 7, 0,…
# $ preleased_renewal_count                 <int> 3, 0, 5, 0, 0, 9, 5, 4, 21, 0, 2, 4, 1, 1, 0, 5, 2, 9, 0, 1, 4, 0, 1, 1, 0, 5, 11, 0, 1, 1, 0, 3, 6, 4, 0…
# $ preleased_count_prior                   <int> 7, 6, 10, 2, 2, 12, 16, 14, 46, 10, 3, 12, 4, 4, 0, 13, 4, 12, 3, 3, 8, 1, 2, 0, 1, 7, 38, 2, 2, 10, 12, …
# $ preleased_count                         <int> 8, 8, 11, 2, 2, 12, 15, 16, 48, 12, 4, 11, 4, 4, 0, 5, 2, 9, 0, 1, 4, 0, 1, 1, 0, 9, 37, 2, 2, 10, 12, 16…
# $ preleased_percent_prior                 <dbl> 0.8750000, 0.7500000, 1.0000000, 1.0000000, 1.0000000, 1.0000000, 1.0000000, 0.8750000, 0.9583333, 0.8333…
# $ preleased_percent                       <dbl> 1.00000000, 1.00000000, 1.10000000, 1.00000000, 1.00000000, 1.00000000, 0.93750000, 1.00000000, 1.0000000…
# $ variance                                <dbl> 0.12500000, 0.25000000, 0.10000000, 0.00000000, 0.00000000, 0.00000000, -0.06250000, 0.12500000, 0.041666…
# $ available_count                         <int> 0, 0, -1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 20, 10, 42, 8, 9, 25, 4, 10, 4, 4, 15, 3, 0, 0, 2, 0, 0, 0,…
# $ scheduled_rent_total                    <dbl> 8055, 8200, 12805, 2375, 2395, 11285, 13045, 14000, 44495, 12075, 4835, 13710, 5415, 5520, 0, 14250, 5830…
# $ number_of_bedrooms                      <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "0", "0", "0", "0", "0", NA, "0", "0", "0", "…
# $ number_of_bathrooms                     <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "1", "1", "1", "1", "1", "1", "1", "1", "1", …

process_pre_lease_summary_data <- function(summary_data) {

  sum_cols <- c(
    "approved_count",
    "completed_count",
    "completed_renewal_count",
    "partially_completed_count",
    "partially_completed_new_count",
    "partially_completed_renewal_count",
    "preleased_count",
    "preleased_new_count",
    "preleased_renewal_count"
  )

  weeks_left_to_lease <- get_weeks_left_to_lease()

  summary_data |>
    dplyr::group_by(
      property_id,
      property_name
    ) |>
    summarise(
      total_beds = sum(available_count, na.rm = TRUE),
      model_beds = 0,
      current_occupied = sum(occupied_count, na.rm = TRUE),
      approved_count = sum(approved_count, na.rm = TRUE),
      completed_count = sum(completed_count, na.rm = TRUE),
      completed_renewal_count = sum(completed_renewal_count, na.rm = TRUE),
      partially_completed_count = sum(partially_completed_count, na.rm = TRUE),
      partially_completed_new_count = sum(partially_completed_new_count, na.rm = TRUE),
      partially_completed_renewal_count = sum(partially_completed_renewal_count, na.rm = TRUE),
      preleased_count = sum(preleased_count, na.rm = TRUE),
      preleased_new_count = sum(preleased_new_count, na.rm = TRUE),
      preleased_renewal_count = sum(preleased_renewal_count, na.rm = TRUE),
      approved_new_count_prior = sum(approved_new_count_prior, na.rm = TRUE),
      partially_completed_new_count_prior = sum(partially_completed_new_count_prior, na.rm = TRUE),
      completed_new_count_prior = sum(completed_new_count_prior, na.rm = TRUE),
      approved_renewal_count_prior = sum(approved_renewal_count_prior, na.rm = TRUE),
      partially_completed_renewal_count_prior = sum(partially_completed_renewal_count_prior, na.rm = TRUE),
      completed_renewal_count_prior = sum(completed_renewal_count_prior, na.rm = TRUE),
      approved_count_prior = sum(approved_count_prior, na.rm = TRUE),
      partially_completed_count_prior = sum(partially_completed_count_prior, na.rm = TRUE),
      completed_count_prior = sum(completed_count_prior, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      leases_count = rowSums(across(all_of(sum_cols)), na.rm = TRUE),
      current_occupency = current_occupied / total_beds,
      total_new = approved_count + partially_completed_new_count + completed_count,
      total_renewals = partially_completed_renewal_count + preleased_renewal_count + completed_renewal_count,
      total_leases = total_new + total_renewals,
      prelease_percent = total_leases / total_beds,
      prior_total_new = approved_new_count_prior + partially_completed_new_count_prior + completed_new_count_prior,
      prior_total_renewals = approved_renewal_count_prior + partially_completed_renewal_count_prior + completed_renewal_count_prior,
      prior_total_leases = approved_count_prior + partially_completed_count_prior + completed_count_prior,
      prior_prelease_percent = prior_total_leases / total_beds,
      yoy_variance_1 = total_leases - prior_total_leases,
      yoy_variance_2 = prelease_percent - prior_prelease_percent,
      seven_new = 0,
      seven_renewal = 0,
      seven_total = seven_new + seven_renewal,
      seven_percent_gained = seven_total / total_beds,
      beds_left = total_beds - total_leases,
      leased_this_week = seven_total,
      vel_90 = beds_left * 0.9 / weeks_left_to_lease,
      vel_95 = beds_left * 0.95 / weeks_left_to_lease,
      vel_100 = beds_left * 1.0 / weeks_left_to_lease
    ) |>
    select(
      property_id,
      property_name,
      leases_count,
      total_beds,
      model_beds,
      current_occupied,
      current_occupency,
      total_new,
      total_renewals,
      total_leases,
      prelease_percent,
      prior_total_new,
      prior_total_renewals,
      prior_total_leases,
      prior_prelease_percent,
      yoy_variance_1,
      yoy_variance_2,
      seven_new,
      seven_renewal,
      seven_total,
      seven_percent_gained,
      beds_left,
      leased_this_week,
      vel_90,
      vel_95,
      vel_100
    )

}

# group pre_lease_summary report table by property
report_summary_data_processed <- report_summary_data |>
  process_pre_lease_summary_data()


#######################

leasing_season_end_date <- get_leasing_period_end_date()
weeks_left_to_lease <- get_weeks_left_to_lease(leasing_season_end_date)
weeks_left_to_lease
# 33

sum_cols <- c(
  "approved_count",
  "completed_count",
  "completed_renewal_count",
  "partially_completed_count",
  "partially_completed_new_count",
  "partially_completed_renewal_count",
  "preleased_count",
  "preleased_new_count",
  "preleased_renewal_count"
)

pre_lease_summary_tbl <- pre_lease_raw_response_summary_data |>
  tibble::as_tibble() |>
  dplyr::mutate(
    leases_count = rowSums(
      dplyr::across(dplyr::all_of(sum_cols)), na.rm = TRUE
    ),
    total_beds = .data$available_count,
    model_beds = 0,
    current_occupied = .data$occupied_count,
    current_occupency = .data$occupied_count / .data$total_beds,
    total_new = .data$approved_new_count + .data$partially_completed_new_count + .data$completed_new_count,
    total_renewals = .data$approved_renewal_count + .data$partially_completed_renewal_count + .data$completed_renewal_count,
    total_leases = .data$total_new + .data$total_renewals, # leases_count,
    prelease_percent = .data$approved_percent,
    # prelease_percent = units / approved_count, # total beds / total leases
    prior_total_new = .data$approved_new_count_prior + .data$partially_completed_new_count_prior + .data$completed_new_count_prior,
    prior_total_renewals = .data$approved_renewal_count_prior + .data$partially_completed_renewal_count_prior + .data$completed_renewal_count_prior,
    prior_total_leases = .data$approved_count_prior + .data$partially_completed_count_prior + .data$completed_count_prior,
    prior_prelease_percent = .data$prior_total_leases / .data$total_beds,
    yoy_variance_1 = .data$total_leases - .data$prior_total_leases,
    yoy_variance_2 = .data$prelease_percent - .data$prior_prelease_percent,
    seven_new = 0,
    seven_renewal = 0,
    seven_total = .data$seven_new + .data$seven_renewal,
    seven_percent_gained = .data$seven_total / .data$total_beds,
    beds_left = .data$total_beds - .data$total_leases,
    leased_this_week = .data$seven_total,
    vel_90 = .data$beds_left * .9 / .env$weeks_left_to_lease,
    vel_95 = .data$beds_left * .95 / .env$weeks_left_to_lease,
    vel_100 = .data$beds_left * 1 / .env$weeks_left_to_lease
  ) |>
  dplyr::select(
    property_id,
    property_name,
    unit_type = units,
    leases_count,
    total_beds,
    model_beds,
    current_occupied,
    current_occupency,
    total_new,
    total_renewals,
    total_leases,
    prelease_percent,
    prior_total_new,
    prior_total_renewals,
    prior_total_leases,
    prior_prelease_percent,
    yoy_variance_1,
    yoy_variance_2,
    seven_new,
    seven_renewal,
    seven_total,
    seven_percent_gained,
    beds_left,
    leased_this_week,
    vel_90,
    vel_95,
    vel_100
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


commonwealth_report_details_data <- dplyr::filter(
  report_details_data,
  property_id == "739085"
)
