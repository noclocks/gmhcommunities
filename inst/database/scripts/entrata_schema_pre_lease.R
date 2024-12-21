
pkgload::load_all()

pool <- db_connect()
conn <- pool::poolCheckout(pool)
connections::connection_view(conn)

on.exit({
  pool::poolReturn(conn, pool)
  pool::poolClose(pool)
})


# pre-lease report --------------------------------------------------------

pre_lease_data_lst_32 <- entrata_pre_lease_report(consider_pre_leased_on = 32)
pre_lease_data_lst_332 <- entrata_pre_lease_report(consider_pre_leased_on = "332")

pre_lease_summary_32 <- pre_lease_data_lst_32$summary
pre_lease_summary_332 <- pre_lease_data_lst_332$summary

available_counts <- pre_lease_summary_332$available_count

pre_lease_summary_data <- pre_lease_summary_32 |>
  dplyr::mutate(
    available_count = available_counts
  )

pre_lease_details_data <- pre_lease_data_lst_32$details
pre_lease_params <- pre_lease_data_lst_32$parameters

csv_path <- "data-raw/data/working/entrata/"

summary_csv_file <- paste0(
  format(pre_lease_params$report_date, "%Y-%m-%d"),
  "-entrata_pre_lease_report_summary.csv"
)

details_csv_file <- paste0(
  format(pre_lease_params$report_date, "%Y-%m-%d"),
  "-entrata_pre_lease_report_details.csv"
)

readr::write_csv(pre_lease_summary_data, file.path(csv_path, summary_csv_file))
readr::write_csv(pre_lease_details_data, file.path(csv_path, details_csv_file))

dbx::dbxUpsert(
  conn,
  table = DBI::SQL("entrata.pre_lease_summary"),
  records = pre_lease_summary_data,
  where_cols = c("report_date", "property_id"),
  skip_existing = FALSE
)

db_pre_lease_summary_data <- dplyr::tbl(pool, I("entrata.pre_lease_summary")) |>
  dplyr::filter(report_date == max(.data$report_date, na.rm = TRUE)) |>
  dplyr::collect()

dbx::dbxUpsert(
  conn,
  table = DBI::SQL("entrata.pre_lease_details"),
  records = pre_lease_details_data,
  where_cols = c("report_date", "property_id"),
  returning = c("id"),
  skip_existing = FALSE
)

DBI::dbWriteTable(
  conn = conn,
  name = DBI::SQL("entrata.pre_lease_details"),
  value = pre_lease_details_data,
  overwrite = TRUE
)

db_pre_lease_details_data <- dplyr::tbl(pool, I("entrata.pre_lease_details")) |>
  # dplyr::filter(report_date == max(.data$report_date, na.rm = TRUE)) |>
  dplyr::collect()

# weekly ------------------------------------------------------------------

weekly_summary_data <- entrata_lease_execution_report() |>
  dplyr::select(
    "report_date",
    "property_id",
    "weekly_new",
    "weekly_renewal"
  )

dbx::dbxUpsert(
  conn,
  table = DBI::SQL("entrata.pre_lease_weekly"),
  records = weekly_summary_data,
  where_cols = c("report_date", "property_id"),
  skip_existing = FALSE
)


# gmh ---------------------------------------------------------------------

gmh_pre_lease_summary_tbl <- report_summary_data |>
  dplyr::left_join(
    model_beds,
    by = "property_id"
  ) |>
  dplyr::left_join(
    weekly_pre_lease,
    by = "property_id"
  ) |>
  dplyr::left_join(
    inv_partners,
    by = "property_id"
  ) |>
  dplyr::transmute(
    report_date = .env$report_date,
    property_id = .data$property_id,
    property_name = .data$property_name,
    investment_partner = .data$investment_partner,
    total_beds = .data$available_count, # units
    model_beds = .data$model_beds,
    current_occupied = .data$occupied_count,
    current_occupancy = .data$occupied_count / .data$total_beds,
    current_total_new = .data$approved_new_count + .data$partially_completed_new_count + .data$completed_new_count,
    current_total_renewals = .data$approved_renewal_count + .data$partially_completed_renewal_count + .data$completed_renewal_count,
    current_total_leases = .data$current_total_new + .data$current_total_renewals,
    current_preleased_percent = .data$current_total_leases / .data$total_beds,
    prior_total_new = .data$approved_new_count_prior + .data$partially_completed_new_count_prior + .data$completed_new_count_prior,
    prior_total_renewals = .data$approved_renewal_count_prior + .data$partially_completed_renewal_count_prior + .data$completed_renewal_count_prior,
    prior_total_leases = .data$approved_count_prior + .data$partially_completed_count_prior + .data$completed_count_prior,
    prior_preleased_percent = .data$prior_total_leases / .data$total_beds,
    yoy_variance_count = .data$current_total_leases - .data$prior_total_leases,
    yoy_variance_percent = .data$current_preleased_percent - .data$prior_preleased_percent,
    weekly_new = .data$weekly_new,
    weekly_renewal = .data$weekly_renewal,
    weekly_total = .data$weekly_total,
    weekly_percent_gained = .data$weekly_total / .data$total_beds,
    beds_left = .data$total_beds - .data$current_total_leases,
    vel_90 = .data$beds_left * .9 / .env$weeks_left_to_lease,
    vel_95 = .data$beds_left * .95 / .env$weeks_left_to_lease,
    vel_100 = .data$beds_left * 1 / .env$weeks_left_to_lease
  )

DBI::dbWriteTable(
  conn = conn,
  name = DBI::SQL("gmh.pre_lease_summary"),
  value = gmh_pre_lease_summary_tbl,
  append = TRUE
)

db_gmh_pre_lease_summary_tbl <- dplyr::tbl(pool, I("gmh.pre_lease_summary")) |> dplyr::collect()
