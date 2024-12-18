leasing_summary_data <- readr::read_csv(
  pkg_sys("extdata/survey/leasing_summary_data.csv")
)

pool <- db_connect()
conn <- pool_checkout(pool)

leasing_weeks_db_data <- dplyr::tbl(conn, I("mkt.leasing_weeks")) |>
  dplyr::collect() |>
  dplyr::arrange(leasing_week_id)

current_week_id <- db_get_weekly_period_id(conn)

week_ids <- db_get_weekly_period_id(conn, as_of_date = leasing_summary_data$leasing_week[1]) |>
  rep(4)

db_drop_tbl(conn, "leasing_summary", "mkt", cascade = TRUE)

db_drop_tbl(conn, tbl = "surveys", schema = "mkt", cascade = TRUE)

DBI::dbExecute(
  conn,
  'CREATE TABLE mkt.leasing_summary (
  property_id TEXT REFERENCES mkt.properties(property_id),
  leasing_week_id INT REFERENCES mkt.leasing_weeks(leasing_week_id),
  property_name TEXT NOT NULL,
  leasing_week DATE NOT NULL,
  reporting_cycle TEXT CHECK (reporting_cycle IN (\'Monday-Sunday\', \'Saturday-Friday\', \'Sunday-Saturday\')) DEFAULT \'Monday-Sunday\',
  lease_launch_date DATE,
  renewal_launch_date DATE,
  current_occupancy DECIMAL(5,2),
  last_year_occupancy DECIMAL(5,2),
  current_pre_lease DECIMAL(5,2),
  last_year_pre_lease DECIMAL(5,2),
  total_renewals INT,
  total_new_leases INT,
  total_leases_weekly INT,
  traffic_weekly INT,
  current_incentive TEXT DEFAULT \'None\',
  incentive_amount DECIMAL(5,2),
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);'
)

leasing_summary_data_for_db <- leasing_summary_data |>
  dplyr::mutate(
    leasing_week_id = week_ids,
  ) |>
  dplyr::select(
    property_id,
    leasing_week_id,
    property_name,
    leasing_week,
    reporting_cycle,
    lease_launch_date,
    renewal_launch_date,
    current_occupancy,
    last_year_occupancy,
    current_pre_lease,
    last_year_pre_lease,
    total_renewals,
    total_new_leases,
    total_leases_weekly,
    traffic_weekly,
    current_incentive,
    incentive_amount
  )

dplyr::glimpse(leasing_summary_data_for_db)

DBI::dbWriteTable(
  conn,
  DBI::SQL("mkt.leasing_summary"),
  leasing_summary_data_for_db,
  append = TRUE
)
