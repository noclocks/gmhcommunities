pool <- db_connect()
conn <- pool_checkout(pool)

rents_by_floorplan_data <- readr::read_csv(
  pkg_sys("extdata/survey/rents_floorplan_data.csv")
) |>
  dplyr::mutate(
    leasing_week_id = 50,
    leasing_week_start = get_weekly_period_start_date(as_of_date = lubridate::today() - lubridate::days(6))
  ) |>
  dplyr::select(
    property_id,
    leasing_week_id,
    property_name,
    leasing_week_start,
    floorplan_type:bundled_rent_per_sf
  )

dplyr::glimpse(rents_by_floorplan_data)

DBI::dbWriteTable(
  conn,
  DBI::SQL("mkt.rents_by_floorplan"),
  rents_by_floorplan_data,
  overwrite = TRUE
)

avg_rents_by_unit_type_data <- readr::read_csv(
  pkg_sys("extdata/survey/average_rents_by_unit_type_data.csv")
) |>
  dplyr::mutate(
    leasing_week_id = 50,
    leasing_week_start = get_weekly_period_start_date(as_of_date = lubridate::today() - lubridate::days(6))
  ) |>
  dplyr::select(
    property_id,
    leasing_week_id,
    property_name,
    leasing_week_start,
    floorplan_type:property_floorplan_bundled_rent_per_sf
  )

dplyr::glimpse(avg_rents_by_unit_type_data)

DBI::dbWriteTable(
  conn,
  DBI::SQL("mkt.avg_rents_by_unit_type"),
  avg_rents_by_unit_type_data,
  overwrite = TRUE
)
