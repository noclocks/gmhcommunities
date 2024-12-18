pool <- db_connect()
conn <- pool_checkout(pool)

office_hours_data <- readr::read_csv(
  pkg_sys("extdata/survey/office_hours_data.csv"),
)

dplyr::glimpse(office_hours_data)

DBI::dbWriteTable(
  conn,
  DBI::SQL("mkt.office_hours"),
  office_hours_data,
  overwrite = TRUE
)
