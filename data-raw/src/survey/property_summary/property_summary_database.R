property_summary_data_for_db <- market_survey_property_summary_data |>
  dplyr::rename(
    year_built = year_built_or_renovated,
    most_recent_sale = date_of_most_recent_sale
  )

con <- db_connect()

DBI::dbWriteTable(
  conn = con,
  name = DBI::SQL("mkt.property_summary"),
  value = property_summary_data_for_db,
  append = TRUE
  # overwrite = FALSE
)

