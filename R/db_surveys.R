
.mktsurvey_sections <- c(
  "property_summary",
  "leasing_summary",
  "amenities",
  "fees",
  "hours",
  "notes",
  "parking",
  "rents",
  "short_term_leases",
  "utilities"
)

db_get_weekly_period_id <- function(conn, as_of_date = lubridate::today()) {

  check_db_conn(conn)

  start_date <- get_weekly_period_start_date(as_of_date)

  # query data
  qry <- glue::glue(
    "SELECT leasing_week_id FROM \"mkt\".\"leasing_weeks\" WHERE start_date = \'{start_date}\'"
  )

  pool::dbGetQuery(conn, qry) |>
    dplyr::pull("leasing_week_id")

}

db_get_mkt_map_locations <- function(conn, property_id) {

  check_db_conn(conn)

  property_id <- as.character(property_id)

  competitor_property_ids <- dplyr::tbl(conn, I("mkt.competitors")) |>
    dplyr::select(
      competitor_id,
      associated_property_id
    ) |>
    dplyr::filter(
      associated_property_id == property_id
    ) |>
    dplyr::pull("competitor_id")

  prop_ids <- c(property_id, competitor_property_ids)

  dplyr::tbl(conn, I("mkt.locations")) |>
    dplyr::filter(property_id %in% prop_ids) |>
    dplyr::select(
      property_name,
      address,
      phone_number,
      property_image,
      latitude,
      longitude,
      gmaps_address,
      gmaps_rating,
      gmaps_num_of_reviews,
      website = gmaps_website,
      gmaps_url,
      map_layer:map_popup_html
    ) |>
    dplyr::collect()

}


db_get_survey_section_data <- function(conn, section = "property_summary", ...) {

  rlang::arg_match0(section, .mktsurvey_sections)
  check_db_conn(conn)

  # query data
  data <- pool::dbGetQuery(
    conn,
    glue::glue(
      "SELECT * FROM \'mkt\'.\'{section}\'"
    )
  )

  return(data)

}



get_last_survey_response <- function(conn, ...) {



}


submit_survey_data <- function(pool, property_id, survey_data) {

  check_db_conn(pool)

  # split data out by section
  property_summary_data <- purrr::pluck(survey_data, "property_summary")

  pool::poolWithTransaction(pool, {

    # update property summary
    if (nrow(property_summary_data) > 0) {
      pool::dbExecute(
        pool,
        "INSERT INTO \'mkt\'.\'property_summary\' (
          property_id,
          ...
        ) VALUES (
          ?,
          ...
        ) ON CONFLICT (property_id) DO UPDATE
        SET ... = EXCLUDED...
        ", list(property_id)
      )
    }

  })

}
