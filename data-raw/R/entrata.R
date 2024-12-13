entrata_base_req <- function() {

  httr2::request(entrata_config$base_url) |>
    httr2::req_method("POST") |>
    httr2::req_auth_basic(
      username = entrata_config$username,
      password = entrata_config$password
    ) |>
    httr2::req_headers(
      `Content-Type` = "application/json; charset=utf-8",
      `Accept` = "application/json"
    ) |>
    httr2::req_user_agent("gmhcommunities/1.0")

}

entrata_cache <- function() {
  cachem::cache_disk(
    dir = "data-raw/cache/entrata",
    read_fn = qs2::qs_read,
    write_fn = qs2::qs_save,
    extension = ".qs",
    logfile = "data-raw/cache/entrata.log"
  )
}

entrata_properties <- function() {

  properties_req <- entrata_base_req() |>
    httr2::req_url_path_append("properties") |>
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
    )

  properties_req

  properties_req |> httr2::req_dry_run()

  properties_resp <- httr2::req_perform(properties_req)

  httr2::resp_check_status(properties_resp)

  properties_resp |>
    httr2::resp_body_json()
    # purrr::pluck("response", "result", "PhysicalProperty", "Property") |>
    # jsonlite::toJSON(pretty = TRUE, auto_unbox = TRUE) |>
    # jsonlite::fromJSON() |>
    # tibble::as_tibble() |>
    # dplyr::select(
    #   property_id = PropertyID,
    #   property_name = MarketingName
    # )

}

mem_entrata_properties <- memoise::memoise(
  entrata_properties,
  cache = entrata_cache()
)

entrata_leases <- function(property_id) {

  leases_req <- entrata_base_req() |>
    httr2::req_url_path_append("leases") |>
    httr2::req_body_json(
      list(
        auth = list(type = "basic"),
        requestId = 15,
        method = list(
          name = "getLeases",
          version = "r2",
          params = list(
            propertyId = property_id,
            leaseExpiringDateFrom = "08/31/2023",
            leaseStatusTypeIds = "3,4,5,6"
          )
        )
      )
    ) |>
    httr2::req_url_query(
      page_no = 1,
      per_page = 1000,
      .multi = "explode"
    )

  leases_req

  leases_req |> httr2::req_dry_run()

  leases_resp <- httr2::req_perform(leases_req)

  httr2::resp_check_status(leases_resp)

  leases_resp |>
    httr2::resp_body_json() |>
    purrr::pluck("response", "result", "leases", "lease") |>
    jsonlite::toJSON(pretty = TRUE, auto_unbox = TRUE) |>
    jsonlite::fromJSON() |>
    tibble::as_tibble()

}

mem_entrata_leases <- memoise::memoise(
  entrata_leases,
  cache = entrata_cache()
)

