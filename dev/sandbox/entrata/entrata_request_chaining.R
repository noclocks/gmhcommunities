library(httr2)
library(jsonlite)
library(purrr)
library(dplyr)

chain_requests <- function(initial_req, ...) {
  chain_steps <- list(...)

  perform_step <- function(prev_resp, step) {
    next_req <- step(prev_resp)
    if (is.null(next_req)) return(prev_resp)
    req_perform(next_req)
  }

  initial_resp <- req_perform(initial_req)
  Reduce(perform_step, chain_steps, init = initial_resp)
}

step_get_queue_id <- function(resp) {
  body <- resp_body_json(resp)
  queue_id <- body$queue_id

  request(entrata_config$base_url) |>
    req_url_path_append("queue") |>
    req_method("POST") |>
    req_body_json(
      entrata_req_body(
        method_name = "getQueue",
        method_version = "r1",
        method_params = list(queueId = queue_id)
      )
    )
}

step_check_status <- function(resp) {
  body <- resp_body_json(resp)

  if (body$status == "completed") {
    return(NULL)  # End the chain
  } else {
    # Return the same request to poll again
    resp_request(resp)
  }
}

entrata_config <- config::get("entrata")

base_url <- entrata_config$base_url
username <- entrata_config$username
password <- entrata_config$password

req <- request(base_url) |>
  req_auth_basic(username, password) |>
  req_headers("Content-Type" = "application/json") |>
  req_headers("Accept" = "application/json") |>
  req_method("POST") |>
  req_error(is_error = function(resp) { FALSE },
            body = function(resp) { resp_body_json(resp)$error }) |>
  req_body_json(
    list(
      auth = list(type = "basic"),
      requestId = 999
    )
  )

entrata_req_body <- function(
    request_id = NULL,
    method_name = NULL,
    method_version = NULL,
    method_params = list(NULL),
    ...
) {

  # check_endpoint(endpoint)
  # check_endpoint_method(endpoint, method_name)
  # check_method_version(method_name, method_version)

  list(
    auth = list(
      type = "basic"
    ),
    requestId = request_id,
    method = list(
      name = method_name,
      version = method_version,
      params = method_params
    )
  ) |>
    purrr::compact()

}

entrata_request <- function() {
  entrata_config <- config::get("entrata")

  base_url <- entrata_config$base_url
  username <- entrata_config$username
  password <- entrata_config$password

  request(base_url) |>
    req_auth_basic(username, password) |>
    req_headers("Content-Type" = "application/json") |>
    req_headers("Accept" = "application/json") |>
    req_method("POST") |>
    req_error(is_error = function(resp) { FALSE },
              body = function(resp) { resp_body_json(resp)$error }) |>
    req_body_json(
      entrata_req_body(
        method_name = "getStatus",
        method_version = "r1"
      )
    )
}

check_status <- function() {

  entrata_request() |>
    req_url_path_append("status") |>
    req_perform() |>
    resp_body_json() |>
    pluck("response", "result")
}

check_status(req)

get_properties <- function(req) {
  req |>
    req_url_path_append("properties") |>
    req_method("POST") |>
    req_body_json(list(method = "getProperties")) |>
    req_perform() |>
    resp_body_json()
}

get_leases <- function(req, property_id) {
  req |>
    req_url_path_append("leases") |>
    req_method("POST") |>
    req_body_json(list(
      method = "getLeases",
      params = list(propertyId = property_id)
    )) |>
    req_perform() |>
    resp_body_json()
}

request_report <- function(req, report_name, filter_params) {
  req |>
    req_url_path_append("reports") |>
    req_method("POST") |>
    req_body_json(list(
      method = "getReportData",
      params = c(list(report = report_name), filter_params)
    )) |>
    req_perform() |>
    resp_body_json()
}

check_queue <- function(req, queue_id) {
  req |>
    req_url_path_append("queue") |>
    req_method("POST") |>
    req_body_json(list(queue_id = queue_id)) |>
    req_perform() |>
    resp_body_json()
}
