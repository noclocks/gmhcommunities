entrata_base_req_proxied <- function() {

  httr2::request(entrata_config$base_url) |>
    httr2::req_auth_basic(entrata_config$username, entrata_config$password) |>
    httr2::req_method("POST") |>
    httr2::req_headers(`Content-Type` = "application/json") |>
    httr2::req_user_agent("gmhdatahub/0.1.0 (https://github.com/noclocks/gmhdatahub)") |>
    httr2::req_progress() |>
    httr2::req_body_json(
      list(
        auth = list(
          type = "basic"
        ),
        requestId = 15,
        method = list(
          name = NULL,
          version = "r1",
          params = list(NULL)
        )
      )
    ) |>
    httr2::req_proxy(
      url = "127.0.0.1",
      port = 8080,
      username = entrata_config$username,
      password = entrata_config$password,
      auth = "basic"
    )

}

entrata_req_proxy_perform <- function(
    endpoint,
    method_name,
    method_version,
    method_params,
    request_id = 15
) {

  req <- entrata_base_req_proxied() |>
    httr2::req_url_path_append(endpoint)

  req$body$data$requestId <- request_id
  req$body$data$method$name <- method_name
  req$body$data$method$version <- method_version
  req$body$data$method$params <- method_params

  entrata_req_log(req)

  resp <- req |> httr2::req_perform()
  entrata_resp_log(resp)

  resp |>
    httr2::resp_body_json()
}


