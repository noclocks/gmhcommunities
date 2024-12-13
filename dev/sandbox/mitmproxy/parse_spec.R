require(tibblify)

apispec_file <- "dev/sandbox/mitmproxy/entrata.apispec.yml"

api_spec <- tibblify::parse_openapi_spec(apispec_file)
dplyr::glimpse(api_spec)

endpoint_specs <- api_spec$spec |>
  purrr::map(purrr::pluck, "paths")
