require(httr2)
library(reticulate)

proxy_url <- "http://localhost:8080"
entrata_config <- config::get("entrata")
flow_file <- "dev/sandbox/mitmproxy/entrata.flow"
log_file <- "dev/sandbox/mitmproxy/entrata.log"
apispec_file <- "dev/sandbox/mitmproxy/entrata.apispec.yml"
# Sys.setenv(CURL_CA_BUNDLE = "/path/to/mitmproxy-ca-cert.pem")

source("dev/sandbox/mitmproxy/R/mitmweb.R")

mitmweb_process <- run_mitmweb(stream_file = flow_file)

resp_status <- entrata_req_proxy_perform("status", "getStatus", "r1", list(NULL))

resp_properties <- entrata_req_proxy_perform("properties", "getProperties", "r1", list(propertyId = ""))

resp_properties_data <- purrr::pluck(resp_properties, "response", "result", "PhysicalProperty", "Property") |>
  parse_properties_response()

property_ids <- resp_properties_data$property_tbl_base$property_id

resp_properties_map <- purrr::map(
  property_ids,
  ~ entrata_req_proxy_perform("properties", "getProperties", "r1", list(propertyId = as.character(.x)))
)

mitmdump_to_swagger(
  input_file = out_file,
  output_file = apispec_file,
  api_prefix = entrata_config$base_url
)

stop_mitmweb(mitmweb_process)
