create_apispec <- function(flow_file, apispec_file, api_url = entrata_config$base_url) {

  mitmproxy2swagger <- reticulate::import("mitmproxy2swagger")
  mitmproxy2swagger_main <- mitmproxy2swagger$mitmproxy2swagger$main

  # mitmproxy2swagger -i <input_path> -o <output_path> -p <api_prefix>
  args <- list(
    "-i", flow_file,
    "-o", apispec_file,
    "-p", api_url
  )

  mitmproxy2swagger_main(args)

  cli::cli_alert_success(
    c(
      "API Specification File Created/Updated Successfully.\n",
      "Input Flows File: {.path {flow_file}}\n",
      "Output API Specification File: {.path {apispec_file}}\n"
    )
  )

}

mitmdump_to_swagger <- function(input_file, output_file, api_prefix) {

  mitmproxy2swagger <- reticulate::import("mitmproxy2swagger")
  mitmproxy2swagger_main <- mitmproxy2swagger$mitmproxy2swagger$main

  # mitmproxy2swagger -i <input_path> -o <output_path> -p <api_prefix>
  args <- list(
    "-i", input_file,
    "-o", output_file,
    "-p", api_prefix
  )

  mitmproxy2swagger_main(args)
}
