
#  ------------------------------------------------------------------------
#
# Title : Entrata /arcodes Endpoint
#    By : Jimmy Briggs
#  Date : 2024-11-24
#
#  ------------------------------------------------------------------------

entrata_arcodes <- function(
  property_id = NULL,
  request_id = NULL,
  entrata_config = get_entrata_config(),
  verbose = FALSE,
  progress = FALSE
) {

  method_name <- "getArCodes"
  method_params <- list(
    propertyId = property_id
  )

  validate_entrata_method(endpoint = "arcodes", method = method_name)
  method_version <- get_default_method_version("arcodes", method_name)
  validate_entrata_method_params(endpoint = "arcodes", method = method_name, params = method_params)
  request_id <- request_id %||% as.integer(Sys.time())

  req <- entrata_request(entrata_config) |>
    entrata_req_endpoint("arcodes") |>
    entrata_req_body(
      id = request_id,
      method = method_name,
      version = method_version,
      params = method_params
    )

  if (verbose) {
    req <- httr2::req_verbose(req)
  }

  if (progress) {
    req <- httr2::req_progress(req)
  }

  resp <- httr2::req_perform(req)

  parse_arcodes_response(resp)

}
