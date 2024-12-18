
#  ------------------------------------------------------------------------
#
# Title : Entrata /status
#    By : Jimmy Briggs
#  Date : 2024-12-13
#
#  ------------------------------------------------------------------------


entrata_status <- function(request_id = NULL, entrata_config = get_entrata_config()) {

  if (is.null(request_id)) {
    request_id <- as.integer(Sys.time())
  }

  req <- entrata_request(entrata_config) |>
    entrata_req_endpoint("status") |>
    entrata_req_body(
      id = request_id,
      method = "getStatus",
      version = "r1"
    )

  resp <- httr2::req_perform(req)

  entrata_resp_body(resp)

}
