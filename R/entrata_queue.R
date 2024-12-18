
#  ------------------------------------------------------------------------
#
# Title : Entrata /queue Endpoint
#    By : Jimmy Briggs
#  Date : 2024-11-24
#
#  ------------------------------------------------------------------------

#' Entrata `/queue` Endpoint
#'
#' @name entrata_queue
#'
#' @description
#' The `/queue` endpoint provides a mechanism for retrieving previously
#' created queue items. Queue items are created when a user requests a
#' report that takes a long time to generate. The user is then able to
#' check the status of the report by querying the queue endpoint with the
#' `queueId`.
#'
#' @param queue_id The unique identifier for the queue item.
#' @param entrata_config A list containing the necessary configuration
#'
#' @returns A list containing the response body and status code.
entrata_queue <- function(
    queue_id,
    request_id = NULL,
    entrata_config = get_entrata_config(),
    verbose = FALSE,
    progress = FALSE
) {

  request_id <- request_id %||% as.integer(Sys.time())

  req <- entrata_request(
    entrata_config = entrata_config
  ) |>
    entrata_req_endpoint("queue") |>
    entrata_req_body(
      id = request_id,
      method = "getResponse",
      version = "r1",
      params = list(
        queueId = queue_id,
        serviceName = "getReportData"
      )
    ) |>
    entrata_req_retry(
      max_tries = 10
    )

  if (verbose) {
    req <- httr2::req_verbose(req)
  }

  if (progress) {
    req <- httr2::req_progress(req)
  }

  resp <- httr2::req_perform(req)

  resp_data <- entrata_resp_body(resp) |>
    purrr::pluck("response", "result", "reportData")

  list(
    resp = httr2::last_response(),
    data = resp_data
  )

}
