
#  ------------------------------------------------------------------------
#
# Title : Entrata /queue Endpoint
#    By : Jimmy Briggs
#  Date : 2024-11-24
#
#  ------------------------------------------------------------------------


# topic -------------------------------------------------------------------

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
#' @return A list containing the response body and status code.
entrata_queue <- function(
    queue_id,
    entrata_config = get_entrata_config()
) {

  req <- entrata_request(
    entrata_config = entrata_config
  ) |>
    entrata_req_endpoint("queue") |>
    entrata_req_body(
      id = 15L,
      method = "getResponse",
      version = "r1",
      params = list(
        queueId = queue_id,
        serviceName = "getReportData"
      )
    )

  resp <- httr2::req_perform(req)

  hold <- entrata_resp_body(resp) |>
    purrr::pluck("response", "result", "reportData")

}
