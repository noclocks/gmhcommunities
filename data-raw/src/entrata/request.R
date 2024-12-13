
#  ------------------------------------------------------------------------
#
# Title : Entrata Request
#    By : Jimmy Briggs
#  Date : 2024-11-18
#
#  ------------------------------------------------------------------------

# request -----------------------------------------------------------------

entrata_req_body <- list(
  auth = list(
    type = "basic"
  ),
  requestId = integer(),
  method = list(
    name = character(),
    version = character(),
    params = list(NULL)
  )
)
