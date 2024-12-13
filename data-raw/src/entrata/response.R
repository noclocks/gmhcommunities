
#  ------------------------------------------------------------------------
#
# Title : Entrata Responses
#    By : Jimmy Briggs
#  Date : 2024-11-18
#
#  ------------------------------------------------------------------------

# response (successful) ---------------------------------------------------

entrata_resp_body_success <- list(
  response = list(
    requestId = integer(),
    code = 200,
    result = list()
  )
)


# response (error) --------------------------------------------------------

entrata_resp_body_error <- list(
  response = list(
    requestId = integer(),
    code = 200,
    error = list(
      code = integer(),
      message = character()
    )
  )
)
