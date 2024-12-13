
#  ------------------------------------------------------------------------
#
# Title : Mocking Helpers
#    By : Jimmy Briggs
#  Date : 2024-12-08
#
#  ------------------------------------------------------------------------

require(jsonlite)


# mock request body -------------------------------------------------------

mock_req_body <- function(request_id = NULL, method_name = NULL, method_version = NULL, method_params = NULL) {
  list(
    auth = list(
      type = "basic"
    ),
    requestId = request_id,
    method = list(
      name = method_name,
      version = method_version,
      params = method_params
    )
  )
}


# mock successful response ------------------------------------------------

mock_resp_getStatus <- function(request_id = NULL) {

  list(
    response = list(
      requestId = request_id,
      code = 200,
      result = list(
        status = "Success",
        message = "API service is available and running."
      )
    )
  )

}

mock_resp_getProperties <- function(request_id, ...) {

  entrata_mocks_dir <- testthat::test_path("mocks/gmhcommunities.entrata.com")
  mock_json_file <- fs::path(entrata_mocks_dir, "properties", "getProperties-99-noParams.json")
  mock_json <- jsonlite::read_json(mock_json_file)

  mock_json$response$requestId <- request_id

  return(mock_json)

}


# mock error response -----------------------------------------------------

mock_resp_error_invalid_auth <- function(request_id = NULL) {

  list(
    response = list(
      requestId = request_id,
      error = list(
        code = 113,
        message = "Username and/or password is incorrect."
      )
    )
  )

}

mock_resp_error_invalid_property_id <- function(request_id = NULL) {

  list(
    response = list(
      requestId = request_id,
      error = list(
        code = 301,
        message = "User doesn't have permission to the property or property is disabled."
      )
    )
  )

}
