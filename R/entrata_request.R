
#  ------------------------------------------------------------------------
#
# Title : Entrata Request
#    By : Jimmy Briggs
#  Date : 2024-11-24
#
#  ------------------------------------------------------------------------

# request -----------------------------------------------------------------

#' Initialize an Entrata API Request
#'
#' @description
#' Sets up a base request object with core Entrata API configurations.
#'
#' @param entrata_config A list containing Entrata configuration
#'   (Base URL, Username and Password, etc.). By default will call
#'   [get_entrata_config()] to retrieve the configuration.
#'
#' @return An [httr2::request()] object with Entrata-specific configurations.
#'
#' @export
#'
#' @importFrom httr2 request req_method req_headers req_user_agent req_auth_basic req_error
entrata_request <- function(entrata_config = get_entrata_config()) {

  validate_entrata_config(entrata_config)

  base_url <- entrata_config$base_url
  username <- entrata_config$username
  password <- entrata_config$password
  user_agent <- entrata_config$user_agent %||% "gmhcommunities/0.0.1"

  httr2::request(base_url) |>
    httr2::req_method("POST") |>
    httr2::req_auth_basic(username, password) |>
    httr2::req_headers(
      `Content-Type` = "application/json; charset=utf-8",
      `Accept` = "application/json",
      .redact = "Authorization"
    ) |>
    httr2::req_user_agent(user_agent) |>
    httr2::req_error(
      is_error = entrata_resp_is_error,
      body = entrata_resp_error_body
    )

}

# request modifiers -------------------------------------------------------

#' Entrata Request Modifiers
#'
#' @name entrata_request_modifiers
#'
#' @description
#' These functions are used to modify the Entrata request object before
#' sending the request to the API. These functions are used to set the
#' endpoint path, Entrata internal endpoint method (name, version, and params),
#' additional headers, authentication, request body, error handling, logging,
#' and more.
#'
#' - [entrata_req_endpoint()] - Set the endpoint path for the request
#' - [entrata_req_body()] - Set the request body options
#' - [entrata_req_auth()] - Add basic authentication to the request
#' - [entrata_req_user_agent()] - Set the user agent for the request
#' - [entrata_req_retry()] - Set the retry policy for the request
#' - [entrata_req_log()] - Set the logging policy for the request
#' - [entrata_req_cache()] - Set the caching policy for the request
#' - [entrata_req_hash()] - Generate a hash key for the request
#'
#' @seealso
#' [entrata_request()]
#' [entrata_req_endpoint()], [entrata_req_body()], [entrata_req_auth()],
#' [entrata_req_user_agent()], [entrata_req_headers()], [entrata_req_config()],
#' [entrata_req_log()]
NULL


# authentication ----------------------------------------------------------

#' Entrata Request Authentication
#'
#' @description
#' Adds basic authentication to the Entrata request object using provided credentials.
#'
#' @param req An [httr2::request()] object
#' @param username A character string representing the username for the Entrata API
#' @param password A character string representing the password for the Entrata API
#'
#' @return The modified request object with authentication headers.
#'
#' @export
#'
#' @importFrom httr2 req_auth_basic
entrata_req_auth <- function(
  req,
  username = NULL,
  password = NULL
) {

  check_request(req)

  username <- username %||% getOption("entrata.username") %||%
    Sys.getenv("ENTRATA_USERNAME") %||% get_entrata_config("username")

  password <- password %||% getOption("entrata.password") %||%
    Sys.getenv("ENTRATA_PASSWORD") %||% get_entrata_config("password")

  if (is.null(username) || is.null(password)) {
    cli::cli_abort(
      "Entrata API username and password must be provided."
    )
  }

  httr2::req_auth_basic(req, username, password)

}


# endpoint ----------------------------------------------------------------

#' Set Endpoint Path for Entrata Request
#'
#' @description
#' Appends an endpoint path to the request URL.
#'
#' @param req An httr2 request object
#' @param endpoint The API endpoint to set. Must be one of the valid Entrata
#'   endpoints. See [entrata_endpoints()] for a list of valid endpoints.
#'
#' @section Warning:
#'
#' This function will overwrite any existing endpoint path set on the request.
#'
#' @return Modified request object with the endpoint URL path appended
#'
#' @export
#'
#' @importFrom httr2 req_url_path_append req_url_path
#' @importFrom cli cli_alert_warning
entrata_req_endpoint <- function(req, endpoint) {

  # validate args
  check_request(req)
  validate_entrata_endpoint(endpoint)

  pre_endpoint <- get_request_endpoint(req)

  if (is.null(pre_endpoint) || pre_endpoint == "") {
    req <- httr2::req_url_path_append(req, endpoint)
    return(req)
  } else {
    cli::cli_alert_warning(
      "Overwriting existing endpoint path: {.field {pre_endpoint}} with {.field {endpoint}}."
    )
    return(httr2::req_url_path(req, endpoint))
  }

}

get_request_endpoint <- function(req) {
  check_request(req)
  req_url <- purrr::pluck(req, "url")
  gsub(paste0("^", "https://gmhcommunities.entrata.com/api/v1"), "", req_url)
}


# body --------------------------------------------------------------------

#' Entrata Request Body
#'
#' @description
#' Modifies the Entrata request object with the provided request body options.
#'
#' Note that the endpoint should be set before attempting to use this function
#' to get the best results.
#'
#' @param req An httr2 request object
#' @param id Request ID
#' @param method Request Endpoint Method (not HTTP method)
#' @param version Request Endpoint Method Version
#' @param params Request Endpoint Method Parameters
#'
#' @return The modified request object with the new request body options
#'
#' @export
#'
#' @importFrom purrr pluck compact
#' @importFrom cli cli_alert_warning
#' @importFrom httr2 req_body_json
entrata_req_body <- function(
    req,
    id = NULL,
    method = NULL,
    version = NULL,
    params = list()
) {

  check_request(req)

  # extract endpoint
  req_endpoint <- get_request_endpoint(req)

  # method, version, and params
  if (req_endpoint != "") {
    req_method <- method %||% get_default_method(endpoint = req_endpoint)
    validate_entrata_method(endpoint = req_endpoint, method = req_method)

    req_version <- version %||% get_default_version(method = req_method, endpoint = req_endpoint)
    validate_entrata_method_version(endpoint = req_endpoint, method = req_method, version = req_version)

    req_params <- params %||% get_default_params(method = req_method, endpoint = req_endpoint)
    validate_entrata_method_params(endpoint = req_endpoint, method = req_method, params = req_params)
  } else {
    cli::cli_alert_warning(
      "No endpoint set on request. Using provided method, version, and params without validation."
    )

    req_method <- method
    req_version <- version
    req_params <- params
  }

  # request ID
  req_id <- id %||% getOption("entrata.default_request_id") %||%
    Sys.getenv("ENTRATA_REQUEST_ID") %||%
    get_entrata_config("default_request_id") %||%
    as.integer(Sys.time())

  # get current request body
  pre_req_body <- purrr::pluck(req, "body", "data")

  if (!is.null(pre_req_body) > 0) {
    cli::cli_alert_warning(
      "Overwriting existing request body with new request body."
    )
  }

  # build new request body
  req_body <- list(
    auth = list(
      type = "basic"
    ),
    requestId = req_id,
    method = list(
      name = req_method,
      version = req_version,
      params = req_params
    )
  ) |>
    purrr::compact()

  # modify body and return
  req |>
    httr2::req_body_json(req_body)
}

# hashing -----------------------------------------------------------------

#' Entrata Request Hash
#'
#' @description
#' Generates a hash key for the request object based on the endpoint and request body.
#'
#' @param req An [httr2::request()] object.
#'
#' @return A character string representing the hash key for the request.
#'
#' @export
#'
#' @importFrom digest digest
entrata_req_hash <- function(req) {
  check_request(req)
  endpoint <- get_request_endpoint(req)
  body <- req$body$data
  cache_key <- paste0(endpoint, digest::digest(body, algo = "md5"), sep = "_")
  return(cache_key)
}


# user agent --------------------------------------------------------------

#' Entrata Request User Agent
#'
#' @description
#' Modifies the Entrata request object with the provided user agent string.
#'
#' @param req An httr2 request object
#' @param string A character string representing the user agent to set on the request
#'   object. If not provided, will use the default user agent set in the Entrata
#'   configuration or the package version and URL.
#'
#' @return The modified request object with the new user agent string
#'
#' @export
#'
#' @importFrom httr2 req_user_agent
#' @importFrom utils packageVersion packageDescription
entrata_req_user_agent <- function(req, string) {

  check_request(req)

  ua <- string %||% getOption("entrata.user_agent") %||%
    Sys.getenv("ENTRATA_USER_AGENT") %||%
    get_entrata_config("user_agent") %||%
    paste0(
      "gmhcommunities/",
      utils::packageVersion("gmhcommunities"), " (",
      utils::packageDescription("gmhcommunities")$URL[[1]], ")"
    )

  req |>
    httr2::req_user_agent(ua)

}


# request headers ---------------------------------------------------------

#' Entrata Request Headers
#'
#' @description
#' Modifies the Entrata request object with the provided headers.
#'
#' @param req An [httr2::request()] object.
#' @param headers A named list of headers to set on the request object.
#'
#' @return The modified request object with the new headers set.
#'
#' @export
entrata_req_headers <- function(req, headers) {
  check_request(req)
  req |>
    httr2::req_headers(headers)
}

#' Entrata Request Pagination
#'
#' @description
#' Many of the Entrata API endpoints support pagination. This function
#' adds pagination related headers and query parameters to the request object.
#'
#' @param req An [httr2::request()] object.
#' @param page_number Pagination page number. Added to the request's URL query
#'   parameters. Default is `1` (i.e. `/?page_no=1`).
#' @param per_page Number of items per page. Added to the request's URL query
#'   parameters. Default is `500` (i.e. `/?page_no=1&per_page=500`).
#' @param include_pagination_links Logical value to include pagination links in
#'   the response. Default is `FALSE`. Alters the request's headers by injecting
#'   the `X-Send-Pagination-Links` header.
#'
#' @return The modified request object with pagination headers and query parameters.
#'
#' @export
#'
#' @importFrom httr2 req_headers req_url_query
entrata_req_pagination <- function(
  req,
  page_number = 1,
  per_page = 500,
  include_pagination_links = FALSE
) {

  check_request(req)

  if (include_pagination_links) {
    req <- req |>
      httr2::req_headers(
        `X-Send-Pagination-Links` = 1
      )
  }

  if (!is.null(page_number)) {
    req <- req |>
      httr2::req_url_query(
        "page_no" = page_number
      )
  }

  if (!is.null(per_page)) {
    req <- req |>
      httr2::req_url_query(
        "per_page" = per_page
      )
  }

}

# retry_policy ------------------------------------------------------------

.exponential_backoff <- function(num_requests) {
  backoff <- 2^num_requests
  jitter <- runif(1, 0, 1)
  backoff <- backoff + jitter
  return(min(backoff, 60))
}

#' Entrata Request Retry Policy
#'
#' @description
#' Set the retry policy for Entrata API requests.
#'
#' @param req An [httr2::request()] object.
#' @inheritParams httr2::req_retry
#'
#' @return Modified [httr2::request()] with retry policy configured.
#'
#' @export
#'
#' @importFrom httr2 req_retry
#'
#' @seealso [httr2::req_retry()], [entrata_resp_is_transient()], [entrata_resp_retry_after()]
entrata_req_retry <- function(
  req,
  max_tries = 5,
  max_seconds = 30,
  retry_on_failure = TRUE,
  is_transient = entrata_resp_is_transient,
  backoff = .exponential_backoff,
  after = entrata_resp_retry_after
) {

  check_request(req)

  httr2::req_retry(
    req,
    max_tries = max_tries,
    max_seconds = max_seconds,
    retry_on_failure = retry_on_failure,
    is_transient = is_transient,
    backoff = backoff,
    after = after
  )

}


# logging -----------------------------------------------------------------

entrata_req_logger <- function(req, ...) {}


# caching -----------------------------------------------------------------

# entrata_req_cache <- function(req, ...) {}



