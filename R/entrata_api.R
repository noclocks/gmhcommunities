#'
#' #  ------------------------------------------------------------------------
#' #
#' # Title : Entrata API Client
#' #    By : Jimmy Briggs
#' #  Date : 2024-11-24
#' #
#' #  ------------------------------------------------------------------------
#'
#' #' Entrata API Manager Client
#' #'
#' #' @description
#' #' This is a wrapper [R6::R6Class()] that provides a high-level interface for
#' #' interacting with the [Entrata API](https://api.entrata.com/v1/documentation/).
#' #'
#' #' It handles authentication, request parameter validation and construction,
#' #' and response parsing, making it easy for developers to work with the API.
#' #'
#' #' @details
#' #' This class is designed to be used as an HTTP client for the Entrata API.
#' #'
#' #' @field config A list containing the Entrata API configuration, including
#' #'   the username, password, and API base URL.
#' #'
#' #' @field user_agent The user agent string to use for API requests.
#' #'
#' #' @export
#' #'
#' #' @importFrom httr2 req_auth_basic req_headers req_url_path_append request
#' #' @importFrom purrr compact
#' #' @importFrom glue glue
#' #' @importFrom rlang abort
#' #' @importFrom config get
#' EntrataAPI <- R6::R6Class(
#'   classname = "EntrataAPI",
#'   public = list(
#'     config = NULL,
#'     user_agent = NULL,
