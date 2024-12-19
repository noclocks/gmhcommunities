#' Get Database Configuration
#'
#' @description
#' This function retrieves the database configuration from the specified configuration
#' file and returns the configuration as a list. You can optionally specify a key
#' to retrieve a specific configuration value.
#'
#' @param key A character string representing the configuration key to retrieve.
#' @param file A character string representing the path to the configuration file.
#' @param config A character string representing the configuration to use from the
#'   configuration file.
#'
#' @returns A list, or vector (if `key` is specified), corresponding to the
#'   contents of the `db` configuration key's values (i.e. `dbname`, `host`,
#'   `port`, `username`, and `password`).
#'
#' @export
#' @importFrom cli cli_abort
#' @importFrom config get
#' @importFrom rlang arg_match
get_db_config <- function(
    key = NULL,
    file = Sys.getenv("R_CONFIG_FILE", "config.yml"),
    config = Sys.getenv("R_CONFIG_ACTIVE", "default")
) {

  # normalize path to config file
  file <- normalizePath(file, mustWork = FALSE)

  # ensure config file exists
  if (!file.exists(file)) {
    cli::cli_abort(
      c(
        "Provided configuration file: {.field {basename(file)}} not found ",
        "under path: {.path {dirname(file)}}. Please ensure the file exists."
      )
    )
  }

  # attempt to read the configuration file
  cfg <- tryCatch({
    config::get(
      value = "db",
      file = file,
      config = config
    )
  }, error = function(e) {
    cli::cli_abort(
      c(
        "Error reading configuration file {.field {basename(file)}}.",
        "Ensure the file contains a {.code db} configuration block for ",
        " the {.field {config}} configuration.",
        "Error: {.error_message {e}}"
      )
    )
  })

  keys <- names(cfg)

  if (!is.null(key)) {
    key <- rlang::arg_match(key, keys)
    return(cfg[[key]])
  }

  return(cfg)

}

#' @rdname db_config
#'
#' @param cfg
#'
#' @returns
#' @export
#'
#' @examples
#' @importFrom cli cli_abort
validate_db_config <- function(cfg) {

  if (!is.list(cfg)) {
    cli::cli_abort("Invalid {.arg cfg}: Must be a list.")
  }

  required_keys <- c("dbname", "host", "port", "user", "password")
  missing_keys <- required_keys[!required_keys %in% names(cfg)]

  if (length(missing_keys) > 0) {
    cli::cli_abort(
      c(
        "Missing required configuration keys: ",
        paste0(missing_keys, collapse = ", ")
      )
    )
  }

  return(invisible(NULL))


}
