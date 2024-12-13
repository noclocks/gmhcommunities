
#  ------------------------------------------------------------------------
#
# Title : Entrata Validation Functions
#    By : Jimmy Briggs
#  Date : 2024-11-24
#
#  ------------------------------------------------------------------------



validate_entrata_params <- function(
    endpoint,
    method,
    params
) {

  method_params <- entrata_params[[endpoint]][[method]]

  validated_params <- list()

  for (param in names(method_params)) {
    param_def <- method_params[[param]]
    param_value <- params[[param]]
    if (is.null(param_value)) {
      if (param_def$required) {
        cli::cli_abort("{.arg {param}} is required but was not provided.")
      }
      if (!is.null(param_def$default)) {
        param_value <- param_def$default
      }
    }

    if (!is.null(param_def$validator)) {
      param_def$validator(param_value, nullable = !param_def$required, arg = param)
    }

    validated_params[[param]] <- param_value

  }

  return(validated_params)

}

# id -------------------------------------------------------------

#' Validate `Id` Parameters
#'
#' @param value The input value to validate.
#' @param nullable Whether the value can be `NULL`. Default is `FALSE`.
#' @inheritParams rlang::args_error_context
#'
#' @return `TRUE` (invisibly) if validation passes; otherwise, throws an error.
#'
#' @export
#'
#' @importFrom cli cli_abort
#' @importFrom rlang caller_arg caller_env
validate_entrata_id <- function(
    value,
    nullable = FALSE,
    arg = rlang::caller_arg(value),
    call = rlang::caller_env()
) {

  if (nullable && is.null(value)) { return(invisible(TRUE)) }

  if (is.numeric(value) && value == as.integer(value)) {
    return(invisible(TRUE))
  }

  if (!is.character(value) || length(value) != 1 || !grepl("^[0-9]+$", value)) {
    cli::cli_abort("{.arg {arg}} must be a single, non-empty string coercible to integer.", call = call)
  }

  return(invisible(TRUE))

}

#' Coerce `Id` to a String
#'
#' @param value The input value to coerce.
#' @param nullable Whether the value can be `NULL`. Default is `FALSE`.
#'
#' @return The coerced `Id` as a string.
#'
#' @export
#'
#' @importFrom cli cli_abort
coerce_entrata_id <- function(value, nullable = FALSE) {

  if (nullable && is.null(value)) { return(NULL) }

  if (is.numeric(value) && value == as.integer(value)) {
    value <- as.character(as.integer(value))
  }

  if (!is.character(value) || length(value) != 1) {
    cli::cli_abort("{.arg propertyId} must be a single, non-empty string coercible to integer.")
  }

  return(value)

}

#' Validate and Coerce an `Id`
#'
#' @param value The input value to sanitize.
#' @param nullable Whether the value can be `NULL`. Default is `FALSE`.
#'
#' @return The sanitized `Id` as a string.
#'
#' @export
sanitize_entrata_id <- function(value, nullable = FALSE) {
  validate_id(value, nullable = nullable)
  coerce_id(value, nullable = nullable)
}


# ids ------------------------------------------------------------

#' Validate Multiple `Ids`
#'
#' @param value The input value to validate.
#' @param nullable Whether the value can be `NULL`. Default is `FALSE`.
#' @inheritParams rlang::args_error_context
#'
#' @return `TRUE` (invisibly) if validation passes; otherwise, an error is raised.
#'
#' @export
#'
#' @importFrom cli cli_abort
#' @importFrom rlang caller_arg caller_env
validate_entrata_ids <- function(
    value,
    nullable = FALSE,
    arg = rlang::caller_arg(value),
    call = rlang::caller_env()
) {

  if (nullable && is.null(value)) { return(invisible(TRUE)) }

  if (is.vector(value)) {
    value <- as.character(value)
  }
  if (is.character(value)) {
    value <- strsplit(value, ",")[[1]]
  } else {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a character string, integer vector, or a mix of ",
        "character and integer values."
      ),
      call = call
    )
  }

  if (any(is.na(as.integer(value)))) {
    cli::cli_abort("{.arg {arg}} must only contain values coercible to integer.", call = call)
  }

  return(invisible(TRUE))

}

#' Coerce `Ids` to a Comma-Separated String
#'
#' @param value The input value to coerce.
#' @param nullable Whether the value can be `NULL`. Default is `FALSE`
#'
#' @return A single comma-separated string of IDs.
#'
#' @export
coerce_entrata_ids <- function(
    value,
    nullable = FALSE
) {

  if (nullable && is.null(value)) { return(NULL) }

  if (is.vector(value)) {
    value <- as.character(value)
  } else if (is.character(value)) {
    value <- strsplit(value, ",")[[1]]
  }

  return(paste(as.integer(value), collapse = ","))

}

#' Validate and Coerce `Ids`
#'
#' @param value The input value to sanitize.
#' @param nullable Whether the value can be `NULL`. Default is `FALSE`.
#'
#' @return A single comma-separated string of IDs.
#'
#' @export
sanitize_entrata_ids <- function(value, nullable = FALSE) {
  validate_property_ids(value, nullable = nullable)
  coerce_property_ids(value, nullable = nullable)
}


# boolean -----------------------------------------------------------------

#' Validate a Boolean Value
#'
#' @param value The input value to validate.
#' @param nullable Whether the value can be `NULL`. Default is `FALSE`.
#' @inheritParams rlang::args_error_context
#'
#' @return `TRUE` (invisibly) if validation passes; otherwise, throws an error.
#'
#' @export
#'
#' @importFrom cli cli_abort
#' @importFrom rlang caller_arg caller_env
validate_entrata_boolean <- function(
    value,
    nullable = FALSE,
    arg = rlang::caller_arg(value),
    call = rlang::caller_env()
) {

  if (nullable && is.null(value)) { return(invisible(TRUE)) }

  # entrata booleans can be R logicals, R integers (only 0 or 1),
  # R character integers ("0" or "1"), or R character strings ("true" or "false"),
  # but when passed in request body must be coerced into a single "0" or "1" string:
  if (is.logical(value)) {
    value <- as.integer(value)
  }

  if (is.numeric(value)) {
    if (value == 0 || value == 1) {
      return(invisible(TRUE))
    }
  }

  if (is.character(value)) {
    if (value %in% c("0", "1", "true", "false")) {
      return(invisible(TRUE))
    }
  }

  cli::cli_abort(
    c(
      "{.arg {arg}} must be a logical, integer (0 or 1), or character string ",
      "('0', '1', 'true', or 'false')."
    ),
    call = call
  )

}

#' Coerce a Boolean Value
#'
#' @param value The input value to coerce.
#' @param nullable Whether the value can be `NULL`. Default is `FALSE`.
#'
#' @return The coerced boolean value as a string.
#'
#' @export
coerce_entrata_boolean <- function(value, nullable = FALSE) {

  if (nullable && is.null(value)) { return(NULL) }

  if (is.logical(value)) {
    value <- as.integer(value)
  }

  if (is.numeric(value)) {
    value <- as.character(value)
  }

  if (is.character(value)) {
    if (value %in% c("true", "false")) {
      value <- ifelse(value == "true", "1", "0")
    }
    if (value %in% c("0", "1")) {
      return(value)
    }
  }

  cli::cli_abort(
    c(
      "{.arg {arg}} must be a logical, integer (0 or 1), or character string ",
      "('0', '1', 'true', or 'false')."
    )
  )

}

#' Validate and Coerce a Boolean Value
#'
#' @param value The input value to sanitize.
#' @param nullable Whether the value can be `NULL`. Default is `FALSE`.
#'
#' @return The sanitized boolean value as a string.
#'
#' @export
sanitize_entrata_boolean <- function(value, nullable = FALSE) {
  validate_entrata_boolean(value, nullable = nullable)
  coerce_entrata_boolean(value, nullable = nullable)
}
