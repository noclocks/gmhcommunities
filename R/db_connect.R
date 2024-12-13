
#  ------------------------------------------------------------------------
#
# Title : Database Connection
#    By : Jimmy Briggs
#  Date : 2024-11-29
#
#  ------------------------------------------------------------------------

#' Connect to GMH Database
#'
#' @description
#' This function establishes a connection pool to the GMH PostgreSQL database
#' hosted in Google Cloud SQL using the `pool` package. Additionally,
#' the function will setup automatic cleanup of the connection pool when
#' the R or shiny session ends.
#'
#' @param db_config A named list containing the required database connection
#'   parameters. The list should contain the following elements: `host`,
#'   `port`, `dbname`, `user`, and `password`. This list is typically
#'   retrieved via a `config.yml` configuration file or via the
#'   [get_db_config()] function (the default).
#'
#' @param user_id An integer representing the user id to use for the database
#'   connection. If not provided, the function will attempt to retrieve the
#'   user id from the shiny session object. If the function is not running
#'   inside a shiny app, the default user id of `0` will be used.
#'
#' @return A `pool` object representing the connection pool to the GMH
#'   PostgreSQL database.
#'
#' @export
db_connect <- function(db_config = get_db_config(), user_id = NULL) {

  validate_db_config(db_config)

  if (is.null(user_id)) {
    # check if inside a shiny app
    if (!is.null(shiny::getDefaultReactiveDomain())) {
      session <- shiny::getDefaultReactiveDomain()
      # try and retrieve user id from session$userData$user_id
      user_id <- purrr::pluck(session, "userData", "user_id")
      if (is.null(user_id)) {
        cli::cli_alert_danger("No user id found in session.")
      } else {
        user_id <- as.integer(user_id)
      }
    } else {
      cli::cli_alert_warning("Not inside a shiny app. Using default user id.")
      user_id <- 0
    }
  } else {
    if (!is.integer(as.integer(user_id)) || user_id < 0) {
      cli::cli_abort("Invalid {.arg user_id}: Must be a non-negative integer.")
    }
    user_id <- as.integer(user_id)
  }

  tryCatch({
    conn <- pool::dbPool(
      drv = RPostgres::Postgres(),
      dbname = db_config$dbname,
      host = db_config$host,
      port = db_config$port,
      user = db_config$user,
      password = db_config$password,
      maxSize = db_config$max_size %||% 10,  # Default to 10 connections
      minSize = db_config$min_size %||% 1,   # Default to 1 connection
      idleTimeout = db_config$idle_timeout %||% 60000, # Default to 60 seconds
      onCreate = function(conn) {
        pool::dbExecute(conn, glue::glue("SET SESSION {DBI::SQL('gmhdatahub.user_id')} = '{user_id}';"))
      }
    )

    shiny_session <- shiny::getDefaultReactiveDomain()

    shiny::onStop(
      fun = function() {
        pool::poolClose(conn)
      },
      session = shiny_session
    )

    if (!is.null(shiny_session)) {
      shiny_session$userData$db_conn <- conn
      shiny_session$userData$db_user_id <- user_id
    }


    cli::cli_alert_info("Connected to Database: {db_config$dbname} on {db_config$host}.")

    return(conn)

  }, error = function(e) {
    cli::cli_abort(
      c(
        "Failed to connect to the database.",
        "Details: {conditionMessage(e)}",
        "i" = "Check if the database server is reachable and credentials are correct.",
        "i" = "Ensure that the database is configured to accept connections from this host."
      )
    )

  })

}


# check -------------------------------------------------------------------

check_db_conn <- function(
    conn,
    arg = rlang::caller_arg(conn),
    call = rlang::caller_env()
) {

  is_dbi <- inherits(conn, "PqConnection")
  is_pool <- inherits(conn, "Pool")
  is_rstudio_conn <- inherits(conn, "connConnection")

  if (is_rstudio_conn) { return(invisible(conn)) }

  if (!is_dbi && !is_pool) {
    cli::cli_abort(
      c(
        "Invalid Database Connection Provided: {.arg {arg}}",
        "The provided connection is not a valid database connection."
      ),
      call = call
    )
  }

  if (is_pool) {
    valid <- pool::dbIsValid(conn)
    if (!valid) {
      cli::cli_abort(
        c(
          "Invalid Database Connection Provided: {.arg {arg}}",
          "The provided connection pool is not valid."
        ),
        call = call
      )
    }
  }

  if (is_dbi) {
    valid <- DBI::dbIsValid(conn)
    if (!valid) {
      cli::cli_abort(
        c(
          "Invalid Database Connection Provided: {.arg {arg}}",
          "The provided DBI connection is not valid."
        ),
        call = call
      )
    }
  }

  return(invisible(conn))

}
