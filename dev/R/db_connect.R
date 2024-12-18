#  ------------------------------------------------------------------------
#  Database Connection Function
#  ------------------------------------------------------------------------

#' Connect to GMH Database
#'
#' @description Establishes a connection pool to the GMH PostgreSQL database,
#' hosted in Google Cloud SQL, using the `pool` package. The function
#' automatically cleans up the pool when the session ends.
#'
#' @param db_config Named list of database connection parameters (`host`, `port`,
#' `dbname`, `user`, `password`).
#' @param user_id Optional integer user ID. Defaults to Shiny session user ID or 0.
#'
#' @returns A `pool` object for database connections.
#'
#' @export
db_connect <- function(db_config = get_db_config(), user_id = NULL) {

  # Validate the database configuration
  validate_db_config(db_config)

  # Determine user ID
  if (is.null(user_id)) {
    if (!is.null(shiny::getDefaultReactiveDomain())) {
      session <- shiny::getDefaultReactiveDomain()
      user_id <- purrr::pluck(session, "userData", "user_id", .default = 0)
    } else {
      user_id <- 0
      cli::cli_alert_warning("Not inside a Shiny app. Using default user ID.")
    }
  }

  if (!is.integer(as.ineger(user_id)) || user_id < 0) {
    cli::cli_abort("Invalid {.arg user_id}: must be a non-negative integer.")
  }
  user_id <- as.integer(user_id)

  # Attempt to connect to the database
  tryCatch({
    conn <- pool::dbPool(
      drv = RPostgres::Postgres(),
      dbname = db_config$dbname,
      host = db_config$host,
      port = db_config$port,
      user = db_config$user,
      password = db_config$password,
      maxSize = db_config$max_size %||% 10,
      minSize = db_config$min_size %||% 1,
      idleTimeout = db_config$idle_timeout %||% 60000,
      onCreate = function(conn) {
        pool::dbExecute(conn, glue::glue("SET user_id = {user_id};"))
      }
    )

    shiny::onStop(
      fun = function() {
        pool::poolClose(conn)
      },
      session = shiny::getDefaultReactiveDomain()
    )

    cli::cli_alert_info("Connected to Database: {db_config$dbname} on {db_config$host}.")
    cli::cli_alert_info("Active connections in pool: {pool::poolSize(conn)}.")

    return(conn)

  }, error = function(e) {
    cli::cli_abort(
      c(
        "Failed to connect to the database.",
        "Details: {conditionMessage(e)}",
        "i" = "Check if the database server is reachable.",
        "i" = "Verify your connection parameters (host, port, user, password)."
      )
    )
  })
}
