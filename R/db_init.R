
#  ------------------------------------------------------------------------
#
# Title : Database Initialization
#    By : Jimmy Briggs
#  Date : 2024-12-12
#
#  ------------------------------------------------------------------------



# create ------------------------------------------------------------------

db_create_tbl <- function(conn, tbl, schema, df, ...) {

  check_db_conn(conn)

  schema <- rlang::arg_match0(
    schema,
    c("public", "entrata", "app", "gmh", "mkt", "logs", "survey", "meta", "auth", "util", "ext")
  )

  if (inherits(conn, "Pool")) {
    pool <- conn
    conn <- pool::poolCheckout(conn)
    on.exit(pool::poolReturn(conn), add = TRUE)
  }

  if (db_tbl_exists(conn, tbl, schema)) {
    cli::cli_alert_warning("Table {.field {schema_tbl}} already exists.")
    return(invisible())
  }

  tryCatch({
    DBI::dbWriteTable(conn, tbl, df, schema = schema, ...)
    cli::cli_alert_success("Successfully created {.field {schema_tbl}}.")
  }, error = function(e) {
    cli::cli_alert_danger(
      c(
        "Failed to create table: {.field {schema_tbl}}.\n",
        "Error: {.error {conditionMessage(e)}}"
      )
    )
  })



}

#' Create Database Table
#'
#' @description
#' Creates a database table given a connection, table name, and both of:
#'
#'  - SQL file specifying the table's schema
#'  - CSV file to seed the table's values
#'
#' @param conn database connection
#' @param tbl_name character string representing table name
#' @param csv_path base path (excluding file) to the CSV file
#' @param sql_path base path (excluding file) to the SQL file
#' @param drop_if_exists Should the table be dropped (with CASCADE) if it already exists?
#'
#' @note
#' It is assumed that the `tbl_name` mirrors both the basename of the CSV file
#' and the SQL file (excluding extensions).
#'
#' @returns the created database table returned as an R [data.frame()].
#'
#' @export
#'
#' @importFrom dbx dbxExecute dbxInsert
#' @importFrom dplyr collect tbl
#' @importFrom readr read_file read_csv
#' @importFrom tibble as_tibble
create_tbl <- function(
    conn,
    tbl_name,
    csv_path = "data-raw/database/CSV",
    sql_path = "data-raw/database/SQL",
    drop_if_exists = TRUE
) {
  sql_path <- file.path(sql_path, paste0(tbl_name, ".sql"))
  csv_path <- file.path(csv_path, paste0(tbl_name, ".csv"))
  sql <- readr::read_file(sql_path)
  csv <- readr::read_csv(csv_path) |> tibble::as_tibble()

  drop_query <- sprintf("DROP TABLE IF EXISTS public.%s CASCADE", tbl_name)
  if (drop_if_exists) dbx::dbxExecute(conn, drop_query)

  dbx::dbxExecute(conn, sql)
  dbx::dbxInsert(conn, tbl_name, csv)

  out <- dplyr::tbl(conn, tbl_name) |> dplyr::collect()
  return(out)
}

# drop --------------------------------------------------------------------

db_drop_tbl <- function(conn, tbl, schema, cascade = FALSE) {

  check_db_conn(conn)

  schema <- rlang::arg_match0(
    schema,
    c("public", "entrata", "app", "gmh", "mkt", "logs", "survey", "meta", "auth", "util", "ext")
  )

  if (inherits(conn, "Pool")) {
    pool <- conn
    conn <- pool::poolCheckout(conn)
    on.exit(pool::poolReturn(conn), add = TRUE)
  }

  qry <- if (!cascade) {
    glue::glue_sql("DROP TABLE IF EXISTS {`schema`}.{`tbl`}", .con = conn)
  } else {
    glue::glue_sql("DROP TABLE IF EXISTS {`schema`}.{`tbl`} CASCADE", .con = conn)
  }

  tryCatch({
    DBI::dbExecute(conn, qry)
    cli::cli_alert_success("Successfully dropped {.field {schema_tbl}}.")
  }, error = function(e) {
    cli::cli_alert_danger(
      c(
        "Failed to drop table: {.field {schema_tbl}}.\n",
        "Error: {.error {conditionMessage(e)}}"
      )
    )
  })

}
