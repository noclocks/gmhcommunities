db_schema_tbl_name <- function(schema = NULL, tbl_name) {

  schemas <- c(
    "public",
    "gmh",
    "entrata",
    "auth",
    "mkt",
    "survey",
    "app",
    "util",
    "ext",
    "archive"
  )

  if (!is.null(schema)) {
    schema <- rlang::arg_match(schema, schemas)
  }

  if (is.null(schema)) {
    schema <- "public"
  }

  DBI::SQL(glue::glue("{schema}.{tbl_name}"))
}

db_tbl_exists <- function(conn, tbl_name, in_schema = NULL) {

  check_db_conn(conn)

  schema_tbl_name <- db_schema_tbl_name(in_schema, tbl_name)

  func <- switch(
    class(conn)[[1]],
    "PgConnection" = DBI::dbExistsTable,
    "Pool" = pool::dbExistsTable
  )

  return(func(conn, schema_tbl_name))

}

db_overwrite_tbl <- function(conn, tbl_name, df, in_schema = NULL) {

  check_db_conn(conn)

  schema_tbl_name <- db_schema_tbl_name(in_schema, tbl_name)

  func <- switch(
    class(conn)[[1]],
    "PgConnection" = DBI::dbWriteTable,
    "Pool" = pool::dbWriteTable
  )

  func(conn, schema_tbl_name, value = df, overwrite = TRUE)

}

db_append_tbl <- function(conn, tbl_name, df, in_schema = NULL) {

  check_db_conn(conn)

  schema_tbl_name <- db_schema_tbl_name(in_schema, tbl_name)

  func <- switch(
    class(conn)[[1]],
    "PgConnection" = DBI::dbWriteTable,
    "Pool" = pool::dbWriteTable
  )

  func(conn, schema_tbl_name, value = df, append = TRUE)

}

db_upsert_tbl <- function(conn, tbl_name, df, in_schema = NULL) {

  check_db_conn(conn)

  schema_tbl_name <- db_schema_tbl_name(in_schema, tbl_name)

  qry <- glue::glue_sql(
    .con = conn,
    "INSERT INTO {schema_tbl_name} ({names}) VALUES ({values}) ON CONFLICT ({conflict}) DO UPDATE SET {update}",
    names = colnames(df),
    values = purrr::map_chr(df, DBI::sqlInterpolate),
    conflict = colnames(df),
    update = glue::glue("{col} = EXCLUDED.{col}")
  )

  func <- switch(
    class(conn)[[1]],
    "PgConnection" = DBI::dbExecute,
    "Pool" = pool::dbExecute
  )

  func(conn, qry)

}
