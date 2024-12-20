
#  ------------------------------------------------------------------------
#
# Title : Manage Database Connection User Setting
#    By : Jimmy Briggs
#  Date : 2024-12-11
#
#  ------------------------------------------------------------------------

db_set_user_id <- function(conn, user_id) {

  check_db_conn(conn)

  qry <- glue::glue_sql("SELECT `public`.`set_user_id`({`user_id`})", .con = conn, user_id = user_id)

  DBI::dbExecute(
    conn,

  )

}
