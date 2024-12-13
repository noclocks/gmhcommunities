
#  ------------------------------------------------------------------------
#
# Title : Manage Database Connection User Setting
#    By : Jimmy Briggs
#  Date : 2024-12-11
#
#  ------------------------------------------------------------------------

db_set_user_id <- function(conn, user_id) {

  check_db_conn(conn)

  DBI::dbExecute(
    conn,
    "SET SESSION gmhdatahub.user_id = ?",
    user_id
  )

}
