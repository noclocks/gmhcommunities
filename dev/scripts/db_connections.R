db_config <- config::get("db")

conn <- connections::connection_open(
  RPostgres::Postgres(),
  dbname = db_config$dbname,
  host = db_config$host,
  port = db_config$port,
  user = db_config$user,
  password = db_config$password
)

auth_db_config <- config::get("auth_db")

auth_conn <- connections::connection_open(
  RPostgres::Postgres(),
  dbname = auth_db_config$dbname,
  host = auth_db_config$host,
  port = auth_db_config$port,
  user = auth_db_config$user,
  password = auth_db_config$password
)
