source("dev/scripts/db_connections.R")

db_tenants <- dplyr::tbl(src = main_conn, I("auth.tenants")) |> dplyr::collect()

auth_users <- dplyr::tbl(src = auth_conn, I("noclocks.users")) |> dplyr::collect()

db_users <- dplyr::tbl(src = main_conn, I("auth.users")) |> dplyr::collect()

db_users_new <-  dplyr::bind_rows(
  db_users,
  auth_users |>
    dplyr::select(
      email,
      hashed_password,
      created_at
    ) |>
    dplyr::filter(
      !(is.na(hashed_password)),
      !(email %in% db_users$email)
    )
) |>
  dplyr::arrange(
    user_id,
    email
  ) |>
  dplyr::select(-user_id, -created_at)

DBI::dbWriteTable(
  conn = main_conn,
  name = DBI::SQL("auth.users"),
  value = db_users_migrate,
  append = TRUE
)

auth_accounts <- dplyr::tbl(src = auth_conn, I("noclocks.accounts")) |> dplyr::collect()


auth_apps <- dplyr::tbl(src = auth_conn, I("noclocks.apps")) |> dplyr::collect()

db_apps <- dplyr::tbl(src = main_conn, I("auth.apps")) |> dplyr::collect()

db_apps_new <- dplyr::bind_rows(
  db_apps,
  auth_apps |>
    dplyr::select(
      app_name,
      app_description,
      app_url,
      app_icon,
      created_at
    ) |>
    dplyr::filter(
      !(app_name %in% db_apps$app_name)
    )
) |>
  dplyr::arrange(
    app_id,
    app_name
  ) |>
  dplyr::select(-app_id, -created_at)

dplyr::glimpse(db_apps)

auth_app_users <- dplyr::tbl(src = auth_conn, I("noclocks.app_users")) |> dplyr::collect()
auth_hosted_apps <- dplyr::tbl(src = auth_conn, I("noclocks.hosted_apps")) |> dplyr::collect()
auth_sessions <- dplyr::tbl(src = auth_conn, I("noclocks.sessions")) |> dplyr::collect()
auth_roles <- dplyr::tbl(src = auth_conn, I("noclocks.roles")) |> dplyr::collect()
auth_user_roles <- dplyr::tbl(src = auth_conn, I("noclocks.user_roles")) |> dplyr::collect()

fs::dir_create("data-raw/data/working/auth")
fs::dir_create("data-raw/data/working/auth/legacy")

readr::write_csv(auth_users, "data-raw/data/working/auth/legacy/noclocks.users.csv")
readr::write_csv(auth_accounts, "data-raw/data/working/auth/legacy/noclocks.accounts.csv")
readr::write_csv(auth_apps, "data-raw/data/working/auth/legacy/noclocks.apps.csv")
readr::write_csv(auth_app_users, "data-raw/data/working/auth/legacy/noclocks.app_users.csv")
readr::write_csv(auth_hosted_apps, "data-raw/data/working/auth/legacy/noclocks.hosted_apps.csv")
readr::write_csv(auth_sessions, "data-raw/data/working/auth/legacy/noclocks.sessions.csv")
readr::write_csv(auth_roles, "data-raw/data/working/auth/legacy/noclocks.roles.csv")
readr::write_csv(auth_user_roles, "data-raw/data/working/auth/legacy/noclocks.user_roles.csv")



