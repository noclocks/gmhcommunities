

pool <- db_connect()
conn <- pool::dbCheckout(pool)

connections::connection_view(conn)

on.exit({
  pool::poolReturn(conn)
  pool::poolClose(pool)
},
  add = TRUE
)


# properties --------------------------------------------------------------

entrata_properties_from_db <- dplyr::tbl(conn, I("entrata.properties")) |>
  dplyr::collect()

gmh_properties_from_db <- dplyr::tbl(conn, I("gmh.properties")) |>
  dplyr::collect()

gmh_properties_updated <- gmh_properties_from_db |>
  dplyr::mutate(
    entrata_property_id = stringr::str_remove_all(
      entrata_property_id, "\""
    ) |>
      as.integer(),
    property_id = entrata_property_id
  ) |>
  dplyr::select(
    property_id,
    is_active
  ) |>
  dplyr::full_join(
    entrata_properties_from_db,
    by = "property_id"
  ) |>
  dplyr::select(
    property_id,
    property_name = marketing_name,
    property_type,
    website,
    address,
    email,
    is_disabled,
    is_featured_property,
    is_active,
    year_built,
    short_description,
    long_description,
    parent_property_id
  ) |>
  dplyr::mutate(
    is_active = TRUE
  )

DBI::dbExecute(conn, "DROP TABLE IF EXISTS \"gmh\".\"properties\" CASCADE")

DBI::dbExecute(
  conn,
  "CREATE TABLE \"gmh\".\"properties\" (
    property_id INTEGER PRIMARY KEY,
    property_name TEXT,
    property_type TEXT DEFAULT 'Student',
    website TEXT,
    address TEXT,
    email TEXT,
    is_disabled BOOLEAN DEFAULT FALSE,
    is_featured_property BOOLEAN DEFAULT FALSE,
    is_active BOOLEAN DEFAULT TRUE,
    year_built INTEGER,
    short_description TEXT,
    long_description TEXT,
    parent_property_id INTEGER REFERENCES \"gmh\".\"properties\" (property_id) DEFAULT NULL,
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
  )"
)

DBI::dbAppendTable(
  conn,
  name = DBI::SQL("gmh.properties"),
  value = gmh_properties_updated,
  append = TRUE
)

gmh_properties_from_db_updated <- dplyr::tbl(conn, I("gmh.properties")) |>
  dplyr::collect()


# portfolios --------------------------------------------------------------

gmh_portfolios_from_db <- dplyr::tbl(conn, I("gmh.portfolios")) |>
  dplyr::collect()

gmh_portfolio_types_from_db <- dplyr::tbl(conn, I("gmh.portfolio_types")) |>
  dplyr::collect()

gmh_investment_partners_from_db <- dplyr::tbl(conn, I("gmh.investment_partners")) |>
  dplyr::collect()

public_investment_partners_from_db <- dplyr::tbl(conn, I("public.investment_partners")) |>
  dplyr::collect()

gmh_portfolios_updated <- gmh_portfolios_from_db |>
