

pool <- db_connect()
conn <- pool::dbCheckout(pool)

connections::connection_view(conn)

on.exit({
  pool::poolReturn(conn)
  pool::poolClose(pool)
},
  add = TRUE
)

public_summary_report_from_db <- dplyr::tbl(conn, I("public.summary_report")) |>
  dplyr::collect() |>
  setNames(
    c("property_name",
      "total_beds",
      "model_beds",
      "current_occupency",
      "total_new",
      "total_renewals",
      "total_leases",
      "prelease_percent",
      "prelease_percent",
      "prior_total_new",
      "prior_total_renewals",
      "prior_total_leases",
      "prior_prelease_percent",
      "yoy_variance_1",
      "yoy_variance_2",
      "seven_new",
      "seven_renewal",
      "seven_total",
      "seven_percent_gained",
      "beds_left",
      "vel_90",
      "vel_95",
      "vel_100")
  )

model_beds_for_db <- public_summary_report_from_db |>
  dplyr::select(
    property_name,
    model_beds
  ) |>
  dplyr::right_join(
    dplyr::select(
      report_summary_data_for_db,
      property_id,
      property_name
    ),
    by = "property_name"
  ) |>
  dplyr::select(
    entrata_property_id = property_id,
    property_name,
    model_beds
  ) |>
  dplyr::left_join(
    dplyr::tbl(
      conn,
      I("gmh.properties")
    ) |>
      dplyr::select(
        property_id,
        entrata_property_id
      ) |>
      dplyr::collect(),
    by = "entrata_property_id"
  ) |>
  dplyr::select(
    gmh_property_id = property_id,
    entrata_property_id,
    property_name,
    model_beds
  ) |>
  dplyr::mutate(
    model_beds = dplyr::case_when(
      property_name == "Academy 65" ~ 4,
      property_name == "Academy Lincoln" ~ 4,
      property_name == "ANOVA uCity Square" ~ 2,
      property_name == "Courts at Spring Mill Station" ~ 2,
      property_name == "Shortbread Lofts" ~ 3,
      property_name == "SOVA" ~ 4,
      property_name == "Station Nine" ~ 1,
      property_name == "The Academy at Frisco" ~ 4,
      property_name == "The Academy on Charles" ~ 4,
      property_name == "The Caswell at Runnymeade" ~ 1,
      property_name == "The Dean Campustown" ~ 4,
      property_name == "The Dean Reno" ~ 6,
      property_name == "Torre" ~ 6,
      TRUE ~ 0
    )
  ) |>
  dplyr::arrange(gmh_property_id)

DBI::dbExecute(conn, "DROP TABLE IF EXISTS \"gmh\".\"model_beds\" CASCADE")

DBI::dbExecute(
  conn,
  "CREATE TABLE IF NOT EXISTS \"gmh\".\"model_beds\" (
    gmh_property_id INT NOT NULL REFERENCES gmh.properties(property_id),
    entrata_property_id INT NOT NULL REFERENCES entrata.properties(property_id),
    property_name TEXT NOT NULL,
    model_beds INT NOT NULL,
    updated_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (gmh_property_id)
  )"
)

DBI::dbWriteTable(
  conn = conn,
  name = DBI::SQL("gmh.model_beds"),
  value = model_beds_for_db,
  append = TRUE
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
    entrata_property_id = property_id,
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
    property_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    entrata_property_id INTEGER REFERENCES \"entrata\".\"properties\" (property_id),
    property_name TEXT,
    property_type TEXT DEFAULT 'Student',
    is_disabled BOOLEAN NOT NULL DEFAULT FALSE,
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
  )"
)

gmh_properties_from_db <- dplyr::tbl(conn, I("gmh.properties")) |>
  dplyr::collect()

gmh_missing_properties <- gmh_investment_partners |>
  dplyr::filter(is.na(property_id))

gmh_properties <- gmh_properties_from_db |>
  dplyr::select(
    entrata_property_id = property_id,
    property_name,
    property_type,
    is_disabled
  ) |>
  dplyr::bind_rows(
    gmh_properties_not_in_entrata |>
      dplyr::select(property_name) |>
      dplyr::mutate(is_disabled = TRUE)
  )

DBI::dbAppendTable(
  conn,
  name = DBI::SQL("gmh.properties"),
  value = gmh_properties,
  append = TRUE
)

gmh_properties_to_add <- gmh_missing_properties |>
  dplyr::select(property_name) |>
  dplyr::mutate(
    property_type = "Student",
    website = NA_character_,
    address = NA_character_,
    email = NA_character_,
    is_disabled = TRUE,
    is_featured_property = FALSE,
    is_active = FALSE,
    year_built = NA_integer_,
    short_description = NA_character_,
    long_description = NA_character_,
    parent_property_id = NA_integer_
  )



# portfolios --------------------------------------------------------------

gmh_portfolios_from_db <- dplyr::tbl(conn, I("gmh.portfolios")) |>
  dplyr::collect()

gmh_portfolio_types_from_db <- dplyr::tbl(conn, I("gmh.portfolio_types")) |>
  dplyr::collect()

gmh_investment_partners_from_db <- dplyr::tbl(conn, I("gmh.investment_partners")) |>
  dplyr::collect()

public_investment_partners_from_db <- dplyr::tbl(conn, I("public.investment_partners")) |>
  dplyr::collect()

DBI::dbExecute(conn, "DROP TABLE IF EXISTS \"gmh\".\"investment_partners\" CASCADE")

DBI::dbExecute(
  conn,
  "CREATE TABLE \"gmh\".\"investment_partners\" (
    partner_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    partner_name TEXT NOT NULL,
    partner_type TEXT DEFAULT 'Equity Owner',
    property_id INTEGER REFERENCES \"gmh\".\"properties\" (property_id),
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
  )"
)

partner_names <- c(
  "AGC",
  "CBRE",
  "AEW",
  "Principal",
  "JHU",
  "CRG and Canyon",
  "Medistar Student Housing LLC"
)

gmh_investment_partners_for_db <- tibble::tibble(
  partner_name = partner_names
)



DBI::dbExecute(conn, "DROP TABLE IF EXISTS \"gmh\".\"investment_partners\" CASCADE")

DBI::dbExecute(
  conn,
  "CREATE TABLE \"gmh\".\"investment_partners\" (
    partner_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    partner_name TEXT NOT NULL,
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
  )"
)

DBI::dbAppendTable(
  conn,
  name = DBI::SQL("gmh.investment_partners"),
  value = gmh_investment_partners_for_db,
  append = TRUE
)


gmh_investment_partner_assignments <- public_investment_partners_from_db |>
  dplyr::left_join(
    entrata_properties_from_db |>
      dplyr::select(
        property_id,
        marketing_name
      ),
    by = c("property_name" = "marketing_name")
  ) |>
  dplyr::select(
    partner_name = investment_partner,
    entrata_property_id = property_id,
    property_name
  ) |>
  dplyr::filter(!is.na(partner_name)) |>
  dplyr::left_join(
    gmh_properties_from_db |>
      dplyr::select(
        property_id,
        property_name
      ),
    by = "property_name"
  ) |>
  dplyr::select(
    partner_name,
    gmh_property_id = property_id,
    entrata_property_id
  ) |>
  dplyr::left_join(
    dplyr::tbl(
      conn,
      I("gmh.investment_partners")
    ) |>
      dplyr::select(
        partner_id,
        partner_name
      ) |>
      dplyr::collect(),
    by = "partner_name"
    ) |>
  dplyr::select(
    partner_id,
    partner_name,
    gmh_property_id,
    entrata_property_id
  ) |>
  dplyr::arrange(gmh_property_id) |>
  dplyr::select(
    gmh_property_id,
    entrata_property_id,
    partner_id,
    partner_name
  )

DBI::dbExecute(conn, "DROP TABLE IF EXISTS \"gmh\".\"investment_partner_assignments\" CASCADE")

DBI::dbExecute(
  conn,
  "CREATE TABLE \"gmh\".\"investment_partner_assignments\" (
    gmh_property_id INTEGER REFERENCES \"gmh\".\"properties\" (property_id),
    entrata_property_id INTEGER REFERENCES \"entrata\".\"properties\" (property_id),
    partner_id INTEGER REFERENCES \"gmh\".\"investment_partners\" (partner_id),
    partner_name TEXT,
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (gmh_property_id, partner_id)
  )"
)

DBI::dbAppendTable(
  conn,
  name = DBI::SQL("gmh.investment_partner_assignments"),
  value = gmh_investment_partner_assignments,
  append = TRUE
)
