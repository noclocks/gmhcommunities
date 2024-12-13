

pool <- db_connect()
conn <- pool::poolCheckout(pool)
connections::connection_view(conn)

on.exit(pool::poolReturn(conn, pool), add = TRUE)
on.exit(pool::poolClose(pool), add = TRUE)

db_properties_tbl <- dbplyr::in_schema("mkt", "properties")
db_competitors_tbl <- dbplyr::in_schema("mkt", "competitors")
db_surveys_tbl <- dbplyr::in_schema("mkt", "surveys")
db_sections_tbl <- dbplyr::in_schema("mkt", "sections")
db_fields_tbl <- dbplyr::in_schema("mkt", "fields")
db_responses_tbl <- dbplyr::in_schema("mkt", "responses")
db_history_tbl <- dbplyr::in_schema("mkt", "history")

# properties --------------------------------------------------------------




# surveys -----------------------------------------------------------------

db_drop_tbl(conn, tbl = "surveys", schema = "mkt", cascade = TRUE)

DBI::dbExecute(
  conn,
  'CREATE TABLE "mkt"."surveys" (
  "survey_id" INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  "property_id" TEXT REFERENCES mkt.properties(property_id),
  "leasing_week_id" INTEGER REFERENCES mkt.leasing_weeks(leasing_week_id),
  "survey_date" DATE NOT NULL,
  "created_by" INTEGER REFERENCES auth.users(user_id),
  "created_at" TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
  "updated_at" TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);'
)

commonwealth_property_id <- "739085"
current_leasing_week_id <- db_get_weekly_period_id(conn)

survey_data_for_db <- tibble::tibble(
  property_id = c(commonwealth_property_id),
  leasing_week_id = c(current_leasing_week_id),
  survey_date = c(lubridate::ymd(lubridate::today())),
  created_by = c(1)
)

DBI::dbWriteTable(
  conn = conn,
  name = DBI::SQL("mkt.surveys"),
  value = survey_data_for_db,
  append = TRUE
  # overwrite = TRUE
)


# sections ----------------------------------------------------------------

mktsurvey_sections <- c(
  "property_summary",
  "leasing_summary",
  "short_term_leases",
  "fees",
  "amenities", # property and unit
  "parking",
  "utilities",
  "office_hours",
  "notes",
  "rents"
)

db_drop_tbl(conn, tbl = "sections", schema = "mkt", cascade = TRUE)

DBI::dbExecute(
  conn,
  'CREATE TABLE "mkt"."sections" (
  "section_id" INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  "section_name" TEXT NOT NULL,
  "section_description" TEXT
);'
)

section_data_for_db <- tibble::tibble(
  section_name = mktsurvey_sections
)

DBI::dbWriteTable(
  conn = conn,
  name = DBI::SQL("mkt.sections"),
  value = section_data_for_db,
  append = TRUE
  # overwrite = TRUE
)

# fields ------------------------------------------------------------------

db_drop_tbl(conn, tbl = "fields", schema = "mkt", cascade = TRUE)

property_summary_inputs <- readr::read_csv("data-raw/data/working/market_survey/property_summary_inputs.csv")

property_summary_fields_for_db <- property_summary_inputs |>
  dplyr::transmute(
    section_id = 1, # property summary
    order = dplyr::row_number(),
    field_name = name,
    data_type = ifelse(type == "mc", "TEXT", toupper(type)),
    input_type = type,
    input_function = input_function,
    input_label = label,
    input_placeholder = placeholder,
    icon = icon,
    required = required,
    choices = choices,
    default = default,
    validation = NA,
    field_description = NA,
    is_disabled = FALSE
  )

dplyr::glimpse(property_summary_inputs)

DBI::dbExecute(
  conn,
  'CREATE TABLE "mkt"."fields" (
  "field_id" INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  "section_id" INTEGER REFERENCES mkt.sections(section_id),
  "field_name" TEXT NOT NULL,
  "data_type" TEXT NOT NULL,

  "field_description" TEXT
);'
)











# DBI::sqlCreateTable(
#   con,
#   table = DBI::SQL("entrata.properties"),
#   fields = entrata_properties_for_db,
#   row.names = FALSE
# )



# database ----------------------------------------------------------------





DBI::dbExecute(
  con,
  'CREATE TABLE "entrata"."properties" (
  "property_id" BIGINT PRIMARY KEY,
  "marketing_name" TEXT NOT NULL,
  "property_type" TEXT NOT NULL CHECK (property_type IN (\'Student\', \'Apartment\')),
  "website" TEXT,
  "address" TEXT,
  "email" TEXT,
  "is_disabled" BOOLEAN DEFAULT FALSE,
  "is_featured_property" BOOLEAN DEFAULT FALSE,
  "parent_property_id" BIGINT REFERENCES entrata.properties(property_id),
  "year_built" INT,
  "short_description" TEXT,
  "long_description" TEXT
);'
)

DBI::dbWriteTable(
  conn = con,
  name = DBI::SQL("entrata.properties"),
  value = entrata_properties_for_db,
  append = TRUE
  # overwrite = TRUE
)
