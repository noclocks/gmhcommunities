
require(httr2)
require(config)

source("data-raw/R/entrata.R")

entrata_config <- config::get("entrata")

resp_properties <- entrata_properties()

entrata_properties_db <- resp_properties$property_tbl_base


  entrata_property_lease_term_windows_db <- resp_properties$property_lease_term_windows |>
  dplyr::select(
    window_id = lease_term_window_id,
    property_id,
    lease_term_id,
    lease_term_name,

  )


resp_data <- resp_properties |>
  purrr::pluck(
    "response",
    "result",
    "PhysicalProperty",
    "Property"
  )


library(httr2)
library(tidyverse)
library(jsonlite)
library(lubridate)

resp_getProperties <- jsonlite::fromJSON("properties.json", simplifyVector = FALSE)

entrata_properties_data <- purrr::pluck(
  resp_getProperties,
  "response",
  "result",
  "PhysicalProperty",
  "Property"
)

entrata_properties_df <- entrata_properties_data |>
  purrr::map_dfr(
    ~ tibble::tibble(
      property_id = .x$PropertyID,
      marketing_name = .x$MarketingName,
      property_type = .x$Type,
      website = .x$webSite,
      address = .x$Address$Address,
      city = .x$Address$City,
      state = .x$Address$State,
      postal_code = .x$Address$PostalCode,
      country = .x$Address$Country,
      email = .x$Address$Email,
      is_disabled = .x$IsDisabled,
      is_featured_property = .x$IsFeaturedProperty
    )
  )

entrata_property_addresses <- entrata_properties_data |>
  purrr::map_dfr(
    function(prop) {
      property_id <- prop$PropertyID
      addresses <- purrr::pluck(prop, "Addresses", "Address")
      purrr::map_dfr(
        addresses,
        ~ tibble::tibble(
          property_id = property_id,
          address_type = .x$AddressType,
          street = .x$Address,
          city = .x$City,
          state = .x$State,
          postal_code = .x$PostalCode,
          country = .x$Country
        )
      )
    }
  ) |>
  dplyr::arrange(
    property_id,
    address_type
  )

entrata_property_hours <- entrata_properties_data |>
  purrr::map_dfr(
    function(prop) {
      property_id <- prop$PropertyID
      hours <- purrr::pluck(prop, "PropertyHours", "OfficeHours", "OfficeHour")
      purrr::map_dfr(
        hours,
        ~ tibble::tibble(
          property_id = property_id,
          day_of_week = .x$Day,
          availability_type = .x$AvailabilityType,
          open_time = .x$OpenTime,
          close_time = .x$CloseTime
        )
      )
    }
  ) |>
  dplyr::arrange(
    property_id,
    day_of_week
  )

entrata_property_lease_terms <- entrata_properties_data |>
  purrr::map_dfr(
    function(prop) {
      property_id <- prop$PropertyID
      terms <- purrr::pluck(prop, "LeaseTerms", "LeaseTerm")
      purrr::map_dfr(
        terms,
        ~ tibble::tibble(
          property_id = property_id,
          term_id = .x$Id,
          term_name = .x$Name,
          term_months = .x$TermMonths,
          is_prospect = .x$IsProspect,
          is_renewal = .x$IsRenewal
        )
      )
    }
  ) |>
  dplyr::arrange(
    property_id,
    term_name
  )

entrata_property_lease_term_windows <- resp_properties$property_lease_term_windows |>
  dplyr::select(
    property_id,
    lease_term_id,
    window_id = lease_term_window_id,
    window_start_date = lease_term_window_start_date,
    window_end_date = lease_term_window_end_date
  ) |>
  dplyr::arrange(
    property_id,
    lease_term_id,
    window_start_date
  )

entrata_property_space_options <- resp_properties$property_space_options |>
  dplyr::select(
    property_id,
    space_option_id,
    space_option_name
  ) |>
  dplyr::arrange(
    property_id,
    space_option_id
  )


entrata_property_phone_numbers <- resp_properties$property_phones |>
  dplyr::select(
    property_id,
    phone_number,
    phone_number_type
  ) |>
  dplyr::arrange(
    property_id,
    phone_number_type
  )

entrata_property_post_months <- resp_properties$property_post_months |>
  dplyr::select(
    property_id,
    ar_post_month,
    ap_post_month,
    gl_post_month
  ) |>
  dplyr::arrange(
    property_id
  )


entrata_property_custom_keys <- resp_properties$property_custom_keys |>
  dplyr::select(
    property_id,
    key,
    value
  ) |>
  dplyr::arrange(
    property_id,
    key
  )

# database ----------------------------------------------------------------

con <- db_connect()

entrata_properties_for_db <- entrata_properties_df |>
  dplyr::mutate(
    is_disabled = as.logical(is_disabled),
    is_featured_property = as.logical(is_featured_property)
  ) |>
  dplyr::select(
    property_id,
    marketing_name,
    property_type,
    website,
    address,
    email,
    is_disabled,
    is_featured_property
  ) |>
  dplyr::left_join(
    dplyr::select(
      entrata_property_data$property_tbl_base,
      property_id,
      parent_property_id,
      year_built = property_year_built,
      short_description = property_short_description,
      long_description = property_long_description
    ),
    by = "property_id"
  ) |>
  dplyr::select(
    property_id,
    marketing_name,
    property_type,
    website,
    address,
    email,
    is_disabled,
    is_featured_property,
    parent_property_id,
    year_built,
    short_description,
    long_description
  ) |>
  dplyr::mutate(
    year_built = as.integer(year_built)
  ) |>
  dplyr::arrange(
    property_id
  )

# DBI::sqlCreateTable(
#   con,
#   table = DBI::SQL("entrata.properties"),
#   fields = entrata_properties_for_db,
#   row.names = FALSE
# )

DBI::dbExecute(con, 'DROP TABLE IF EXISTS "entrata"."properties"')

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

entrata_property_addresses_for_db <- entrata_property_addresses

DBI::dbExecute(con, 'DROP TABLE IF EXISTS "entrata"."property_addresses"')

DBI::dbExecute(
  con,
  'CREATE TABLE "entrata"."property_addresses" (
  "property_id" BIGINT REFERENCES entrata.properties(property_id),
  "address_type" TEXT NOT NULL CHECK (address_type IN (\'Primary\', \'Mailing\')),
  "street" TEXT NOT NULL,
  "city" TEXT NOT NULL,
  "state" TEXT NOT NULL,
  "postal_code" TEXT NOT NULL,
  "country" TEXT NOT NULL DEFAULT \'US\',
  PRIMARY KEY (property_id, address_type)
);'
)

DBI::dbWriteTable(
  conn = con,
  name = DBI::SQL("entrata.property_addresses"),
  value = entrata_property_addresses_for_db,
  append = TRUE
  # overwrite = TRUE
)

entrata_property_hours_for_db <- entrata_property_hours |>
  dplyr::mutate(
    open_time = hms::parse_hm(open_time),
    close_time = hms::parse_hm(close_time)
  ) |>
  dplyr::arrange(
    property_id,
    day_of_week,
    open_time
  )

DBI::dbExecute(con, 'DROP TABLE IF EXISTS "entrata"."property_hours"')

DBI::dbExecute(
  con,
  'CREATE TABLE "entrata"."property_hours" (
  "property_id" BIGINT REFERENCES entrata.properties(property_id),
  "day_of_week" TEXT NOT NULL CHECK (day_of_week IN (\'Sunday\', \'Monday\', \'Tuesday\', \'Wednesday\', \'Thursday\', \'Friday\', \'Saturday\')),
  "availability_type" TEXT NOT NULL CHECK (availability_type IN (\'Open\', \'Closed\', \'By Appointment Only\')),
  "open_time" TIME,
  "close_time" TIME,
  PRIMARY KEY (property_id, day_of_week)
);'
)

DBI::dbWriteTable(
  conn = con,
  name = DBI::SQL("entrata.property_hours"),
  value = entrata_property_hours_for_db,
  append = TRUE
  # overwrite = TRUE
)

entrata_property_lease_terms_for_db <- entrata_property_lease_terms |>
  dplyr::arrange(
    property_id,
    term_id
  )

DBI::dbExecute(con, 'DROP TABLE IF EXISTS "entrata"."property_lease_terms"')

DBI::dbExecute(
  con,
  'CREATE TABLE "entrata"."property_lease_terms" (
  "property_id" BIGINT REFERENCES entrata.properties(property_id),
  "term_id" BIGINT NOT NULL,
  "term_name" TEXT,
  "term_months" INT,
  "is_prospect" BOOLEAN,
  "is_renewal" BOOLEAN,
  PRIMARY KEY (property_id, term_id)
);'
)

DBI::dbWriteTable(
  conn = con,
  name = DBI::SQL("entrata.property_lease_terms"),
  value = entrata_property_lease_terms_for_db,
  append = TRUE
  # overwrite = TRUE
)

entrata_lease_term_windows_for_db <- entrata_property_lease_term_windows |>
  dplyr::arrange(
    property_id,
    lease_term_id,
    window_start_date
  )

DBI::dbExecute(con, 'DROP TABLE IF EXISTS "entrata"."property_lease_term_windows"')

DBI::dbExecute(
  con,
  'CREATE TABLE "entrata"."property_lease_term_windows" (
  "property_id" BIGINT REFERENCES entrata.properties(property_id),
  "lease_term_id" BIGINT NOT NULL,
  "window_id" BIGINT NOT NULL,
  "window_start_date" DATE NOT NULL,
  "window_end_date" DATE NOT NULL,
  PRIMARY KEY (property_id, lease_term_id, window_id)
);'
)

DBI::dbWriteTable(
  conn = con,
  name = DBI::SQL("entrata.property_lease_term_windows"),
  value = entrata_lease_term_windows_for_db,
  append = TRUE
  # overwrite = TRUE
)

entrata_space_options_for_db <- entrata_property_space_options |>
  dplyr::select(
    space_option_id,
    space_option_name
  ) |>
  dplyr::distinct() |>
  dplyr::arrange(
    space_option_id
  )

DBI::dbExecute(con, 'DROP TABLE IF EXISTS "entrata"."space_options"')

DBI::dbExecute(
  con,
  'CREATE TABLE "entrata"."space_options" (
  "space_option_id" INT PRIMARY KEY,
  "space_option_name" TEXT NOT NULL
);'
)

DBI::dbWriteTable(
  conn = con,
  name = DBI::SQL("entrata.space_options"),
  value = entrata_space_options_for_db,
  append = TRUE
  # overwrite = TRUE
)

entrata_property_space_options_for_db <- entrata_property_space_options |>
  dplyr::arrange(
    property_id,
    space_option_id
  )

DBI::dbExecute(con, 'DROP TABLE IF EXISTS "entrata"."property_space_options"')

DBI::dbExecute(
  con,
  'CREATE TABLE "entrata"."property_space_options" (
  "property_id" BIGINT REFERENCES entrata.properties(property_id),
  "space_option_id" INT NOT NULL REFERENCES entrata.space_options(space_option_id),
  "space_option_name" TEXT NOT NULL,
  PRIMARY KEY (property_id, space_option_id)
);'
)

DBI::dbWriteTable(
  conn = con,
  name = DBI::SQL("entrata.property_space_options"),
  value = entrata_property_space_options_for_db,
  append = TRUE
  # overwrite = TRUE
)

entrata_property_phone_numbers_for_db <- entrata_property_phone_numbers |>
  dplyr::mutate(
    phone_number_type = tolower(phone_number_type)
  ) |>
  dplyr::arrange(
    property_id,
    phone_number_type
  )

DBI::dbExecute(con, 'DROP TABLE IF EXISTS "entrata"."property_phone_numbers"')

DBI::dbExecute(
  con,
  'CREATE TABLE "entrata"."property_phone_numbers" (
  "property_id" BIGINT REFERENCES entrata.properties(property_id),
  "phone_number" TEXT NOT NULL,
  "phone_number_type" TEXT NOT NULL,
  PRIMARY KEY (property_id, phone_number_type)
);'
)

DBI::dbWriteTable(
  conn = con,
  name = DBI::SQL("entrata.property_phone_numbers"),
  value = entrata_property_phone_numbers_for_db,
  append = TRUE
  # overwrite = TRUE
)

entrata_property_post_months_for_db <- entrata_property_post_months |>
  dplyr::mutate(
    ar_post_month = lubridate::ym(ar_post_month),
    ap_post_month = lubridate::ym(ap_post_month),
    gl_post_month = lubridate::ym(gl_post_month)
  ) |>
  dplyr::arrange(
    property_id,
    ar_post_month
  )

DBI::dbExecute(con, 'DROP TABLE IF EXISTS "entrata"."property_post_months"')

DBI::dbExecute(
  con,
  'CREATE TABLE "entrata"."property_post_months" (
  "property_id" BIGINT REFERENCES entrata.properties(property_id),
  "ar_post_month" VARCHAR(10) NOT NULL,
  "ap_post_month" VARCHAR(10) NOT NULL,
  "gl_post_month" VARCHAR(10) NOT NULL,
  PRIMARY KEY (property_id)
);'
)

DBI::dbWriteTable(
  conn = con,
  name = DBI::SQL("entrata.property_post_months"),
  value = entrata_property_post_months_for_db,
  append = TRUE
  # overwrite = TRUE
)



resp_properties <- entrata_properties()

properties_for_db <- resp_properties$property_tbl_base |>
  dplyr::mutate(
    property_id = as.character(property_id),
    property_is_disabled = ifelse(property_is_disabled == 0, FALSE, TRUE),
    property_is_featured = ifelse(property_is_featured == 0, FALSE, TRUE),
    property_year_built = as.integer(property_year_built)
  )

DBI::dbWriteTable(
  conn = con,
  name = DBI::SQL("entrata.properties"),
  value = properties_for_db,
  overwrite = TRUE
)


# addresses ---------------------------------------------------------------

property_addresses <- resp_properties$property_addresses

dplyr::glimpse(property_addresses)

property_addresses_primary <- property_addresses |>
  dplyr::mutate(
    address_type = "Primary",
    address = primary_address_full,
    street = primary_address_street,
    city = primary_address_city,
    state = primary_address_state,
    postal_code = primary_address_zip,
    country = primary_address_country
  ) |>
  dplyr::select(
    property_id,
    address_type,
    address,
    city,
    state,
    postal_code,
    country
  )

property_addresses_mailing <- property_addresses |>
  dplyr::mutate(
    address_type = "Mailing",
    address = mailing_address_full,
    street = mailing_address_street,
    city = mailing_address_city,
    state = mailing_address_state,
    postal_code = mailing_address_zip,
    country = mailing_address_country
  ) |>
  dplyr::select(
    property_id,
    address_type,
    address,
    city,
    state,
    postal_code,
    country
  )

property_addresses_for_db <- dplyr::bind_rows(
  property_addresses_primary,
  property_addresses_mailing
)

DBI::dbWriteTable(
  conn = con,
  name = DBI::SQL("entrata.property_addresses"),
  value = property_addresses_for_db,
  overwrite = TRUE
)

# lease term windows ------------------------------------------------------

property_lease_term_windows <- resp_properties$property_lease_term_windows

DBI::dbWriteTable(
  conn = con,
  name = DBI::SQL("entrata.property_lease_term_windows"),
  value = property_lease_term_windows,
  overwrite = TRUE
)

# post months -------------------------------------------------------------

DBI::dbWriteTable(
  conn = con,
  name = DBI::SQL("entrata.property_post_months"),
  value = resp_properties$property_post_months,
  overwrite = TRUE
)


# space_options -----------------------------------------------------------

DBI::dbWriteTable(
  conn = con,
  name = DBI::SQL("entrata.property_space_options"),
  value = resp_properties$property_space_options,
  overwrite = TRUE
)

# phones ------------------------------------------------------------------

property_phones <- resp_properties$property_phones |>
  dplyr::select(
    property_id,
    phone_number,
    phone_number_type,
    phone_number_intl,
    phone_number_link,
    phone_number_link_html
  )

DBI::dbWriteTable(
  conn = con,
  name = DBI::SQL("entrata.property_phones"),
  value = property_phones,
  overwrite = TRUE
)


# hours ------------------------------------------------------------------

DBI::dbWriteTable(
  conn = con,
  name = DBI::SQL("entrata.property_hours"),
  value = resp_properties$property_hours,
  overwrite = TRUE
)
