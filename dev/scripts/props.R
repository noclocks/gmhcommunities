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
      property_marketing_name = .x$MarketingName,
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


# database ----------------------------------------------------------------


properties_tbl_ddl <- "
CREATE TABLE IF NOT EXISTS entrata.properties (
  property_id BIGINT PRIMARY KEY,
  marketing_name TEXT,
  property_type TEXT,
  website TEXT,
  address TEXT,
  city TEXT,
  state TEXT,
  postal_code TEXT,
  country TEXT,
  email TEXT,
  is_disabled BOOLEAN,
  is_featured_property BOOLEAN
);
