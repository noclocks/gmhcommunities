read_raw_market_survey_data <- function(wb_path, sheet, property_id, property_name, leasing_week, ...) {

  args <- list(
    property_detail = list(
      range = "X1",
      col_names = c("property_detail")
    ),
    property_summary = list(
      range = "A4:B19",
      col_names = c("key", "value")
    ),
    leasing_summary = list(
      range = "A22:B35",
      col_names = c("key", "value")
    ),
    short_term_leases = list(
      range = "A38:B43",
      col_names = c("key", "value")
    ),
    fees = list(
      range = "A46:C58",
      col_names = c("fees", "amount", "frequency")
    ),
    property_amenitites = list(
      range = "E4:H35",
      col_names = c("key", "x", "xx", "value")
    ),
    parking = list(
      range = "E39:H43",
      col_names = c("parking", "required", "included", "amount")
    ),
    unit_amenities = list(
      range = "K4:P30",
      col_names = c("key", "x", "xx", "xxx", "xxxx", "value")
    ),
    utilities_summary_1 = list(
      range = "K34:P36",
      col_names = c("utility", "all_inclusive", "cap", "allowance", "amount", "per_unit_bed")
    ),
    utilities_summary_2 = list(
      range = "K38:P42",
      col_names = c("utility", "included", "amount", "x", "xx", "per_unit_bed")
    ),
    parking = list(
      range = "E39:H43",
      col_names = c("parking_type", "required", "included", "amount")
    ),
    notes = list(
      range = "R20:R36",
      col_names = c("notes")
    ),
    office_hours = list(
      range = "R38:T44",
      col_names = c("day", "x", "hours")
    ),
    rents_by_floorplan = list(
      range = "A71:X81",
      col_names = c(
        "floorplan_type",
        "description",
        "x",
        "count",
        "sf_per_bed",
        "bed",
        "bath",
        "available",
        "market_rent_per_bed",
        "market_rent_per_sf",
        "concessions_gift_card",
        "concessions_one_time_rent",
        "concessions_monthly_rent",
        "effective_rent_per_bed",
        "effective_rent_per_sf",
        "additional_monthly_expenses_furniture",
        "additional_monthly_expenses_tv",
        "additional_monthly_expenses_electricity_and_gas",
        "additional_monthly_expenses_water",
        "additional_monthly_expenses_cable_and_internet",
        "additional_monthly_expenses_trash_and_valet",
        "additional_monthly_expenses_parking",
        "bundled_rent_per_bed",
        "bundled_rent_per_sf"
      )
    ),
    average_rents_by_unit_type = list(
      range = "A235:X242",
      col_names = c(
        "unit_type",
        "x",
        "xx",
        "xxx",
        "xxxx",
        "count",
        "sf_per_bed",
        "available",
        "market_rent_per_bed",
        "market_rent_per_sf",
        "concessions_gift_card",
        "concessions_one_time_rent",
        "concessions_monthly_rent",
        "effective_rent_per_bed",
        "effective_rent_per_sf",
        "additional_monthly_expenses_furniture",
        "additional_monthly_expenses_tv",
        "additional_monthly_expenses_electricity_and_gas",
        "additional_monthly_expenses_water",
        "additional_monthly_expenses_cable_and_internet",
        "additional_monthly_expenses_trash_and_valet",
        "additional_monthly_expenses_parking",
        "bundled_rent_per_bed",
        "bundled_rent_per_sf"
      )
    )
  )

  market_survey_data_lst <- purrr::map(
    args,
    function(x) {
      readxl::read_excel(
        wb_path,
        sheet = sheet,
        range = x$range,
        col_names = x$col_names,
        na = c("", "N/A")
      )
    }
  ) |>
    setNames(names(args)) |>
    purrr::map(
      function(df) {
        df |>
          dplyr::mutate(
            property_id = property_id,
            property_name = property_name,
            leasing_week = leasing_week
          )
      }
    ) |>
    purrr::map(
      function(df) {
        df |>
          # remove x, xx, xxx cols (only if they exist)
          dplyr::select(-matches("x|xx|xxx|xxxx"))
      }
    )

  # property detail ---------------------------------------------------------
  property_detail <- market_survey_data_lst$property_detail |>
    dplyr::pull(property_detail) |>
    stringr::str_split(": ") |>
    purrr::pluck(2)

  # property summary --------------------------------------------------------

  property_summary <- market_survey_data_lst$property_summary |>
    dplyr::filter(!is.na(key))

  property_summary_wide <- property_summary |>
    tidyr::pivot_wider(
      names_from = key,
      values_from = value
    )

  property_summary_lst <- list(
    property_summary = property_summary,
    property_summary_wide = property_summary_wide
  )


  # leasing summary ---------------------------------------------------------

  leasing_summary <- market_survey_data_lst$leasing_summary |>
    dplyr::filter(!is.na(key))

  leasing_summary_wide <- leasing_summary |>
    tidyr::pivot_wider(
      names_from = key,
      values_from = value
    )

  leasing_summary_lst <- list(
    leasing_summary = leasing_summary,
    leasing_summary_wide = leasing_summary_wide
  )


  # short term leases -------------------------------------------------------

  short_term_leases <- market_survey_data_lst$short_term_leases |>
    dplyr::filter(!is.na(key))

  short_term_leases_wide <- short_term_leases |>
    tidyr::pivot_wider(
      names_from = key,
      values_from = value
    )

  short_term_leases_lst <- list(
    short_term_leases = short_term_leases,
    short_term_leases_wide = short_term_leases_wide
  )


  # fees --------------------------------------------------------------------

  fees_fee_structure <- market_survey_data_lst$fees |>
    dplyr::filter(
      fees == "Fee Structure"
    ) |>
    dplyr::pull(amount)

  fees_text_amounts <- market_survey_data_lst$fees |>
    # find values in amount column that are not parseable to numeric
    dplyr::filter(
      !grepl("^\\d+\\.?\\d*$", amount)
    )

  fees <- market_survey_data_lst$fees |>
    dplyr::filter(
      !(fees %in% fees_text_amounts$fees)
    ) |>
    dplyr::mutate(
      amount = as.numeric(amount)
    )

  fees_wide <- fees |>
    tidyr::pivot_wider(
      names_from = fees,
      values_from = amount
    )

  fees_lst <- list(
    fee_structure = fees_fee_structure,
    fees_text_amounts = fees_text_amounts,
    fees = fees,
    fees_wide = fees_wide
  )

  # property amenities ------------------------------------------------------

  property_amenities_common_area_rating <- market_survey_data_lst$property_amenitites |>
    dplyr::filter(
      key == "Common Area Rating"
    ) |>
    dplyr::pull(value)

  property_amenitites <- market_survey_data_lst$property_amenitites |>
    dplyr::filter(
      !is.na(key),
      key != "Common Area Rating"
    )

  property_amenitites_wide <- property_amenitites |>
    tidyr::pivot_wider(
      names_from = key,
      values_from = value
    )


  # unit amenities ----------------------------------------------------------

  unit_amenities_unit_rating <- market_survey_data_lst$unit_amenities |>
    dplyr::filter(
      key == "Unit Rating"
    ) |>
    dplyr::pull(value)

  unit_amenities <- market_survey_data_lst$unit_amenities |>
    dplyr::filter(
      !is.na(key),
      key != "Unit Rating"
    )

  unit_amenities[13, "key"] <- "TV Included in Rent"
  unit_amenities[14, "key"] <- "TV Renatable Rate"
  unit_amenities[15, "key"] <- "TV Bedroom"
  unit_amenities[16, "key"] <- "TV Common Area"
  unit_amenities[18, "key"] <- "Furniture Included in Rent"
  unit_amenities[19, "key"] <- "Furniture Renatable Rate"
  unit_amenities[21, "key"] <- "Floor Premium"
  unit_amenities[22, "key"] <- "Poolside Premium"
  unit_amenities[23, "key"] <- "Top Floor Premium"
  unit_amenities[24, "key"] <- "View Premium"
  unit_amenities[25, "key"] <- "Other Premiums"

  unit_amenities <- unit_amenities |>
    dplyr::filter(
      !(key %in% c("TV", "Furniture", "Other Premiums"))
    )

  unit_amenities_wide <- unit_amenities |>
    tidyr::pivot_wider(
      names_from = key,
      values_from = value
    )

  amenities_lst <- list(
    property_amenities_common_area_rating = property_amenities_common_area_rating,
    property_amenitites = property_amenitites,
    property_amenitites_wide = property_amenitites_wide,
    unit_amenities_unit_rating = unit_amenities_unit_rating,
    unit_amenities = unit_amenities,
    unit_amenities_wide = unit_amenities_wide
  )

  # parking -----------------------------------------------------------------

  parking <- market_survey_data_lst$parking |>
    dplyr::filter(
      !is.na(parking)
    ) |>
    dplyr::rename(
      parking_type = parking
    ) |>
    dplyr::mutate(
      included = ifelse(included == "Yes", TRUE, FALSE),
      required = ifelse(required == "Yes", TRUE, FALSE)
    ) |>
    dplyr::select(
      property_id,
      property_name,
      parking_type,
      required,
      included,
      amount
    )

  parking_lst <- list(
    parking = parking
  )

  # utilities ---------------------------------------------------------------

  utilities_summary_2 <- market_survey_data_lst$utilities_summary_2 |>
    dplyr::filter(
      !is.na(utility)
    )

  utilities_lst <- list(
    utilities_summary_1 = market_survey_data_lst$utilities_summary_1,
    utilities_summary_2 = utilities_summary_2
  )


  # notes -------------------------------------------------------------------

  notes_leasing_special <- market_survey_data_lst$notes[2, ]
  notes_property_notes <- market_survey_data_lst$notes[12, ]

  notes_lst <- list(
    notes_leasing_special = notes_leasing_special,
    notes_property_notes = notes_property_notes
  )


  # office hours -----------------------------------------------------------

  office_hours <- market_survey_data_lst$office_hours |>
    dplyr::filter(
      !is.na(day)
    )

  office_hours_lst <- list(
    office_hours = office_hours
  )


  # rents -------------------------------------------------------------------

  rents_by_floorplan <- market_survey_data_lst$rents_by_floorplan |>
    dplyr::filter(
      !is.na(floorplan_type)
    )

  average_rents_by_unit_type <- market_survey_data_lst$average_rents_by_unit_type |>
    dplyr::filter(
      !is.na(unit_type)
    )

  rents_lst <- list(
    rents_by_floorplan = rents_by_floorplan,
    average_rents_by_unit_type = average_rents_by_unit_type
  )


  # finish ------------------------------------------------------------------

  list(
    property_detail,
    property_summary_lst,
    leasing_summary_lst,
    short_term_leases_lst,
    fees_lst,
    amenities_lst,
    utilities_lst,
    parking_lst,
    notes_lst,
    office_hours_lst,
    rents_lst
  ) |>
    setNames(
      c(
        "property_detail",
        "property_summary",
        "leasing_summary",
        "short_term_leases",
        "fees",
        "amenities",
        "utilities",
        "parking",
        "notes",
        "office_hours",
        "rents"
      )
    )

}

get_property_data_for_csv <- function(property, data_lst = market_survey_data_by_property) {

  property_data <- purrr::pluck(
    data_lst,
    property
  )

  # get property_summary data
  property_summary_data <- property_data$property_summary$property_summary
  property_summary_data_wide <- property_data$property_summary$property_summary_wide

  # get leasing_summary data
  leasing_summary_data <- property_data$leasing_summary$leasing_summary
  leasing_summary_data_wide <- property_data$leasing_summary$leasing_summary_wide

  # get short_term_leases data
  short_term_leases_data <- property_data$short_term_leases$short_term_leases
  short_term_leases_data_wide <- property_data$short_term_leases$short_term_leases_wide

  # get fees data
  fees_text_amounts_data <- property_data$fees$fees_text_amounts
  fees_data <- property_data$fees$fees
  fees_data_wide <- property_data$fees$fees_wide

  # get amenities data
  property_amenitites_data <- property_data$amenities$property_amenitites
  property_amenitites_data_wide <- property_data$amenities$property_amenitites_wide
  unit_amenities_data <- property_data$amenities$unit_amenities
  unit_amenities_data_wide <- property_data$amenities$unit_amenities_wide

  # get utilities data
  utilities_summary_1_data <- property_data$utilities$utilities_summary_1
  utilities_summary_2_data <- property_data$utilities$utilities_summary_2

  # get notes data
  notes_leasing_special_data <- property_data$notes$notes_leasing_special
  notes_property_notes_data <- property_data$notes$notes_property_notes

  # get office hours data
  office_hours_data <- property_data$office_hours$office_hours

  # get rents data
  rents_by_floorplan_data <- property_data$rents$rents_by_floorplan
  average_rents_by_unit_type_data <- property_data$rents$average_rents_by_unit_type

  # get file names
  property_summary_file <- paste0("market_survey_", property, "_property_summary.csv")
  property_summary_wide_file <- paste0("market_survey_", property, "_property_summary_wide.csv")
  leasing_summary_file <- paste0("market_survey_", property, "_leasing_summary.csv")
  leasing_summary_wide_file <- paste0("market_survey_", property, "_leasing_summary_wide.csv")
  short_term_leases_file <- paste0("market_survey_", property, "_short_term_leases.csv")
  short_term_leases_wide_file <- paste0("market_survey_", property, "_short_term_leases_wide.csv")
  fees_text_amounts_file <- paste0("market_survey_", property, "_fees_text_amounts.csv")
  fees_file <- paste0("market_survey_", property, "_fees.csv")
  fees_wide_file <- paste0("market_survey_", property, "_fees_wide.csv")
  property_amenitites_file <- paste0("market_survey_", property, "_property_amenitites.csv")
  property_amenitites_wide_file <- paste0("market_survey_", property, "_property_amenitites_wide.csv")
  unit_amenities_file <- paste0("market_survey_", property, "_unit_amenities.csv")
  unit_amenities_wide_file <- paste0("market_survey_", property, "_unit_amenities_wide.csv")
  utilities_summary_1_file <- paste0("market_survey_", property, "_utilities_summary_1.csv")
  utilities_summary_2_file <- paste0("market_survey_", property, "_utilities_summary_2.csv")
  notes_leasing_special_file <- paste0("market_survey_", property, "_notes_leasing_special.csv")
  notes_property_notes_file <- paste0("market_survey_", property, "_notes_property_notes.csv")
  office_hours_file <- paste0("market_survey_", property, "_office_hours.csv")
  rents_by_floorplan_file <- paste0("market_survey_", property, "_rents_by_floorplan.csv")
  average_rents_by_unit_type_file <- paste0("market_survey_", property, "_average_rents_by_unit_type.csv")

  # return list ready to pass to readr write csv
  list(
    property_summary = list(
      property_summary_data = property_summary_data,
      property_summary_file = property_summary_file,
      property_summary_data_wide = property_summary_data_wide,
      property_summary_wide_file = property_summary_wide_file
    ),
    leasing_summary = list(
      leasing_summary_data = leasing_summary_data,
      leasing_summary_file = leasing_summary_file,
      leasing_summary_data_wide = leasing_summary_data_wide,
      leasing_summary_wide_file = leasing_summary_wide_file
    ),
    short_term_leases = list(
      short_term_leases_data = short_term_leases_data,
      short_term_leases_file = short_term_leases_file,
      short_term_leases_data_wide = short_term_leases_data_wide,
      short_term_leases_wide_file = short_term_leases_wide_file
    ),
    fees = list(
      fees_text_amounts_data = fees_text_amounts_data,
      fees_text_amounts_file = fees_text_amounts_file,
      fees_data = fees_data,
      fees_file = fees_file,
      fees_data_wide = fees_data_wide,
      fees_wide_file = fees_wide_file
    ),
    amenities = list(
      property_amenitites_data = property_amenitites_data,
      property_amenitites_file = property_amenitites_file,
      property_amenitites_data_wide = property_amenitites_data_wide,
      property_amenitites_wide_file = property_amenitites_wide_file,
      unit_amenities_data = unit_amenities_data,
      unit_amenities_file = unit_amenities_file,
      unit_amenities_data_wide = unit_amenities_data_wide,
      unit_amenities_wide_file = unit_amenities_wide_file
    ),
    utilities = list(
      utilities_summary_1_data = utilities_summary_1_data,
      utilities_summary_1_file = utilities_summary_1_file,
      utilities_summary_2_data = utilities_summary_2_data,
      utilities_summary_2_file = utilities_summary_2_file
    ),
    notes = list(
      notes_leasing_special_data = notes_leasing_special_data,
      notes_leasing_special_file = notes_leasing_special_file,
      notes_property_notes_data = notes_property_notes_data,
      notes_property_notes_file = notes_property_notes_file
    ),
    office_hours = list(
      office_hours_data = office_hours_data,
      office_hours_file = office_hours_file
    ),
    rents = list(
      rents_by_floorplan_data = rents_by_floorplan_data,
      rents_by_floorplan_file = rents_by_floorplan_file,
      average_rents_by_unit_type_data = average_rents_by_unit_type_data,
      average_rents_by_unit_type_file = average_rents_by_unit_type_file
    )
  )

}

save_market_survey_csvs <- function(
  data_by_property,
  csv_dir = "data-raw/data/working",
  nest_folders_by_property = TRUE,
  nest_folders_by_section = TRUE
) {

  if (!dir.exists(csv_dir)) {
    dir.create(csv_dir, recursive = TRUE)
  }

  purrr::walk(
    names(data_by_property),
    function(x) {

      property <- x

      csv_dir <- if (nest_folders_by_property) {
        file.path(csv_dir, property)
      } else {
        csv_dir
      }

      if (!dir.exists(csv_dir)) {
        dir.create(csv_dir, recursive = TRUE)
      }

      property_data <- purrr::pluck(
        data_by_property,
        property
      )

      # get property_summary data
      property_summary_data <- property_data$property_summary$property_summary
      property_summary_data_wide <- property_data$property_summary$property_summary_wide

      # get leasing_summary data
      leasing_summary_data <- property_data$leasing_summary$leasing_summary
      leasing_summary_data_wide <- property_data$leasing_summary$leasing_summary_wide

      # get short_term_leases data
      short_term_leases_data <- property_data$short_term_leases$short_term_leases
      short_term_leases_data_wide <- property_data$short_term_leases$short_term_leases_wide

      # get fees data
      fees_text_amounts_data <- property_data$fees$fees_text_amounts
      fees_data <- property_data$fees$fees
      fees_data_wide <- property_data$fees$fees_wide

      # get amenities data
      property_amenitites_data <- property_data$amenities$property_amenitites
      property_amenitites_data_wide <- property_data$amenities$property_amenitites_wide
      unit_amenities_data <- property_data$amenities$unit_amenities
      unit_amenities_data_wide <- property_data$amenities$unit_amenities_wide

      # get utilities data
      utilities_summary_1_data <- property_data$utilities$utilities_summary_1
      utilities_summary_2_data <- property_data$utilities$utilities_summary_2

      # get notes data
      notes_leasing_special_data <- property_data$notes$notes_leasing_special
      notes_property_notes_data <- property_data$notes$notes_property_notes

      # get office hours data
      office_hours_data <- property_data$office_hours$office_hours

      # get rents data
      rents_by_floorplan_data <- property_data$rents$rents_by_floorplan
      average_rents_by_unit_type_data <- property_data$rents$average_rents_by_unit_type

      # get file names
      property_summary_file <- paste0("market_survey_", property, "_property_summary.csv")
      property_summary_wide_file <- paste0("market_survey_", property, "_property_summary_wide.csv")
      leasing_summary_file <- paste0("market_survey_", property, "_leasing_summary.csv")
      leasing_summary_wide_file <- paste0("market_survey_", property, "_leasing_summary_wide.csv")
      short_term_leases_file <- paste0("market_survey_", property, "_short_term_leases.csv")
      short_term_leases_wide_file <- paste0("market_survey_", property, "_short_term_leases_wide.csv")
      fees_text_amounts_file <- paste0("market_survey_", property, "_fees_text_amounts.csv")
      fees_file <- paste0("market_survey_", property, "_fees.csv")
      fees_wide_file <- paste0("market_survey_", property, "_fees_wide.csv")
      property_amenitites_file <- paste0("market_survey_", property, "_property_amenitites.csv")
      property_amenitites_wide_file <- paste0("market_survey_", property, "_property_amenitites_wide.csv")
      unit_amenities_file <- paste0("market_survey_", property, "_unit_amenities.csv")
      unit_amenities_wide_file <- paste0("market_survey_", property, "_unit_amenities_wide.csv")
      utilities_summary_1_file <- paste0("market_survey_", property, "_utilities_summary_1.csv")
      utilities_summary_2_file <- paste0("market_survey_", property, "_utilities_summary_2.csv")
      notes_leasing_special_file <- paste0("market_survey_", property, "_notes_leasing_special.csv")
      notes_property_notes_file <- paste0("market_survey_", property, "_notes_property_notes.csv")
      office_hours_file <- paste0("market_survey_", property, "_office_hours.csv")
      rents_by_floorplan_file <- paste0("market_survey_", property, "_rents_by_floorplan.csv")
      average_rents_by_unit_type_file <- paste0("market_survey_", property, "_average_rents_by_unit_type.csv")

      readr::write_csv(property_summary_data, file.path(csv_dir, property_summary_file))
      readr::write_csv(property_summary_data_wide, file.path(csv_dir, property_summary_wide_file))
      readr::write_csv(leasing_summary_data, file.path(csv_dir, leasing_summary_file))
      readr::write_csv(leasing_summary_data_wide, file.path(csv_dir, leasing_summary_wide_file))
      readr::write_csv(short_term_leases_data, file.path(csv_dir, short_term_leases_file))
      readr::write_csv(short_term_leases_data_wide, file.path(csv_dir, short_term_leases_wide_file))
      readr::write_csv(fees_text_amounts_data, file.path(csv_dir, fees_text_amounts_file))
      readr::write_csv(fees_data, file.path(csv_dir, fees_file))
      readr::write_csv(fees_data_wide, file.path(csv_dir, fees_wide_file))
      readr::write_csv(property_amenitites_data, file.path(csv_dir, property_amenitites_file))
      readr::write_csv(property_amenitites_data_wide, file.path(csv_dir, property_amenitites_wide_file))
      readr::write_csv(unit_amenities_data, file.path(csv_dir, unit_amenities_file))
      readr::write_csv(unit_amenities_data_wide, file.path(csv_dir, unit_amenities_wide_file))
      readr::write_csv(utilities_summary_1_data, file.path(csv_dir, utilities_summary_1_file))
      readr::write_csv(utilities_summary_2_data, file.path(csv_dir, utilities_summary_2_file))
      readr::write_csv(notes_leasing_special_data, file.path(csv_dir, notes_leasing_special_file))
      readr::write_csv(notes_property_notes_data, file.path(csv_dir, notes_property_notes_file))
      readr::write_csv(office_hours_data, file.path(csv_dir, office_hours_file))
      readr::write_csv(rents_by_floorplan_data, file.path(csv_dir, rents_by_floorplan_file))
      readr::write_csv(average_rents_by_unit_type_data, file.path(csv_dir, average_rents_by_unit_type_file))

      cli::cli_alert_info("Saved data for property {.field {property}} to path: {.path {csv_dir}}")

    }
  )

  cli::cli_alert_info("Saved all market survey data to csvs at path: {.path {csv_dir}}")

}
