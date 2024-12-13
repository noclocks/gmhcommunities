property_summary_data <- commonwealth_market_survey_data$property_summary$property_summary_wide |>
  dplyr::bind_rows(boylston_market_survey_data$property_summary$property_summary_wide) |>
  dplyr::bind_rows(bower_market_survey_data$property_summary$property_summary_wide) |>
  dplyr::bind_rows(van_ness_market_survey_data$property_summary$property_summary_wide) |>
  janitor::clean_names() |>
  dplyr::mutate_all(dplyr::na_if, "n/a") |>
  dplyr::mutate(
    property_id = tolower(property_id),
    leasing_week_start = as.Date(leasing_week),
    property_rating = as.numeric(property_rating),
    year_built_renovated = as.integer(year_built_renovated),
    most_recent_sale = openxlsx::convertToDate(most_recent_sale),
    # "0.1 Miles", "1 mile", "0.2 miles", "1 Miles"
    # get everything before first space
    distance_from_campus = stringr::str_extract(distance_from_campus, "^[^ ]+") |>
      as.numeric()
  )

dplyr::glimpse(property_summary_data)


leasing_summary_data <- commonwealth_market_survey_data$leasing_summary$leasing_summary_wide |>
  dplyr::bind_rows(boylston_market_survey_data$leasing_summary$leasing_summary_wide) |>
  dplyr::bind_rows(bower_market_survey_data$leasing_summary$leasing_summary_wide) |>
  dplyr::bind_rows(van_ness_market_survey_data$leasing_summary$leasing_summary_wide) |>
  janitor::clean_names() |>
  dplyr::mutate_all(dplyr::na_if, "n/a") |>
  dplyr::mutate(
    property_id = tolower(property_id),
    leasing_week_start = as.Date(leasing_week),
    lease_launch_date = openxlsx::convertToDate(lease_launch_date),
    renewal_launch_date = openxlsx::convertToDate(renewal_launch_date),
    current_occupancy = as.numeric(current_occupancy),
    last_year_occupancy = current_occupancy,
    current_pre_lease = as.numeric(current_pre_lease),
    last_year_pre_lease = current_pre_lease,
    total_renewals = as.integer(total_renewals),
    total_new_leases = as.integer(total_new_leases),
    total_leases_weekly = as.integer(total_leases_weekly),
    traffic_weekly = as.integer(traffic_weekly),
    incentive_amount = as.numeric(incentive_amount),
    data_last_updated = openxlsx::convertToDate(data_last_updated)
  )

dplyr::glimpse(leasing_summary_data)

short_term_leases_data <- commonwealth_market_survey_data$short_term_leases$short_term_leases_wide |>
  dplyr::bind_rows(boylston_market_survey_data$short_term_leases$short_term_leases_wide) |>
  dplyr::bind_rows(bower_market_survey_data$short_term_leases$short_term_leases_wide) |>
  dplyr::bind_rows(van_ness_market_survey_data$short_term_leases$short_term_leases_wide) |>
  janitor::clean_names() |>
  dplyr::mutate_all(dplyr::na_if, "n/a") |>
  setNames(c(
    "property_id",
    "property_name",
    "leasing_week_start",
    "five_month_term_available",
    "five_month_term_premium",
    "five_month_term_quantity",
    "ten_month_term_available",
    "ten_month_term_premium",
    "ten_month_term_quantity"
  )) |>
  dplyr::mutate(
    property_id = tolower(property_id),
    leasing_week_start = as.Date(leasing_week_start),
    five_month_term_available = ifelse(
      five_month_term_available == "Yes", TRUE, FALSE
    ),
    five_month_term_premium = as.numeric(five_month_term_premium),
    five_month_term_quantity = as.integer(five_month_term_quantity),
    ten_month_term_available = ifelse(
      ten_month_term_available == "Yes", TRUE, FALSE
    ),
    ten_month_term_premium = as.numeric(ten_month_term_premium),
    ten_month_term_quantity = as.integer(ten_month_term_quantity)
  )

dplyr::glimpse(short_term_leases_data)

fees_data_new <- tibble::tribble(
                   ~property_id,             ~property_name, ~leasing_week_start,           ~fees_name, ~fees_amount, ~fees_frequency,
                       "739085", "1047 Commonwealth Avenue",        "2024-12-01",    "Application Fee",         "$0",       "Monthly",
                       "739085", "1047 Commonwealth Avenue",        "2024-12-01", "Administration Fee",         "$0",       "Monthly",
                       "739085", "1047 Commonwealth Avenue",        "2024-12-01", "Utility Set Up Fee",         "$0",       "Monthly",
                       "739085", "1047 Commonwealth Avenue",        "2024-12-01",    "Utility Deposit",         "$0",       "Monthly",
                       "739085", "1047 Commonwealth Avenue",        "2024-12-01",        "Amenity Fee",         "$0",       "Monthly",
                       "739085", "1047 Commonwealth Avenue",        "2024-12-01",    "Common Area Fee",         "$0",       "Monthly",
                       "739085", "1047 Commonwealth Avenue",        "2024-12-01",     "Smart Home Fee",         "$0",       "Monthly",
                       "739085", "1047 Commonwealth Avenue",        "2024-12-01",    "Restoration Fee",         "$0",       "Monthly",
                       "739085", "1047 Commonwealth Avenue",        "2024-12-01",   "Security Deposit",     "$1,000",       "Monthly",
                       "739085", "1047 Commonwealth Avenue",        "2024-12-01",            "Pet Fee",         "$0",       "Monthly",
                       "739085", "1047 Commonwealth Avenue",        "2024-12-01",        "Pet Deposit",         "$0",       "Monthly",
                       "739085", "1047 Commonwealth Avenue",        "2024-12-01",           "Pet Rent",         "$0",       "Monthly",
                     "boylston",            "1330 Boylston",        "2024-12-01",    "Application Fee",       "$250",       "Monthly",
                     "boylston",            "1330 Boylston",        "2024-12-01", "Administration Fee",         "$0",       "Monthly",
                     "boylston",            "1330 Boylston",        "2024-12-01", "Utility Set Up Fee",         "$0",       "Monthly",
                     "boylston",            "1330 Boylston",        "2024-12-01",    "Utility Deposit",         "$0",       "Monthly",
                     "boylston",            "1330 Boylston",        "2024-12-01",        "Amenity Fee",         "$0",       "Monthly",
                     "boylston",            "1330 Boylston",        "2024-12-01",    "Common Area Fee",         "$0",       "Monthly",
                     "boylston",            "1330 Boylston",        "2024-12-01",     "Smart Home Fee",         "$0",       "Monthly",
                     "boylston",            "1330 Boylston",        "2024-12-01",    "Restoration Fee",         "$0",       "Monthly",
                     "boylston",            "1330 Boylston",        "2024-12-01",   "Security Deposit",     "$1,000",       "Monthly",
                     "boylston",            "1330 Boylston",        "2024-12-01",            "Pet Fee",         "$0",       "Monthly",
                     "boylston",            "1330 Boylston",        "2024-12-01",        "Pet Deposit",         "$0",       "Monthly",
                     "boylston",            "1330 Boylston",        "2024-12-01",     "Per Rent - Dog",       "$100",       "Monthly",
                     "boylston",            "1330 Boylston",        "2024-12-01",     "Pet Rent - Cat",        "$50",       "Monthly",
                      "vanness",                 "Van Ness",        "2024-12-01",    "Application Fee",         "$0",       "Monthly",
                      "vanness",                 "Van Ness",        "2024-12-01", "Administration Fee",         "$0",       "Monthly",
                      "vanness",                 "Van Ness",        "2024-12-01", "Utility Set Up Fee",         "$0",       "Monthly",
                      "vanness",                 "Van Ness",        "2024-12-01",    "Utility Deposit",         "$0",       "Monthly",
                      "vanness",                 "Van Ness",        "2024-12-01",        "Amenity Fee",         "$0",       "Monthly",
                      "vanness",                 "Van Ness",        "2024-12-01",    "Common Area Fee",         "$0",       "Monthly",
                      "vanness",                 "Van Ness",        "2024-12-01",     "Smart Home Fee",         "$0",       "Monthly",
                      "vanness",                 "Van Ness",        "2024-12-01",    "Restoration Fee",         "$0",       "Monthly",
                      "vanness",                 "Van Ness",        "2024-12-01",   "Security Deposit",       "$500",       "Monthly",
                      "vanness",                 "Van Ness",        "2024-12-01",            "Pet Fee",         "$0",       "Monthly",
                      "vanness",                 "Van Ness",        "2024-12-01",        "Pet Deposit",         "$0",       "Monthly",
                      "vanness",                 "Van Ness",        "2024-12-01",     "Per Rent - Dog",       "$100",       "Monthly",
                      "vanness",                 "Van Ness",        "2024-12-01",     "Pet Rent - Cat",        "$50",       "Monthly",
                        "bower",                    "Bower",        "2024-12-01",    "Application Fee",         "$0",       "Monthly",
                        "bower",                    "Bower",        "2024-12-01", "Administration Fee",         "$0",       "Monthly",
                        "bower",                    "Bower",        "2024-12-01", "Utility Set Up Fee",         "$0",       "Monthly",
                        "bower",                    "Bower",        "2024-12-01",    "Utility Deposit",         "$0",       "Monthly",
                        "bower",                    "Bower",        "2024-12-01",        "Amenity Fee",         "$0",       "Monthly",
                        "bower",                    "Bower",        "2024-12-01",    "Common Area Fee",         "$0",       "Monthly",
                        "bower",                    "Bower",        "2024-12-01",     "Smart Home Fee",         "$0",       "Monthly",
                        "bower",                    "Bower",        "2024-12-01",    "Restoration Fee",         "$0",       "Monthly",
                        "bower",                    "Bower",        "2024-12-01",   "Security Deposit",       "$500",       "Monthly",
                        "bower",                    "Bower",        "2024-12-01",            "Pet Fee",         "$0",       "Monthly",
                        "bower",                    "Bower",        "2024-12-01",        "Pet Deposit",         "$0",       "Monthly",
                        "bower",                    "Bower",        "2024-12-01",           "Pet Rent",        "$35",       "Monthly"
                   ) |>
  dplyr::mutate(
    leasing_week_start = as.Date(leasing_week_start),
    fees_amount = stringr::str_replace_all(fees_amount, "[^0-9]", "") |>
      as.numeric()
  )

dplyr::glimpse(fees_data_new)

fee_structures <- tibble::tribble(
                    ~property_id,             ~property_name, ~leasing_week_start,     ~fee_structure,
                        "739085", "1047 Commonwealth Avenue",        "2024-12-01", "Both Fees Waived",
                      "boylston",            "1330 Boylston",        "2024-12-01", "Both Fees Waived",
                       "vanness",                 "Van Ness",        "2024-12-01", "Both Fees Waived",
                         "bower",                    "Bower",        "2024-12-01", "Both Fees Waived"
                    ) |>
  dplyr::mutate(
    leasing_week_start = as.Date(leasing_week_start)
  )

dplyr::glimpse(fee_structures)

property_amenities_data_ <- commonwealth_market_survey_data$amenities$property_amenitites_wide |>
  dplyr::bind_rows(boylston_market_survey_data$amenities$property_amenitites_wide) |>
  dplyr::bind_rows(bower_market_survey_data$amenities$property_amenitites_wide) |>
  dplyr::bind_rows(van_ness_market_survey_data$amenities$property_amenitites_wide) |>
  janitor::clean_names()

property_amenities_data <- property_amenities_data_ |>
  dplyr::mutate_all(dplyr::na_if, "n/a") |>
  dplyr::mutate(
    property_id = tolower(property_id),
    leasing_week_start = as.Date(leasing_week)
  ) |>
  dplyr::mutate_at(
    dplyr::vars(university_shuttle:dog_park),
    ~ ifelse(.x == "Yes", TRUE, FALSE)
  ) |>
  dplyr::select(
    property_id,
    property_name,
    leasing_week_start,
    dplyr::everything()
  ) |>
  dplyr::select(
    -leasing_week
  ) |>
  tidyr::pivot_longer(
    cols = university_shuttle:dog_park,
    names_to = "amenity",
    values_to = "availability"
  ) |>
  dplyr::select(
    property_id,
    property_name,
    amenity,
    availability
  ) |>
  dplyr::distinct()

dplyr::glimpse(property_amenities_data)


unit_amenities_data <- commonwealth_market_survey_data$amenities$unit_amenities_wide |>
  dplyr::bind_rows(boylston_market_survey_data$amenities$unit_amenities_wide) |>
  dplyr::bind_rows(bower_market_survey_data$amenities$unit_amenities_wide) |>
  dplyr::bind_rows(van_ness_market_survey_data$amenities$unit_amenities_wide) |>
  janitor::clean_names() |>
  dplyr::mutate_all(dplyr::na_if, "n/a") |>
  dplyr::mutate(
    property_id = tolower(property_id),
    unit_levels = 1,
    tv_included_in_rent = c("No", "No", "No", "No"),
    tv_renatable_rate = 0,
    tv_bedroom = 0,
    tv_common_area = 0,
    furniture_included_in_rent = c("Yes", "No", "No", "No"),
    furniture_rentable_rate = 0,
    floor_premium = 0,
    poolside_premium = 0,
    top_floor_premium = 0,
    view_premium = 0,
    other_premiums = 0
  ) |>
  dplyr::select(
    property_id,
    property_name,
    everything()
  ) |>
  dplyr::select(
    -leasing_week,
    -common_area,
    -furniture_renatable_rate,
    -rentable_rate_if_not_included,
    -other
  )

dplyr::glimpse(unit_amenities_data)

utilities_1_data <- commonwealth_market_survey_data$utilities$utilities_summary_1 |>
  dplyr::bind_rows(boylston_market_survey_data$utilities$utilities_summary_1) |>
  dplyr::bind_rows(bower_market_survey_data$utilities$utilities_summary_1) |>
  dplyr::bind_rows(van_ness_market_survey_data$utilities$utilities_summary_1) |>
  janitor::clean_names() |>
  dplyr::transmute(
    property_id = tolower(property_id),
    property_name = property_name,
    utility_name = utility,
    utility_amount = amount,
    utility_category = "main",
    per = per_unit_bed,
    included = ifelse(all_inclusive == "No", FALSE, TRUE),
    capped = ifelse(cap == "No", FALSE, TRUE),
    allowance = ifelse(allowance == "No", FALSE, TRUE)
  )

dplyr::glimpse(utilities_1_data)

utilities_2_data <- commonwealth_market_survey_data$utilities$utilities_summary_2 |>
  dplyr::bind_rows(boylston_market_survey_data$utilities$utilities_summary_2) |>
  dplyr::bind_rows(bower_market_survey_data$utilities$utilities_summary_2) |>
  dplyr::bind_rows(van_ness_market_survey_data$utilities$utilities_summary_2) |>
  janitor::clean_names() |>
  dplyr::transmute(
    property_id = tolower(property_id),
    property_name = property_name,
    utility_name = utility,
    utility_amount = amount,
    utility_category = "other",
    per = per_unit_bed,
    included = ifelse(included == "No", FALSE, TRUE),
    capped = FALSE,
    allowance = FALSE
  )

dplyr::glimpse(utilities_2_data)

utilities_data <- utilities_1_data |>
  dplyr::bind_rows(utilities_2_data) |>
  dplyr::arrange(property_id, utility_category, utility_name)

dplyr::glimpse(utilities_data)

notes_data_leasing <- commonwealth_market_survey_data$notes$notes_leasing_special |>
  dplyr::bind_rows(boylston_market_survey_data$notes$notes_leasing_special) |>
  dplyr::bind_rows(bower_market_survey_data$notes$notes_leasing_special) |>
  dplyr::bind_rows(van_ness_market_survey_data$notes$notes_leasing_special) |>
  janitor::clean_names() |>
  dplyr::transmute(
    property_id = tolower(property_id),
    property_name = property_name,
    leasing_week_start = as.Date(leasing_week),
    notes_leasing_special = notes
  )

dplyr::glimpse(notes_data_leasing)

notes_data_property <- commonwealth_market_survey_data$notes$notes_property_notes |>
  dplyr::bind_rows(boylston_market_survey_data$notes$notes_property_notes) |>
  dplyr::bind_rows(bower_market_survey_data$notes$notes_property_notes) |>
  dplyr::bind_rows(van_ness_market_survey_data$notes$notes_property_notes) |>
  janitor::clean_names() |>
  dplyr::transmute(
    property_id = tolower(property_id),
    property_name = property_name,
    leasing_week_start = as.Date(leasing_week),
    notes_property_operational_changes = notes
  )

dplyr::glimpse(notes_data_property)

notes_data <- notes_data_leasing |>
  dplyr::full_join(notes_data_property, by = c("property_id", "property_name", "leasing_week_start"))

dplyr::glimpse(notes_data)

office_hours_data <- commonwealth_market_survey_data$office_hours$office_hours |>
  dplyr::bind_rows(boylston_market_survey_data$office_hours$office_hours) |>
  dplyr::bind_rows(bower_market_survey_data$office_hours$office_hours) |>
  dplyr::bind_rows(van_ness_market_survey_data$office_hours$office_hours) |>
  janitor::clean_names() |>
  dplyr::transmute(
    property_id = tolower(property_id),
    property_name = property_name,
    day_of_week = day,
    hours = hours
  )

dplyr::glimpse(office_hours_data)

rents_floorplan_data <- commonwealth_market_survey_data$rents$rents_by_floorplan |>
  dplyr::bind_rows(boylston_market_survey_data$rents$rents_by_floorplan) |>
  dplyr::bind_rows(bower_market_survey_data$rents$rents_by_floorplan) |>
  dplyr::bind_rows(van_ness_market_survey_data$rents$rents_by_floorplan) |>
  janitor::clean_names() |>
  dplyr::transmute(
    property_id = tolower(property_id),
    property_name = property_name,
    leasing_week_start = as.Date(leasing_week),
    floorplan_type = floorplan_type,
    floorplan_description = description,
    total_beds = count,
    sf_per_bed = sf_per_bed,
    number_of_beds = bed,
    number_of_baths = bath,
    available = ifelse(available == "Yes", TRUE, FALSE),
    market_rent_per_bed = market_rent_per_bed,
    market_rent_per_sf = market_rent_per_sf,
    concessions_gift_card = concessions_gift_card,
    concessions_one_time_rent = concessions_one_time_rent,
    concessions_monthly_rent = concessions_monthly_rent,
    effective_rent_per_bed = market_rent_per_bed - (concessions_gift_card / 12) - (concessions_one_time_rent / 12) - concessions_monthly_rent,
    effective_rent_per_sf = market_rent_per_sf - (concessions_gift_card / 12) - (concessions_one_time_rent / 12) - concessions_monthly_rent,
    additional_monthly_expenses_furniture = 0,
    additional_monthly_expenses_tv = 0,
    additional_monthly_expenses_electricity_gas = 0,
    additional_monthly_expenses_water = 0,
    additional_monthly_expenses_cable_internet = 0,
    additional_monthly_expenses_trash = 0,
    additional_monthly_expenses_parking = 0,
    additional_monthly_expenses_total = additional_monthly_expenses_furniture + additional_monthly_expenses_tv + additional_monthly_expenses_electricity_gas + additional_monthly_expenses_water + additional_monthly_expenses_cable_internet + additional_monthly_expenses_trash + additional_monthly_expenses_parking,
    bundled_rent_per_bed = effective_rent_per_bed + additional_monthly_expenses_total,
    bundled_rent_per_sf = bundled_rent_per_bed / sf_per_bed
  )

dplyr::glimpse(rents_floorplan_data)

rents_floorplan_data_totals <- rents_floorplan_data |>
  dplyr::mutate(
    sf_per_bed_times_total_beds = sf_per_bed * total_beds
  ) |>
  dplyr::group_by(
    property_name
  ) |>
  dplyr::summarize(
    property_total_beds = sum(total_beds),
    property_sf_per_bed = sum(sf_per_bed_times_total_beds) / sum(total_beds),
    available = TRUE,
    market_rent_per_bed = sum(market_rent_per_bed * total_beds) / sum(total_beds),
    market_rent_per_sf = sum(market_rent_per_sf * total_beds) / sum(total_beds),
    concessions_gift_card = sum(concessions_gift_card),
    concessions_one_time_rent = sum(concessions_one_time_rent),
    concessions_monthly_rent = sum(concessions_monthly_rent),
    effective_rent_per_bed = sum(effective_rent_per_bed * total_beds) / sum(total_beds),
    effective_rent_per_sf = sum(effective_rent_per_sf * total_beds) / sum(total_beds),
    additional_monthly_expenses_furniture = sum(additional_monthly_expenses_furniture),
    additional_monthly_expenses_tv = sum(additional_monthly_expenses_tv),
    additional_monthly_expenses_electricity_gas = sum(additional_monthly_expenses_electricity_gas),
    additional_monthly_expenses_water = sum(additional_monthly_expenses_water),
    additional_monthly_expenses_cable_internet = sum(additional_monthly_expenses_cable_internet),
    additional_monthly_expenses_trash = sum(additional_monthly_expenses_trash),
    additional_monthly_expenses_parking = sum(additional_monthly_expenses_parking),
    additional_monthly_expenses_total = sum(additional_monthly_expenses_total),
    bundled_rent_per_bed = sum(bundled_rent_per_bed * total_beds) / sum(total_beds),
    bundled_rent_per_sf = sum(bundled_rent_per_sf * total_beds) / sum(total_beds)
  ) |>
  dplyr::ungroup()

unit_types <- c("Studio",
                "1 Bedroom",
                "2 Bedroom",
                "3 Bedroom",
                "4 Bedroom",
                "5 Bedroom",
                "6 Bedroom")

average_rents_by_unit_type_skeleton <- expand.grid(
  property_id = unique(rents_floorplan_data$property_id),
  unit_type = unit_types
) |>
  dplyr::left_join(
    tibble::tibble(
      property_id = unique(rents_floorplan_data$property_id),
      property_name = unique(rents_floorplan_data$property_name),
      leasing_week_start = unique(rents_floorplan_data$leasing_week_start)
    ),
    by = "property_id"
  ) |>
  dplyr::arrange(
    property_id,
    unit_type
  )

average_rents_by_unit_type_data <- rents_floorplan_data |>
  dplyr::mutate(
    sf_per_bed_times_total_beds = sf_per_bed * total_beds
  ) |>
  dplyr::group_by(
    property_name,
    floorplan_type
  ) |>
  dplyr::summarize(
    property_floorplan_total_beds = sum(total_beds),
    property_floorplan_sf_per_bed = sum(sf_per_bed_times_total_beds) / sum(total_beds),
    available = TRUE,
    property_floorplan_market_rent_per_bed = sum(market_rent_per_bed * total_beds) / sum(total_beds),
    property_floorplan_market_rent_per_sf = sum(market_rent_per_sf * total_beds) / sum(total_beds),
    concessions_gift_card = sum(concessions_gift_card),
    concessions_one_time_rent = sum(concessions_one_time_rent),
    concessions_monthly_rent = sum(concessions_monthly_rent),
    property_floorplan_effective_rent_per_bed = sum(effective_rent_per_bed * total_beds) / sum(total_beds),
    property_floorplan_effective_rent_per_sf = sum(effective_rent_per_sf * total_beds) / sum(total_beds),
    additional_monthly_expenses_furniture = sum(additional_monthly_expenses_furniture),
    additional_monthly_expenses_tv = sum(additional_monthly_expenses_tv),
    additional_monthly_expenses_electricity_gas = sum(additional_monthly_expenses_electricity_gas),
    additional_monthly_expenses_water = sum(additional_monthly_expenses_water),
    additional_monthly_expenses_cable_internet = sum(additional_monthly_expenses_cable_internet),
    additional_monthly_expenses_trash = sum(additional_monthly_expenses_trash),
    additional_monthly_expenses_parking = sum(additional_monthly_expenses_parking),
    additional_monthly_expenses_total = sum(additional_monthly_expenses_total),
    property_floorplan_bundled_rent_per_bed = sum(bundled_rent_per_bed * total_beds) / sum(total_beds),
    property_floorplan_bundled_rent_per_sf = sum(bundled_rent_per_sf * total_beds) / sum(total_beds)
  ) |>
  dplyr::ungroup() |>
  dplyr::right_join(
    average_rents_by_unit_type_skeleton,
    by = c(
      "property_name" = "property_name",
      "floorplan_type" = "unit_type"
    )
  ) |>
  dplyr::arrange(
    property_name,
    floorplan_type
  ) |>
  dplyr::mutate_if(is.numeric, ~ ifelse(is.na(.x), 0, .x)) |>
  dplyr::mutate(
    available = ifelse(is.na(available), FALSE, TRUE)
  ) |>
  dplyr::select(
    property_id,
    property_name,
    leasing_week_start,
    floorplan_type,
    dplyr::everything()
  )

dplyr::glimpse(average_rents_by_unit_type_data)


market_survey_master_data_lst <- list(
  property_summary_data = property_summary_data,
  leasing_summary_data = leasing_summary_data,
  short_term_leases_data = short_term_leases_data,
  fees_data = fees_data_new,
  fee_structures = fee_structures,
  property_amenities_data = property_amenities_data,
  unit_amenities_data = unit_amenities_data,
  utilities_data = utilities_data,
  notes_data = notes_data,
  office_hours_data = office_hours_data,
  rents_floorplan_data = rents_floorplan_data,
  rents_floorplan_data_totals = rents_floorplan_data_totals,
  average_rents_by_unit_type_data = average_rents_by_unit_type_data
)

cache_file <- "data-raw/cache/market_survey_master_data_lst.qs"

qs2::qs_save(
  object = market_survey_master_data_lst,
  file = cache_file
)

cli::cli_alert_success(
  c(
    "Market survey data preparation successful.",
    "Data saved to: {.path {cache_file}}"
  )
)

csv_path <- "data-raw/data/working/market_survey"
csv_files <- purrr::map_chr(
  names(market_survey_master_data_lst),
  ~ file.path(csv_path, paste0(.x, ".csv"))
)

purrr::walk2(
  market_survey_master_data_lst,
  csv_files,
  ~ readr::write_csv(.x, .y)
)

cli::cli_alert_success(
  c(
    "Market survey data saved to CSV files.",
    "Files saved to: {.path {csv_path}}"
  )
)
