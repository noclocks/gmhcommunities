property_amenity_names <- c(
  "University Shuttle",
  "Private Shuttle",
  "Limited Access Gates",
  "Fitness Center",
  "Computer Lounge",
  "Game Room",
  "Spray Tanning",
  "UV Tanning",
  "Pool",
  "Hot Tub",
  "24hr Package System",
  "EV Charging Stations",
  "Car Sharing Services",
  "Smart Vending",
  "Mini Market",
  "Movie Theatre",
  "Co-Working/Study Spaces",
  "Free Printing",
  "Coffee Bar",
  "Retail",
  "Sauna/Spa",
  "Cycling/Yoga Studio",
  "Rentable Guest Suite",
  "Wellness Classes",
  "24hr Concierge",
  "Outdoor Grill Area",
  "Sand Volleyball Court",
  "Basketball Court",
  "Pets Allowed",
  "Dog Wash",
  "Dog Park"
)

property_amenity_values <- c(
  FALSE,
  FALSE,
  FALSE,
  TRUE,
  TRUE,
  TRUE,
  FALSE,
  FALSE,
  FALSE,
  FALSE,
  TRUE,
  FALSE,
  FALSE,
  TRUE,
  FALSE,
  FALSE,
  TRUE,
  TRUE,
  TRUE,
  TRUE,
  FALSE,
  FALSE,
  FALSE,
  FALSE,
  FALSE,
  TRUE,
  FALSE,
  FALSE,
  FALSE,
  FALSE,
  FALSE
)


property_amenities_data <- tibble::tibble(
  Amenity = property_amenity_names,
  Available = property_amenity_values
)


# property_amenities <- tibble::tribble(
#   ~property_id,      ~property_name, ~common_area_rating, ~university_shuttle, ~private_shuttle, ~limited_access_gates, ~fitness_center, ~computer_lounge, ~game_room, ~spray_tanning, ~uv_tanning, ~pool, ~hot_tub, ~x24hr_package_system, ~ev_charging_stations, ~car_sharing_services, ~smart_vending, ~mini_market, ~movie_theatre, ~co_working_study_spaces, ~free_printing, ~coffee_bar, ~retail, ~sauna_spa, ~cycling_yoga_studio, ~rentable_guest_suite, ~wellness_classes, ~x24hr_concierge, ~outdoor_grill_area, ~sand_volleyball_court, ~basketball_court, ~pets_allowed, ~dog_wash, ~dog_park,
#        "739085", "1047 Commonwealth",                  3L,               FALSE,            FALSE,                 FALSE,            TRUE,             TRUE,       TRUE,          FALSE,       FALSE, FALSE,    FALSE,                  TRUE,                 FALSE,                 FALSE,           TRUE,        FALSE,          FALSE,                     TRUE,           TRUE,        TRUE,    TRUE,      FALSE,                FALSE,                 FALSE,             FALSE,            FALSE,                TRUE,                  FALSE,             FALSE,         FALSE,     FALSE,     FALSE
#   )
#
#
#
#
# "Common Area Rating",
# "3",
#
#
#
#
#
# property_amenities_df <- property_amenities |>
#   tidyr::pivot_wider(names_from = key, values_from = value) |>
#   dplyr::mutate_at(
#     .vars = dplyr::vars(starts_with("No")),
#     .funs = ~ "No"
#   ) |>
