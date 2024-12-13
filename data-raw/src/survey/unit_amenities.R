unit_amenities <- tibble::tibble(
  property_id = c("739085"),
  property_name = c("1047 Commonwealth"),
  unit_rating = c(2),
  private_bathrooms = c(TRUE),
  walk_in_closets = c(FALSE),
  washer_dryer_in_unit = c(TRUE),
  smart_home_technology = c(FALSE),
  smart_bedroom_locks = c(FALSE),
  smart_unit_locks = c(TRUE),
  energy_efficient_appliances = c(TRUE),
  stainless_steel_appliances = c(TRUE),
  balconies = c(FALSE),
  patios = c(FALSE),
  backyards = c(FALSE),
  tv_included_with_rent = c(FALSE),
  tv_rentable_rate = c(0),
  tv_bedroom = c(0),
  tv_common_area = c(0),
  furniture_included_with_rent = c(TRUE),
  furniture_rentable_rate = c(0),
  other_premiums_floor = c(0),
  other_premiums_poolside = c(0),
  other_premiums_top_floor = c(0),
  other_premiums_view = c(0),
  other_premiums_other = c(0)
)

unit_amenities_availability <- tibble::tibble(
  Amenity = c(
    "Private Bathrooms",
    "Walk-in Closets",
    "Washer / Dryer in Unit",
    "Smart Home Technology",
    "Smart Bedroom Locks",
    "Smart Unit Locks",
    "Energy Efficient Appliances",
    "Stainless Steel Appliances",
    "Balconies",
    "Patios",
    "Backyards",
    "TV Included with Rent",
    "Furniture Included with Rent"
  ),
  Available = c(
    TRUE,
    FALSE,
    TRUE,
    FALSE,
    FALSE,
    TRUE,
    TRUE,
    TRUE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    TRUE
  )
)

unit_amenities_rates <- tibble::tibble(
  name = c(
    "TV Rentable Rate",
    "TV Bedroom",
    "TV Common Area",
    "Furniture Rentable Rate",
    "Other Premiums: Floor",
    "Other Premiums: Poolside",
    "Other Premiums: Top Floor",
    "Other Premiums: View",
    "Other Premiums: Other"
  ),
  rate = c(
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0
  )
)


unit_amenities_data_lst <- list(
  property_id = "739085",
  property_name = "1047 Commonwealth",
  unit_amenities = unit_amenities,
  unit_rating = 2,
  unit_amenities_availability = unit_amenities_availability,
  unit_amenities_rates = unit_amenities_rates
)

unit_amenities_inputs <- purrr::map2(
  unit_amenities_availability$Amenity,
  unit_amenities_availability$Available,
  function(x, y) {
    id <- snakecase::to_snake_case(x)
    value <- y
    shinyWidgets::switchInput(
      inputId = id,
      label = x,
      value = value,
      onLabel = "Yes",
      offLabel = "No",
      size = "small"
    )
  }
) |>
  setNames(unit_amenities_availability$Amenity)



# unit_rating <- tibble::tibble(
#   property_id = c("739085"),
#   property_name = c("1047 Commonwealth"),
#   unit_rating = c(2)
# )
#
# unit_amenities <- tibble::tibble(
#   amenities = c(
#     "Private Bathrooms",
#     "Walk-in Closets",
#     "Washer / Dryer in Unit",
#     "Smart Home Technology",
#     "Smart Bedroom Locks",
#     "Smart Unit Locks",
#     "Energy Efficient Appliances",
#     "Stainless Steel Appliances",
#     "Balconies",
#     "Patios",
#     "Backyards"
#   ),
#   available = c(
#     TRUE,
#     FALSE,
#     TRUE,
#     FALSE,
#     FALSE,
#     TRUE,
#     TRUE,
#     TRUE,
#     FALSE,
#     FALSE,
#     FALSE
#   )
# )


