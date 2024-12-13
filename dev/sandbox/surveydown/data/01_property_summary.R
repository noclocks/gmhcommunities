property_summary_inputs <- list(
  property_name = list(
    type = "text",
    label = "Property Name",
    icon = "building",
    required = TRUE,
    options = NULL
  )
)



property_summary_inputs <- tibble::tibble(
  order = c(1:16),
  name = c(
    "Property Name",
    "Property Website",
    "Property Address",
    "Property Phone Number",
    "Property Developer",
    "Property Manager",
    "Property Owner",
    "Property Type",
    "Property Status",
    "Product Type",
    "Property Rating",
    "Comp Status",
    "Year Built",
    "Year Last Renovated",
    "Most Recent Sale Date",
    "Distance to Campus"
  ),
  type = c(
    "text",    # Property Name
    "text",    # Property Website
    "text",    # Property Address
    "text",    # Property Phone Number
    "text",    # Property Developer
    "text",    # Property Manager
    "text",    # Property Owner
    "mc",      # Property Type
    "mc",      # Property Status
    "mc",      # Product Type
    "numeric", # Property Rating
    "mc",      # Comp Status
    "numeric", # Year Built
    "numeric", # Year Last Renovated
    "date",    # Most Recent Sale Date
    "numeric"  # Distance to Campus
  ),
  id = c(
    "property_name",
    "property_website",
    "property_address",
    "property_phone_number",
    "property_developer",
    "property_manager",
    "property_owner",
    "property_type",
    "property_status",
    "product_type",
    "property_rating",
    "comp_status",
    "year_built",
    "year_last_renovated",
    "most_recent_sale_date",
    "distance_to_campus"
  ),
  label = c(
    "Property Name",
    "Property Website",
    "Property Address",
    "Property Phone Number",
    "Property Developer",
    "Property Manager",
    "Property Owner",
    "Property Type",
    "Property Status",
    "Product Type",
    "Property Rating",
    "Comp Status",
    "Year Built",
    "Year Last Renovated",
    "Most Recent Sale Date",
    "Distance to Campus"
  ),
  required = c(
    TRUE,  # Property Name
    FALSE, # Property Website
    TRUE,  # Property Address
    TRUE,  # Property Phone Number
    FALSE, # Property Developer
    FALSE, # Property Manager
    FALSE, # Property Owner
    TRUE,  # Property Type
    TRUE,  # Property Status
    TRUE,  # Product Type
    TRUE,  # Property Rating
    TRUE,  # Comp Status
    TRUE,  # Year Built
    FALSE, # Year Last Renovated
    FALSE, # Most Recent Sale Date
    TRUE   # Distance to Campus
  ),
  options = NULL
)

property_summary_inputs <- dplyr::mutate(
  property_summary_inputs,
  options = dplyr::case_when(
    name == "Property Type" ~ list(c(
      "Student" = "0",
      "Conventional" = "1",
      "Affordable" = "2",
      "Innovative" = "3"
    )),
    name == "Property Status" ~ list(c(
      "Operational" = "0",
      "New Construction" = "1",
      "Undergoing Renovations" = "2"
    )),
    name == "Product Type" ~ list(c(
      "High-Rise" = "0",
      "Mid-Rise" = "1",
      "Wrap" = "2",
      "Garden" = "3",
      "Cottage" = "4",
      "SFR" = "5"
    )),
    name == "Comp Status" ~ list(c(
      "Subject Property" = "0",
      "Tier 1" = "1",
      "Tier 2" = "2"
    )),
    TRUE ~ list(NULL)
  )
)

property_summary_questions <- purrr::map(
  1:nrow(property_summary_inputs),
  function(i) {
    args <- property_summary_inputs[i, ]
    surveydown::sd_question(
      id = as.character(args$id),
      type = args$type,
      label = args$label,
      option = unlist(args$options)
    )
  }
)
