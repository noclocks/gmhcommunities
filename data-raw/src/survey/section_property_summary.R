# excel data --------------------------------------------------------------

market_survey_property_summary_data <- tibble::tibble(
  entrata_property_id = c(
    "739085",
    NA_character_,
    NA_character_,
    NA_character_
  ),
  entrata_property_name = c(
    "1047 Commonwealth Avenue",
    NA_character_,
    NA_character_,
    NA_character_
  ),
  property_name = c(
    "1047 Commonwealth",
    "1330 Boylston",
    "Van Ness",
    "Bower"
  ),
  website = c(
    "https://www.1047commonwealth.com/",
    "https://1330boylston.com/",
    "https://www.thevanness.com/",
    "https://bowerboston.com/"
  ),
  address = c(
    "1047 Commonwealth Ave, Boston, MA, 02215",
    "1330 Boylston St, Boston, MA, 02215",
    "1335 Boylston St, Boston, MA, 02215",
    "771 Beacon St Apartment 775, Boston, MA, 02215"
  ),
  phone_number = c(
    "(617) 500-6481",
    "(617) 267-1330",
    "(617) 424-1335",
    "(617) 341-9700"
  ),
  developer = c(
    "BPDA",
    "Samuels and Associates",
    "Samuels And Associates",
    "The Green Cities Company"
  ),
  manager = c(
    "GMH Communities",
    "Samuels and Associates",
    "Samuels And Associates",
    "Greystar"
  ),
  owner = c(
    "AGC + GMH Communities",
    "Samuels and Associates",
    "Samuels And Associates",
    "Greystar"
  ),
  property_type = c(
    "Student",
    "Conventional",
    "Conventional",
    "Conventional"
  ),
  property_status = c("Operational", "Operational", "Operational", "Operational"),
  product_type = c("Mid-Rise", "High-Rise", "High-Rise", "High-Rise"),
  property_rating = c(2L, 5L, 4L, 5L),
  comp_status = c("Subject Property", "Tier 2", "Tier 2", "Tier 2"),
  year_built_or_renovated = c(2017L, 2008L, 2015L, 2020L),
  date_of_most_recent_sale = as.Date(c("2019-01-01", NA_character_, NA_character_, NA_character_)),
  distance_from_campus = c(0.1, 1, 1, 0.2)
)





# choices -----------------------------------------------------------------

market_survey_property_summary_input_choices <- list(
  property = c(
    "1047 Commonwealth",
    "1330 Boylston",
    "Van Ness",
    "Bower"
  ),
  property_type = c(
    "Student",
    "Conventional",
    "Affordable",
    "Innovative"
  ),
  property_status = c(
    "Operational",
    "New Construction",
    "Undergoing Renovations"
  ),
  product_type = c(
    "High-Rise",
    "Mid-Rise",
    "Wrap",
    "Garden",
    "Cottage",
    "SFR"
  ),
  comp_status = c(
    "Subject Property",
    "Tier 1",
    "Tier 2"
  )
)

# input data --------------------------------------------------------------

market_survey_property_summary_input_names <- c(
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
  "Year Built/Renovated",
  "Most Recent Sale Date",
  "Distance to Campus"
)

market_survey_property_summary_input_icons <- c(
  "building",
  "globe",
  "map-marker-alt",
  "phone",
  "hard-hat",
  "user-tie",
  "user",
  "check",
  "building",
  "star",
  "check-circle",
  "calendar-alt",
  "dollar-sign",
  "ruler-horizontal",
  "map-marker-alt"
)

market_survey_property_summary_input_help <- c(
  "Enter the name of the property.",
  "Enter the website of the property.",
  "Enter the address of the property.",
  "Enter the phone number of the property.",
  "Enter the developer of the property.",
  "Enter the manager of the property.",
  "Enter the owner of the property.",
  "Select the type of property.",
  "Select the status of the property.",
  "Select the product type of the property.",
  "Enter the rating of the property.",
  "Select the comp status of the property.",
  "Enter the year the property was built.",
  "Enter the most recent sale date of the property.",
  "Enter the distance from campus in miles."
)

market_survey_property_summary_input_types <- c(
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
  "date",    # Year Built
  "date",    # Most Recent Sale Date
  "numeric"  # Distance to Campus
)

property_summary_inputs <- tibble::tibble(
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
    "Year Built/Renovated",
    "Most Recent Sale Date",
    "Distance to Campus"
  )
)


property_summary_inputs <- property_summary_inputs |>
  dplyr::mutate(
    id = snakecase::to_snake_case(name),
    order = 1:nrow(property_summary_inputs),
    label = name,
    placeholder = paste("Enter", name),
    icon = c(
      "building",
      "globe",
      "map-marker-alt",
      "phone",
      "hard-hat",
      "user-tie",
      "user",
      "check",
      "building",
      "star",
      "check-circle",
      "calendar-alt",
      "dollar-sign",
      "ruler-horizontal",
      "map-marker-alt"
    ),
    icon_function = rep("shiny::icon", nrow(property_summary_inputs)),
    help = c(
      "Enter the name of the property.",
      "Enter the website of the property.",
      "Enter the address of the property.",
      "Enter the phone number of the property.",
      "Enter the developer of the property.",
      "Enter the manager of the property.",
      "Enter the owner of the property.",
      "Select the type of property.",
      "Select the status of the property.",
      "Select the product type of the property.",
      "Enter the rating of the property.",
      "Select the comp status of the property.",
      "Enter the year the property was built.",
      "Enter the most recent sale date of the property.",
      "Enter the distance from campus in miles."
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
      "date",    # Year Built
      "date",    # Most Recent Sale Date
      "numeric"  # Distance to Campus
    ),
    input_function = c(
      "shinyWidgets::textInputIcon",
      "shinyWidgets::textInputIcon",
      "shinyWidgets::textInputIcon",
      "shinyWidgets::textInputIcon",
      "shinyWidgets::textInputIcon",
      "shinyWidgets::textInputIcon",
      "shinyWidgets::textInputIcon",
      "shinyWidgets::radioGroupButtons",
      "shinyWidgets::radioGroupButtons",
      "shinyWidgets::radioGroupButtons",
      "shinyWidgets::noUiSliderInput",
      "shinyWidgets::radioGroupButtons",
      "shinyWidgets::airYearpickerInput",
      "shinyWidgets::airDatepickerInput",
      "shinyWidgets::numericInputIcon"
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
      FALSE, # Most Recent Sale Date
      TRUE   # Distance to Campus
    ),
    choices = c(
      list(NULL), # Property Name
      list(NULL), # Property Website
      list(NULL), # Property Address
      list(NULL), # Property Phone Number
      list(NULL), # Property Developer
      list(NULL), # Property Manager
      list(NULL), # Property Owner
      'c("Student", "Conventional", "Affordable", "Innovative")',
      'c("Operational", "New Construction", "Undergoing Renovations")',
      'c("High-Rise", "Mid-Rise", "Wrap", "Garden", "Cottage", "SFR")',
      list(NULL), # Property Rating
      'c("Subject Property", "Tier 1", "Tier 2")',
      list(NULL), # Year Built
      list(NULL), # Most Recent Sale Date
      list(NULL)  # Distance to Campus
    ),
    default = c(
      NA, # Property Name
      NA, # Property Website
      NA, # Property Address
      NA, # Property Phone Number
      NA, # Property Developer
      "GMH Communities", # Property Manager
      "GMH Communities", # Property Owner
      "Student", # Property Type
      "Operational", # Property Status
      "Mid-Rise", # Product Type
      "3", # Property Rating
      "Subject Property", # Comp Status
      NA, # Year Built
      NA, # Most Recent Sale Date
      NA  # Distance to Campus
    ),
    prepopulate = c(
      TRUE,  # Property Name
      TRUE,  # Property Website
      TRUE,  # Property Address
      TRUE,  # Property Phone Number
      TRUE,  # Property Developer
      TRUE,  # Property Manager
      TRUE,  # Property Owner
      TRUE,  # Property Type
      TRUE,  # Property Status
      TRUE,  # Product Type
      TRUE,  # Property Rating
      TRUE,  # Comp Status
      TRUE,  # Year Built
      FALSE, # Most Recent Sale Date
      TRUE   # Distance to Campus
    ),
    prepopulate_method = c(
      "prior", # Property Name
      "prior", # Property Website
      "prior", # Property Address
      "prior", # Property Phone Number
      "prior", # Property Developer
      "prior", # Property Manager
      "prior", # Property Owner
      "prior", # Property Type
      "prior", # Property Status
      "prior", # Product Type
      "prior", # Property Rating
      "prior", # Comp Status
      "prior", # Year Built
      "current", # Most Recent Sale Date
      "prior"  # Distance to Campus
    )
  )

readr::write_csv(property_summary_inputs, "data-raw/data/working/market_survey/property_summary_inputs.csv")


