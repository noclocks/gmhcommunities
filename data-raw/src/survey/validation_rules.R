
library(validate)

property_summary_validation_tbl <- property_summary_fields_for_db |>
  dplyr::select(
    section_id,
    order,
    name = field_name,
    data_type,
    input_type,
    required,
    choices,
    validation,
    is_disabled
  ) |>
  dplyr::mutate(
    section_id = "property_summary",
    section_name = "Property Summary",
    validation = paste0("shinyvalidate::sv_required(message = '", name, " is required.')")
  )

phone_regex <- "^(?:\\(?[0-9]{3}\\)?[-. ]?[0-9]{3}[-. ]?[0-9]{4})$"

address_regex <- paste0(
  "\\b\\d+\\s+",                    # Street number
  "[A-Za-z\\s]+",                   # Street name
  "(?:Street|St|Avenue|Ave|Road|Rd|Boulevard|Blvd)",  # Street type
  "(?:\\s+(?:Apartment|Apt|Unit|#)\\s+\\d+)?",       # Optional apartment/unit
  ",\\s*",                          # Comma separator
  "[A-Za-z\\s]+",                   # City
  ",\\s*",                          # Comma separator
  "[A-Z]{2}",                       # State (2 letter code)
  ",?\\s*",                         # Optional comma
  "\\d{5}(?:-\\d{4})?"              # ZIP code (5 digits, optional +4)
)


property_competitors <- list(
  "739085" = c("bower", "boylston", "vanness")
)

property_ids <- c(
  "739085",
  "bower",
  "boylston",
  "vanness"
)

property_names <- c(
  "1047 Commonwealth Avenue",
  "1047 Commmonwealth",
  "1330 Boylston",
  "Van Ness",
  "Bower"
)

property_types <- c(
  "Student",
  "Conventional",
  "Affordable",
  "Innovative"
)

property_statuses = c(
  "Operational",
  "New Construction",
  "Undergoing Renovations"
)

product_types = c(
  "High-Rise",
  "Mid-Rise",
  "Wrap",
  "Garden",
  "Cottage",
  "SFR"
)

comp_statuses = c(
  "Subject Property",
  "Tier 1",
  "Tier 2"
)

property_ratings <- seq(0.5, 5, by = 0.5)

property_summary_validators <- list(
  property_name = "shinyvalidate::compose_rules(shinyvalidate::sv_required(message = \"Property Name is required.\")",
  property_website = "shinyvalidate::compose_rules(shinyvalidate::sv_optional(), shinyvalidate::sv_url(message = \"Please enter a valid URL for the property website.\"))",
  property_address = "shinyvalidate::compose_rules(shinyvalidate::sv_required(message = \"Property Address is required.\"), shinyvalidate::sv_regex(pattern = address_regex, message = \"Enter a valid address.\"))",
  property_phone_number = "shinyvalidate::compose_rules(shinyvalidate::sv_optional(), shinyvalidate::sv_regex(pattern = phone_regex, message = \"Enter a valid phone number (i.e. 123-456-7890).\"))",
  property_type = "shinyvalidate::compose_rules(shinyvalidate::sv_in_set(property_types, message = \"Please select a valid property type.\"), shinyvalidate::sv_required(message = \"Property Type is required.\"))",
  property_status = "shinyvalidate::compose_rules(shinyvalidate::sv_in_set(property_statuses, message = \"Please select a valid property status.\"), shinyvalidate::sv_required(message = \"Property Status is required.\"))",
  product_type = "shinyvalidate::compose_rules(shinyvalidate::sv_in_set(product_types, message = \"Please select a valid product type.\"), shinyvalidate::sv_required(message = \"Product Type is required.\"))",
  property_rating = "shinyvalidate::compose_rules(shinyvalidate::sv_optional(), shinyvalidate::sv_in_set(property_ratings, message = \"Please select a valid property rating (0.5 to 5.0).\"))",
  comp_status = "shinyvalidate::compose_rules(shinyvalidate::sv_in_set(comp_statuses, message = \"Please select a valid competitor status.\"), shinyvalidate::sv_required(message = \"Competitor Status is required.\"))",
  year_built = "shinyvalidate::compose_rules(shinyvalidate::sv_optional(), shinyvalidate::sv_integer(message = \"Year Built must be an integer.\"), shinyvalidate::sv_gt(1970, message = \"Year Built must be greater than 1970.\"), shinyvalidate::sv_lt(2025, message = \"Year Built must be less than 2025.\"))",
  distance_to_campus = "shinyvalidate::compose_rules(shinyvalidate::sv_optional(), shinyvalidate::sv_numeric(message = \"Distance must be a number.\"), shinyvalidate::sv_between(0, 100, message = \"Distance must be between 0 and 100 miles.\"))"
)

yaml::as.yaml(property_summary_validators) |> cat(sep = "\n")




ps_validation_rules <- yaml::read_yaml(file = "inst/extdata/survey/validation/property_summary.validation.yml", eval.expr = TRUE)

ps_validation_rules$variables |>
  purrr::map(
    function(x) {
      name <- names(x)[1]
      assign(
        name,
        eval(parse(text = x[[name]]))
      )
    }
  )


property_summary_validation_rules <- list(
  property_summary = list(
    property_name = list(
      type = "required",
      message = "Property Name is required."
    ),
    property_website = list(
      type = "url",
      required = TRUE,
      message = "Please enter a valid URL for the property website."
    ),
    property_phone_number = list(
      type = "regex",
      pattern = "^(?:\\(?[0-9]{3}\\)?[-. ]?[0-9]{3}[-. ]?[0-9]{4})$",
      required = TRUE,
      message = "Enter a valid phone number."
    ),
    property_type = list(
      type = "set",
      required = TRUE,
      options = c("Student", "Conventional", "Affordable", "Innovative"),
      message = "Please select a valid property type."
    ),
    property_status = list(
      type = "set",
      required = TRUE,
      options = c("Operational", "New Construction", "Undergoing Renovations"),
      message = "Please select a valid property status."
    ),
    property_rating = list(
      type = "numeric",
      required = TRUE,
      min = 1,
      step = 0.5,
      max = 5,
      message = "Rating must be between 1 and 5."
    ),
    distance_to_campus = list(
      type = "numeric",
      required = TRUE,
      min = 0,
      max = 100,
      message = "Distance must be between 0 and 100 miles."
    )
  )
)

create_validation_rule <- function(field, rule) {
  switch(
    rule$type,
    "required" = shinyvalidate::sv_required(message = rule$message),
    "url" = shinyvalidate::sv_url(message = rule$message),
    "regex" = shinyvalidate::sv_regex(pattern = rule$pattern, message = rule$message),
    "numeric" = shinyvalidate::compose_rules(
      shinyvalidate::sv_required(),
      shinyvalidate::sv_between(rule$min, rule$max, message = rule$message)
    ),
    "set" = shinyvalidate::sv_in_set(rule$options, message = rule$message)
  )
}
