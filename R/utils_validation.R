
#  ------------------------------------------------------------------------
#
# Title : Validation Utilities
#    By : Jimmy Briggs
#  Date : 2024-12-12
#
#  ------------------------------------------------------------------------


validate_phone_regex <- function(phone) {
  phone_regex <- "^(?:\\(?[0-9]{3}\\)?[-. ]?[0-9]{3}[-. ]?[0-9]{4})$"
  grepl(phone_regex, phone)
}

validate_address_regex <- function(address) {
  address_regex <- "^[0-9]+\\s+([a-zA-Z]+|[a-zA-Z]+\\s[a-zA-Z]+)\\s[a-zA-Z]+"
  grepl(address_regex, address)
}

validate_address_geocode <- function(address) {
  geocode <- tryCatch({
    geocode_address(address)
  }, error = function(e) {
    NULL
  })

  if (is.null(geocode)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}


load_validation_rules <- function() {
  yaml::read_yaml(pkg_sys("extdata/validation.yml"))
}

apply_validation_rules <- function(iv, survey_section = NULL, rules = get_section_rules(survey_section)) {


}

initialize_property_summary_validator <- function() {

  if (!exists("property_summary_inputs")) {
    property_summary_inputs <- readr::read_csv(
      system.file(
        "extdata/survey/property_summary_inputs.csv",
        package = "gmhcommunities"
      )
    )
  }

  property_summary_input_ids <- property_summary_inputs$id
  property_summary_required_input_ids <- property_summary_inputs |>
    dplyr::filter(required == TRUE) |>
    dplyr::pull(id)
  property_summary_required_input_validation_messages <- property_summary_inputs |>
    dplyr::filter(required == TRUE) |>
    dplyr::pull(name) |>
    purrr::map_chr(~ paste0(.x, " is required."))

  iv <- shinyvalidate::InputValidator$new()

  # add all required inputs
  purrr::walk2(
    property_summary_required_input_ids,
    property_summary_required_input_validation_messages,
    ~ iv$add_rule(.x, shinyvalidate::sv_required(message = .y))
  )

  # add regex rules for phone and website
  phone_regex <- "^(?:\\(?[0-9]{3}\\)?[-. ]?[0-9]{3}[-. ]?[0-9]{4})$"

  iv$add_rule("property_website", shinyvalidate::sv_url(message = "Please enter a valid URL for the property website."))
  iv$add_rule("property_phone_number", shinyvalidate::sv_regex(phone_regex, "Enter a valid phone number."))

  # add rules for distance to campus and property rating
  iv$add_rule("distance_to_campus", shinyvalidate::sv_gt(0, "Distance must be greater than 0."))
  iv$add_rule("property_rating", shinyvalidate::sv_gt(0, "Rating must be greater than 0."))

  # return
  return(iv)
}
