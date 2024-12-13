



create_property_summary_inputs <- function(ns = NULL, ...) {

  if (is.null(ns)) { ns <- function(x) x }
  else { if (!rlang::is_function(ns)) cli::cli_abort("{.arg ns} must be a function") }

  num_inputs <- nrow(property_summary_inputs)

  purrr::map(c(1:num_inputs), function(i) {

    input_data <- property_summary_inputs[i, ]

    input_func_str <- input_data$input_func
    input_func <- eval(parse(text = input_func_str))

    input_id <- ns(input_data$id)

    input_label_str <- input_data$label
    input_label_icon <- input_data$icon
    input_label <- icon_text(input_label_icon, input_label_str)

    input_help_str <- input_data$help

    input_choices <- NULL
    input_choices_str <- input_data$choices
    if (!is.na(input_choices_str)) {
      input_choices <- eval(parse(text = input_choices_str))
    }




  })

}


property_summary_input_partials <- list(
  "property_name_input" = purrr::partial(
    .f = shiny::textInput,
    label = icon_text("building", "Property Name"),
    value = "",
    width = "100%",
    placeholder = "Enter the name of the property"
  ),
  "property_website_input" = purrr::partial(
    .f = shiny::textInput,
    label = icon_text("globe", "Website"),
    value = "",
    width = "100%",
    placeholder = "Enter the website of the property"
  ),
  "property_address_input" = purrr::partial(
    .f = shiny::textInput,
    label = icon_text("map-marker-alt", "Address"),
    value = "",
    width = "100%",
    placeholder = "Enter the address of the property"
  ),
  "property_phone_input" = purrr::partial(
    .f = shiny::textInput,
    label = icon_text("phone", "Phone"),
    value = "",
    width = "100%",
    placeholder = "Enter the phone number of the property"
  )
)



purrr::partial
