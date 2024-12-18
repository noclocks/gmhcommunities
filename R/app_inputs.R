
#  ------------------------------------------------------------------------
#
# Title : Shiny App Custom Inputs
#    By : Jimmy Briggs
#  Date : 2024-12-03
#
#  ------------------------------------------------------------------------

email_input <- function(id, label = "Email", placeholder = "Enter Email Address", width = "100%") {
  htmltools::tags$input(
    type = "email",
    id = id,
    class = "form-control"
  ) |>
    htmltools::browsable()
}







survey_input_label <- function(label, tooltip_content, tooltip_icon = "info-circle", tooltip_title = "About") {

  htmltools::tagList(
    label,
    bslib::tooltip(
      bsicons::bs_icon(tooltip_icon, title = tooltip_title),
      tooltip_content
    )
  )

}

survey_input_label("Property Name", "The name of the property.")

survey_input_partials <- list(
  "property_name" = purrr::partial(
    .f = shinyWidgets::textInputIcon,
    label = survey_input_label("Property Name", "The name of the property."),
    icon = shiny::icon("building"),
    placeholder = "Enter Property Name",
    width = "100%"
  ),
  "property_website" = purrr::partial(
    .f = shinyWidgets::textInputIcon,
    label = survey_input_label("Property Website", "The website of the property."),
    icon = shiny::icon("globe"),
    placeholder = "Enter Property Website",
    width = "100%"
  ),
  "property_address" = purrr::partial(
    .f = shinyWidgets::textInputIcon,
    label = survey_input_label("Property Address", "The address of the property."),
    icon = shiny::icon("map-marker-alt"),
    placeholder = "Enter Property Address",
    width = "100%"
  ),
  "property_phone_number" = purrr::partial(
    .f = shinyWidgets::textInputIcon,
    label = survey_input_label("Phone Number", "The phone number of the property."),
    icon = shiny::icon("phone"),
    placeholder = "Enter Property Phone Number",
    width = "100%"
  ),
  "property_image" = purrr::partial(
    .f = shinyWidgets::textInputIcon,
    label = survey_input_label("Property Image", "The image of the property."),
    icon = shiny::icon("image"),
    value = NULL,
    placeholder = "www.example.com/image.jpg",
    width = "100%"
  )
)


# survey input ------------------------------------------------------------

survey_input <- function(
  id,
  ns = function(x) { x },
  ...
) {

  # filter for row for id
  input_df <- property_summary_inputs |>
    dplyr::filter(
      .data$id == .env$id
    )

  if (nrow(input_df) != 1) {
    cli::cli_abort("No input data found for id: {.arg id}.")
  }

  # get input & icon functions
  input_fn <- eval(parse(text = input_df$input_function[[1]]))
  input_icon_fn <- eval(parse(text = input_df$icon_function[[1]]))

  stopifnot(
    rlang::is_function(input_fn),
    rlang::is_function(input_icon_fn)
  )

  # get input icon
  input_icon_str <- input_df$icon[[1]]
  input_icon <- input_icon_fn(input_icon_str)

  input_fn_formals <- formalArgs(input_fn)
  input_fn_arg_names <- names(input_df)[names(input_df) %in% input_fn_formals]
  input_fn_arg_names <- input_fn_arg_names[input_fn_arg_names != "id"]
  input_fn_arg_names <- input_fn_arg_names[input_fn_arg_names != "icon"]

  # get input values
  input_args <- input_df[input_fn_arg_names] |> as.list()

  do.call(input_fn, c(list(inputId = ns(id), icon = input_icon), input_args))

}

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


survey_input <- function(
  id,
  ns = function(x) { x },
  ...
) {

  inputs_df <- market_survey_inputs |>
    dplyr::filter(
      .data$id == .env$id
    )

  if (nrow(inputs_df) == 0) {
    cli::cli_abort("No Survey Input Data for ID: {.arg {id}}.")
  }

  input_func_str <- purrr::pluck(inputs_df, "input_function", 1)
  input_icon_func_str <- purrr::pluck(inputs_df, "icon_function", 1) %||% shiny::icon
  input_icon_name <- purrr::pluck(inputs_df, "icon", 1) %||% "question"

  input_func <- eval(parse(text = input_func_str))
  icon_func <- eval(parse(text = input_icon_func_str))

  stopifnot(
    rlang::is_function(input_func),
    rlang::is_function(icon_func)
  )

  input_icon <- icon_func(input_icon_name)

  input_func_formals <- formalArgs(input_func)


  # create label
  label <- htmltools::span(
    shiny::icon(icon),
    " ",
    name
  )

  # create input
  input <- switch(
    type,
    "text" = shiny::textInput(id, label, value = preload, placeholder = help, ...),
    "number" = shiny::numericInput(id, label, value = preload, placeholder = help, ...),
    "date" = shiny::dateInput(id, label, value = preload, placeholder = help, ...),
    "select" = shiny::selectInput(id, label, choices = choices, selected = preload, ...),
    "radio" = shiny::radioButtons(id, label, choices = choices, selected = preload, ...),
    "checkbox" = shiny::checkboxGroupInput(id, label, choices = choices, selected = preload, ...),
    "textarea" = shiny::textareaInput(id, label, value = preload, placeholder = help, ...),
    shiny::textInput(id, label, value = preload, placeholder = help, ...)
  )

  # return input
  return(input)

}


# property selectors ------------------------------------------------------

# property_selector_market_survey <- function(
    #     id,
#     choices = market_survey_property_choices,
#     selected = market_survey_property_choices[[1]],
#     multiple = FALSE,
#     width = "100%",
#     ...
# ) {
#
#   label <- htmltools::span(
#     shiny::icon("building"),
#     " Select a Property:"
#   )
#
#   shinyWidgets::pickerInput(
#     inputId = id,
#     label = label,
#     multiple = multiple,
#     width = width,
#     choices = choices,
#     selected = selected,
#     choicesOpt = market_survey_property_choice_options,
#     options = market_survey_property_picker_options,
#     ...
#   )
#
# }

# demo in app
# library(shiny)
# shiny::shinyApp(
#   ui = shiny::fluidPage(
#     property_selector_market_survey("property_selector")
#   ),
#   server = function(input, output, session) {
#
#   }
# )


# leasing week ------------------------------------------------------------

#' Custom Leasing Week Period Date Range Input
#'
#' @description
#' This custom input is designed to allow the user to select a date range
#' representing a leasing week period. The input is a datepicker that
#' allows the user to select a single date. The input will then automatically
#' select the entire week period (Monday to Sunday) that the selected date
#' falls within.
#'
#' The input is built leveraging the [shinyWidgets::airDatepickerInput()] function.
#'
#' @param id ID to use for the input.
#' @inheritParams shinyWidgets::airDatepickerInput
#' @inheritDotParams shinyWidgets::airDatepickerInput
#'
#' @returns [htmltools::tagList()] containing the custom leasing week input.
#'
#' @export
#'
#' @importFrom glue glue
#' @importFrom htmltools tagList tags HTML
#' @importFrom shinyWidgets airDatepickerInput
#'
#' @examples
#' leasing_week_input("leasing_week")
leasing_week_input <- function(
    id,
    label = icon_text("calendar-alt", "Select Leasing Week"),
    value = NULL,
    inline = FALSE,
    ...
) {

  # derive initial values for input dates
  value <- get_weekly_period(as_of_date = value)

  # derive JS
  input_custom_js <- glue::glue(
    "$(document).ready(function () {
    var picker = $('#{{id}}').data('datepicker');
    picker.update({
        onSelect: function (formattedDate, date, inst) {
            if (date) {
                var selectedDate = new Date(date);
                var day = selectedDate.getDay();
                var diff = selectedDate.getDate() - day + (day === 0 ? -6 : 1);
                var monday = new Date(selectedDate.setDate(diff));
                var sunday = new Date(monday);
                sunday.setDate(sunday.getDate() + 6);

                inst.selectDate([monday, sunday]);
            }
        }
    });
});",
    id = id,
    .open = "{{",
    .close = "}}"
  )

  htmltools::tagList(
    shinyWidgets::airDatepickerInput(
      inputId = id,
      label = label,
      value = value,
      range = TRUE,
      dateFormat = "yyyy-MM-dd",
      firstDay = 1,  # Monday as first day of week
      view = "days",
      minView = "days",
      onlyTimepicker = FALSE,
      autoClose = TRUE,
      update_on = "change",
      inline = inline,
      ...
    ),
    htmltools::tags$script(htmltools::HTML(input_custom_js))
  )

}

update_leasing_week_input <- function(
  session = shiny::getDefaultReactiveDomain(),
  id,
  value,
  ...
) {

  shinyWidgets::updateAirDateInput(
    session = session,
    inputId = id,
    value = value,
    ...
  )

}




