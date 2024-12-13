
#  ------------------------------------------------------------------------
#
# Title : Shiny App Custom Inputs
#    By : Jimmy Briggs
#  Date : 2024-12-03
#
#  ------------------------------------------------------------------------

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
#' @return [htmltools::tagList()] containing the custom leasing week input.
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




