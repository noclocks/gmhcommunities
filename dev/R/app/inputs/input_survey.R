
#  ------------------------------------------------------------------------
#
# Title : Custom Survey Input
#    By : Jimmy Briggs
#  Date : 2024-12-03
#
#  ------------------------------------------------------------------------

survey_input <- function(
    id,
    name,
    icon,
    type,
    choices,
    required,
    preload,
    validation,
    help,
    ...
) {

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
