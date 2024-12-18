
#  ------------------------------------------------------------------------
#
# Title : Shiny Utilities
#    By : Jimmy Briggs
#  Date : 2024-12-04
#
#  ------------------------------------------------------------------------

#' Inverted versions of in
#' @noRd
`%notin%` <- Negate(`%in%`)


card_title <- function(title, icon = NULL, ...) {

  htmltools::tags$span(
    htmltools::tags$strong(
      icon_text(icon, title)
    )
  )

}

# loading spinners --------------------------------------------------------

#' Add Loading Spinners to Outputs
#'
#' @description
#' This function adds a loading spinner to a Shiny output element via
#' [shinycustomloader::withLoader()].
#'
#' @param output The output element to add the loading spinner to.
#' @inheritDotParams shinycustomloader::withLoader
#'
#' @return The output element with the loading spinner added.
#'
#' @seealso [shinycustomloader::withLoader()]
#'
#' @export
#'
#' @importFrom shinycustomloader withLoader
#'
#' @examplesIf interactive()
#' library(shiny)
#'
#' ui <- fluidPage(
#'   with_loader(
#'     plotOutput("plot")
#'   ),
#'   plotOutput("plot") |>
#'     with_loader()
#'   )
#' )
#'
#' server <- function(input, output, session) {
#'
#'   output$plot <- renderPlot({
#'     plot(1:10)
#'   })
#'
#'   outputOptions(output, "plot", suspendWhenHidden = FALSE)
#'
#' }
#'
#' shinyApp(ui = ui, server = server)
with_loader <- function(
  output,
  ...
) {

  shinycustomloader::withLoader(
    ui_element = output,
    ...
  )

}

# tooltips ----------------------------------------------------------------

#' Add Tooltips to Inputs
#'
#' @description
#' This function adds a tooltip to a Shiny input element via [bslib::tooltip()].
#'
#' @param input The input element to add the tooltip to.
#' @param tooltip_text The text to display in the tooltip.
#' @param placement The placement of the tooltip. Default is "right".
#'
#' @return The input element with the tooltip added.
#'
#' @seealso [bslib::tooltip()]
#'
#' @export
#'
#' @importFrom bslib tooltip
#' @importFrom bsicons bs_icon
#' @importFrom htmltools tags
#'
#' @examplesIf interactive()
#' library(shiny)
#' library(bslib)
#' library(bsicons)
#'
#' ui <- fluidPage(
#'   with_tooltip(
#'     textInput("text", "Text Input"),
#'     "This is a tooltip."
#'     ),
#'     # or pipe the input into the function
#'     textInput("text", "Text Input") |>
#'       with_tooltip("This is a tooltip.")
#' )
#'
#' shinyApp(ui = ui, server = function(input, output, session) {})
with_tooltip <- function(input, tooltip_text, placement = "right") {

  current_label <- input$children[[1]]$children[[1]]

  updated_label <- htmltools::tags$span(
    current_label,
    bslib::tooltip(
      bsicons::bs_icon("info-circle", style = "cursor: help;"),
      tooltip_text,
      placement = placement
    )
  )

  input$children[[1]]$children[[1]] <- updated_label

  return(input)
}

#' #' Add a Tooltip to a Table Header
#' #'
#' #' @param value Table header title.
#' #' @param tooltip Tooltip content.
#' #' @param ... Not in use.
#' #'
#' #' @importFrom tippy tippy
#' with_tooltip <- function(value, tooltip, ...) {
#'   tags$div(
#'     style = "cursor: help",
#'     tippy::tippy(value, tooltip, ...)
#'   )
#' }


# icon_text ---------------------------------------------------------------

#' Combine Icons and Text
#'
#' @name icon_text
#'
#' @description
#' These functions simply wrap an icon and some text in an inline HTML span tag.
#'
#' - `icon_text()` places the icon before the text
#' - `text_icon()` places the icon after the text
#'
#' Note that the position of the arguments is different between the two functions
#' to match the position of the icon in the output.
#'
#' @param icon The icon to display. This can be a character string or an icon
#'   object.
#' @param text The text to display.
#' @param .function The function to use to create the icon if it is a character string.
#'   Default is [shiny::icon()], other possible options are [bsicons::bs_icon()]
#'   or [fontawesome::fa_i()].
#'
#' @return [htmltools::tagList()] containing the icon and text.
#'
#' @export
#'
#' @examples
#' icon_text("user", "User")
#' icon_text(shiny::icon("user"), "User")
#'
#' text_icon("User", "user")
#' text_icon("User", shiny::icon("user"))
#'
#' @importFrom shiny icon
#' @importFrom bsicons bs_icon
#' @importFrom fontawesome fa_i
#' @importFrom htmltools tagList tags
#' @importFrom rlang is_function
#' @importFrom cli cli_abort
icon_text <- function(icon, text, .function = shiny::icon) {

  if (is.character(icon) && length(icon) == 1) {
    icon <- .function(icon)
  }

  text <- paste0(" ", text)

  htmltools::tags$span(
    icon,
    text
  )

}

#' @rdname icon_text
#' @export
#' @importFrom shiny icon
#' @importFrom bsicons bs_icon
#' @importFrom fontawesome fa_i
#' @importFrom htmltools tagList tags
#' @importFrom rlang is_function
#' @importFrom cli cli_abort
text_icon <- function(text, icon, .function = shiny::icon) {

  icon <- .function(icon)
  text <- paste0(text, " ")

  htmltools::tags$span(
    text,
    icon
  )

}

#' # messages ----------------------------------------------------------------
#'
#'
#'
#' # reactive triggers -------------------------------------------------------
#'
#' #' @keywords internal
#' #' @noRd
#' #' @importFrom shiny reactiveVal getDefaultReactiveDomain in_devmode
#' #' @importFrom cli cli_alert_info
#' init <- function(..., session = shiny::getDefaultReactiveDomain()){
#'
#'   lapply(
#'     list(...),
#'     function(name) {
#'       session$userData[[name]] <- shiny::reactiveVal(0)
#'       if (getOption("app.verbose", FALSE) || shiny::in_devmode()) {
#'         cli::cli_alert_info(
#'           "Initialized event-based reactive trigger for {.field {name}}."
#'         )
#'       }
#'     }
#'   )
#'
#' }
#'
#' #' @keywords internal
#' #' @noRd
#' #' @importFrom shiny getDefaultReactiveDomain in_devmode
#' #' @importFrom cli cli_alert_info
#' trigger <- function(..., session = shiny::getDefaultReactiveDomain()){
#'   lapply(
#'     list(...),
#'     function(name) {
#'       if (getOption("app.verbose", FALSE) || shiny::in_devmode()) {
#'         cli::cli_alert_info(
#'           "Triggering {.field {name}}'s reactive trigger.\n"
#'         )
#'         session$userData[[name]](
#'           session$userData[[name]]() + 1
#'         )
#'       }
#'     }
#'   )
#' }
#'
#' #' @keywords internal
#' #' @noRd
#' #' @importFrom shiny getDefaultReactiveDomain
#' #' @importFrom cli cli_alert_info
#' watch <- function(name, session = shiny::getDefaultReactiveDomain()){
#'   session$userData[[name]]()
#' }
#'
#'
