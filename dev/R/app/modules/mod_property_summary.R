
#  ------------------------------------------------------------------------
#
# Title : property_summary Shiny Module
#    By : Jimmy Briggs
#  Date : 2024-11-27
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' property_summary Shiny Module
#'
#' @name mod_property_summary
#'
#' @description
#' A Shiny Module for ...
#'
#' - `mod_property_summary_ui()`: User interface
#' - `mod_property_summary_server()`: Server logic
#'
#' @param id Module's namespace ID.
#'
#' @return
#' - `mod_property_summary_ui()`: UI HTML Output.
#' - `mod_property_summary_server()`: Reactive values returned from server logic.
#'
#' @examples
#' if (interactive()) {
#'   mod_property_summary_demo()
#' }
NULL


# UI ----------------------------------------------------------------------

#' @rdname mod_property_summary
#' @export
#' @importFrom shiny NS
#' @importFrom htmltools tagList tags
mod_property_summary_ui <- function(id) {


}


# server ------------------------------------------------------------------

#' @rdname mod_property_summary
#' @export
#' @importFrom shiny moduleServer
#' @importFrom cli cat_rule


# demo --------------------------------------------------------------------

#' @rdname mod_property_summary
#' @export
#' @importFrom pkgload load_all
#' @importFrom bslib page_fluid bs_theme
#' @importFrom shiny shinyApp
mod_property_summary_demo <- function() {

  pkgload::load_all()

  ui <- bslib::page_fluid(
    title = "Demo",
    theme = bslib::bs_theme(version = 5),
    lang = "en",
    mod_property_summary_ui("demo")
  )

  server <- function(input, output, session) {
    mod_property_summary_server("demo")
  }

  shiny::shinyApp(ui, server)
}






# class = "p-2",
# htmltools::tags$div(
#   style = "margin-bottom: 10px;",
#   htmltools::tags$label(
#     bsicons::bs_icon("building-fill"),
#     "Property Name",
#     style = "font-size: 0.9rem;"
#   ),
#   htmltools::tags$div(
#     "1077 Commonwealth",
#     style = "padding: 4px 8px; border: 1px solid #ccc; border-radius: 4px; background-color: #e9ecef;"
#   )
# ),
# htmltools::tags$div(
#   class = "container-fluid p-0",
#   style = "font-size: 0.9rem;",
#   htmltools::tags$div(
#     class = "row g-2",
#     shiny::textInput(
#       ns("website"),
#       label = htmltools::tags$span(bsicons::bs_icon("globe"), "Website"),
#       value = "http://www.1077commonwealth.com"
#     ),
#     shiny::textInput(ns("address"), label = htmltools::tags$span(bsicons::bs_icon("geo-alt"), "Address"), value = "1077 Commonwealth Ave, Boston, MA 02215"),
#     shiny::textInput(ns("phone"), label = htmltools::tags$span(bsicons::bs_icon("telephone"), "Phone"), value = "617-500-6481"),
#     shiny::selectInput(ns("developer"), label = htmltools::tags$span(bsicons::bs_icon("tools"), "Developer"), choices = c("BPDA", "Other"), selected = "BPDA"),
#     shiny::selectInput(ns("manager"), label = htmltools::tags$span(bsicons::bs_icon("person-workspace"), "Manager"), choices = c("GMH Communities", "Other"), selected = "GMH Communities"),
#     shiny::textInput(ns("owner"), label = htmltools::tags$span(bsicons::bs_icon("person"), "Owner"), value = "AGC + GMH Communities"),
#     shiny::selectInput(ns("property_type"), label = htmltools::tags$span(bsicons::bs_icon("houses"), "Property Type"), choices = c("Student", "Conventional", "Affordable", "Innovative"), selected = "Student"),
#     shiny::sliderInput(ns("property_rating"), label = htmltools::tags$span(bsicons::bs_icon("star"), "Property Rating"), min = 1, max = 5, value = 2, step = 1),
#     shiny::selectInput(ns("property_status"), label = htmltools::tags$span(bsicons::bs_icon("info-circle"), "Property Status"), choices = c("New Construction", "Operational", "Undergoing Renovation"), selected = "Operational"),
#     shiny::selectInput(ns("comp_status"), label = htmltools::tags$span(bsicons::bs_icon("diagram-2"), "Comp Status"), choices = c("Subject Property", "Tier 1", "Tier 2"), selected = "Subject Property"),
#     shiny::numericInput(ns("year_built"), label = htmltools::tags$span(bsicons::bs_icon("calendar"), "Year Built"), value = 2017),
#     shiny::dateInput(ns("most_recent_sale"), label = htmltools::tags$span(bsicons::bs_icon("calendar-date"), "Most Recent Sale"), value = "2019-01-01"),
#     shiny::sliderInput(ns("distance"), label = htmltools::tags$span(bsicons::bs_icon("map"), "Distance from Campus (miles)"), min = 0, max = 5, value = 0.1, step = 0.1)
#   )
# )
# )
# )
# )
