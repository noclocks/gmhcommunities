
#  ------------------------------------------------------------------------
#
# Title : Market Survey - Property Summary Shiny Module
#    By : Jimmy Briggs
#  Date : 2024-11-27
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' Market Survey - Property Summary Shiny Module
#'
#' @name mod_market_property_summary
#'
#' @description
#' A Shiny Module for the Property Summary Section of the Market Survey.
#'
#' - `mod_market_property_summary_ui()`: User interface
#' - `mod_market_property_summary_server()`: Server logic
#'
#' @param id Module's namespace ID.
#'
#' @return
#' - `mod_market_property_summary_ui()`: UI HTML Output.
#' - `mod_market_property_summary_server()`: Reactive values returned from server logic.
#'
#' @examples
#' if (interactive()) {
#'   mod_market_property_summary_demo()
#' }
NULL


# UI ----------------------------------------------------------------------

#' @rdname mod_market_property_summary
#' @export
#' @importFrom shiny NS
#' @importFrom htmltools tagList tags
mod_market_property_summary_ui <- function(id) {

  ns <- shiny::NS(id)

  # htmltools::tagList(
  bslib::nav_panel(
    title = "Property Summary",
    value = "property_summary",
    icon = bsicons::bs_icon("building"),
    bslib::layout_column_wrap(
      shinyWidgets::textInputIcon(
        ns("name"),
        "Property Name",
        icon = bsicons::bs_icon("building")
      ),
      shinyWidgets::textInputIcon(
        ns("website"),
        "Website",
        icon = bsicons::bs_icon("globe")
      ),
      shinyWidgets::textInputIcon(
        ns("address"),
        "Address",
        icon = bsicons::bs_icon("map")
      ),
      shinyWidgets::textInputIcon(
        ns("phone_number"),
        "Phone Number",
        icon = bsicons::bs_icon("phone")
      ),
      shinyWidgets::textInputIcon(
        ns("developer"),
        "Developer",
        icon = bsicons::bs_icon("person")
      ),
      shinyWidgets::pickerInput(
        ns("property_type"),
        label = htmltools::tags$span(bsicons::bs_icon("list"), " Property Type"),
        choices = c("Student", "Conventional", "Affordable", "Innovative")
      ),
      shinyWidgets::pickerInput(
        ns("property_status"),
        label = htmltools::tags$span(bsicons::bs_icon("list"), " Property Status"),
        choices = c("Operational", "New Construction", "Undergoing Renovations")
      ),
      shinyWidgets::pickerInput(
        ns("product_type"),
        label = htmltools::tags$span(bsicons::bs_icon("list"), " Product Type"),
        choices = c("High-Rise", "Mid-Rise", "Wrap", "Garden", "Cottage", "Single Family")
      ),
      shinyWidgets::noUiSliderInput(
        ns("property_rating"),
        label = htmltools::tags$span(bsicons::bs_icon("star"), "Property Rating"),
        min = 0,
        max = 5,
        step = 0.5,
        value = 0
      ),
      shinyWidgets::pickerInput(
        ns("comp_status"),
        label = htmltools::tags$span(bsicons::bs_icon("list"), " Comp Status"),
        choices = c("Subject Property", "Tier 1", "Tier 2")
      ),
      shinyWidgets::numericInputIcon(
        ns("year_built"),
        label = htmltools::tags$span(bsicons::bs_icon("calendar"), " Year Built"),
        max = lubridate::year(Sys.Date()),
        step = 1,
        value = 2017
      ),
      shinyWidgets::airDatepickerInput(
        ns("most_recent_sale"),
        label = htmltools::tags$span(bsicons::bs_icon("calendar"), " Most Recent Sale"),
        maxDate = Sys.Date(),
        view = "days",
        todayButton = TRUE,
        autoClose = TRUE
      ),
      shinyWidgets::numericInputIcon(
        ns("distance_from_campus"),
        label = "Distance from Campus (Miles)",
        min = 0,
        step = 0.1,
        value = 1.1,
        icon = bsicons::bs_icon("map")
      )
    )
  )
  # )
}

# server ------------------------------------------------------------------

#' @rdname mod_market_property_summary
#' @export
#' @importFrom shiny moduleServer reactiveValues observe
#' @importFrom cli cat_rule
mod_market_property_summary_server <- function(id) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      cli::cat_rule("mod_market_property_summary_server")

      property_summary_inputs <- shiny::reactiveValues()

      shiny::observe({
        property_summary_inputs(
          name = input$name,
          website = input$website,
          address = input$address,
          phone_number = input$phone_number,
          developer = input$developer,
          property_type = input$property_type,
          property_status = input$property_status,
          product_type = input$product_type,
          property_rating = input$property_rating,
          comp_status = input$comp_status,
          year_built = input$year_built,
          most_recent_sale = input$most_recent_sale,
          distance_from_campus = input$distance_from_campus
        )
      })

      return(property_summary_inputs)

    }
  )
}


# demo --------------------------------------------------------------------

#' @rdname mod_market_property_summary
#' @export
#' @importFrom pkgload load_all
#' @importFrom bslib page_fluid bs_theme
#' @importFrom shiny shinyApp
mod_market_property_summary_demo <- function() {

  pkgload::load_all()

  ui <- bslib::page_fluid(
    title = "Demo",
    theme = bslib::bs_theme(version = 5),
    lang = "en",
    bslib::navset_card_tab(
      mod_market_property_summary_ui("demo"),
      footer = htmltools::tagList(
        shiny::actionButton("submit", "Submit", icon = shiny::icon("check")
        )
      )
    )
  )

  server <- function(input, output, session) {
    mod_market_property_summary_server("demo")
  }

  shiny::shinyApp(ui, server)
}
