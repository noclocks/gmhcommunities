
#  ------------------------------------------------------------------------
#
# Title : market_survey_rents Shiny Module
#    By : Jimmy Briggs
#  Date : 2024-11-27
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' market_survey_rents Shiny Module
#'
#' @name mod_market_survey_rents
#'
#' @description
#' A Shiny Module for ...
#'
#' - `mod_market_survey_rents_ui()`: User interface
#' - `mod_market_survey_rents_server()`: Server logic
#'
#' @param id Module's namespace ID.
#'
#' @return
#' - `mod_market_survey_rents_ui()`: UI HTML Output.
#' - `mod_market_survey_rents_server()`: Reactive values returned from server logic.
#'
#' @examples
#' if (interactive()) {
#'   mod_market_survey_rents_demo()
#' }
NULL


# UI ----------------------------------------------------------------------

#' @rdname mod_market_survey_rents
#' @export
#' @importFrom shiny NS
#' @importFrom htmltools tagList tags
mod_market_survey_rents_ui <- function(id) {

  ns <- shiny::NS(id)

  htmltools::tagList(
    htmltools::tags$head(
      shinyjs::useShinyjs(),
      waiter::use_waiter(),
      shinyWidgets::useSweetAlert(),
      waiter::waiterShowOnLoad(waiter::spin_fading_circles())
    ),
    bslib::navset_card_pill(
      id = ns("rents"),
      title = icon_text("table", "Market Survey Rent Tables"),
      sidebar = NULL,
      header = bslib::card_header(
        shiny::textOutput(ns("selected_property_title"), inline = TRUE),
        class = "bg-dark text-white"
      ),
      footer = bslib::card_footer(
        shiny::textOutput(ns("last_updated_at"), inline = TRUE)
      ),
      # nav panels for tables and charts
      bslib::nav_panel(
        title = "Rent by Floor Plan",
        icon = bsicons::bs_icon("table"),
        selected = TRUE,
        bslib::card(
          htmltools::tags$h3("Rents by Floorplan"),
          reactable::reactableOutput(ns("rents_by_floorplan_table")) |>
            with_loader()
        )
      ),

      bslib::nav_panel(
        title = "Rent by Unit Type",
        icon = bsicons::bs_icon("table"),
        bslib::card(
          htmltools::tags$h3("Average Rents by Unit Type"),
          reactable::reactableOutput(ns("average_rents_by_unit_type_table")) |>
            with_loader()
        )
      ),

      bslib::nav_panel(
        title = "Market vs. Effective Rent",
        icon = bsicons::bs_icon("chart-line"),
        bslib::card(
          htmltools::tags$h3("Market vs. Effective Rent"),
          apexcharter::apexchartOutput(ns("market_vs_effective_rent_chart")) |>
            with_loader()
        )
      ),

      bslib::nav_panel(
        title = "Average Market Rent by Unit Type",
        icon = bsicons::bs_icon("chart-line"),
        bslib::card(
          htmltools::tags$h3("Average Market Rent by Unit Type"),
          apexcharter::apexchartOutput(ns("average_market_rent_by_unit_type_chart")) |>
            with_loader()
        )
      )
    )
  )

}


# server ------------------------------------------------------------------

#' @rdname mod_market_survey_rents
#' @export
#' @importFrom shiny moduleServer
#' @importFrom cli cat_rule
mod_market_survey_rents_server <- function(id) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      cli::cat_rule("mod_market_survey_rents_server")

      output$selected_property_title <- shiny::renderText({
        "Selected Property Title"
      })

      output$last_updated_at <- shiny::renderText({
        "Last Updated At"
      })



      return(
        list(

        )
      )

    }
  )
}


# demo --------------------------------------------------------------------

#' @rdname mod_market_survey_rents
#' @export
#' @importFrom pkgload load_all
#' @importFrom bslib page_fluid bs_theme
#' @importFrom shiny shinyApp
mod_market_survey_rents_demo <- function() {

  pkgload::load_all()

  ui <- bslib::page_fluid(
    title = "Demo",
    theme = bslib::bs_theme(version = 5),
    lang = "en",
    mod_market_survey_rents_ui("demo")
  )

  server <- function(input, output, session) {
    mod_market_survey_rents_server("demo")
  }

  shiny::shinyApp(ui, server)
}
