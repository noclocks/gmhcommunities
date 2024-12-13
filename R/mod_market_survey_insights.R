
#  ------------------------------------------------------------------------
#
# Title : market_survey_insights Shiny Module
#    By : Jimmy Briggs
#  Date : 2024-11-27
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' market_survey_insights Shiny Module
#'
#' @name mod_market_survey_insights
#'
#' @description
#' A Shiny Module for ...
#'
#' - `mod_market_survey_insights_ui()`: User interface
#' - `mod_market_survey_insights_server()`: Server logic
#'
#' @param id Module's namespace ID.
#'
#' @return
#' - `mod_market_survey_insights_ui()`: UI HTML Output.
#' - `mod_market_survey_insights_server()`: Reactive values returned from server logic.
#'
#' @examples
#' if (interactive()) {
#'   mod_market_survey_insights_demo()
#' }
NULL


# UI ----------------------------------------------------------------------

#' @rdname mod_market_survey_insights
#' @export
#' @importFrom shiny NS
#' @importFrom htmltools tagList tags
mod_market_survey_insights_ui <- function(id) {

  ns <- shiny::NS(id)

  htmltools::tagList(
    bslib::card(
      bslib::card_header(
        htmltools::tags$span(
          bsicons::bs_icon("bar-chart"),
          "Survey Insights: ",
          shiny::textOutput(ns("selected_property_title"), inline = TRUE)
        ),
        class = "bg-dark text-white"
      ),
      bslib::card_body(
        bslib::layout_column_wrap(
          width = 1/2,
          bslib::card(
            bslib::card_header(icon_text("line-chart", "Occupancy Trends")),
            bslib::card_body(
              apexcharter::apexchartOutput(ns("occupancy_chart")) |>
                with_loader()
            ),
            bslib::card_footer(
              shiny::textOutput(ns("occupancy_chart_last_updated"))
            )
          ),
          bslib::card(
            bslib::card_header(icon_text("line-chart", "Rent Comparison")),
            bslib::card_body(
              apexcharter::apexchartOutput(ns("rent_chart")) |>
                with_loader()
            ),
            bslib::card_footer(
              shiny::textOutput(ns("rent_chart_last_updated"))
            )
          )
        )
      ),
      bslib::card_footer(
        shiny::actionButton(ns("refresh_button"), "Refresh Insights")
      )
    )
  )
}


# server ------------------------------------------------------------------

#' @rdname mod_market_survey_insights
#' @export
#' @importFrom shiny moduleServer
#' @importFrom cli cat_rule
mod_market_survey_insights_server <- function(id) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
      cli::cat_rule("[Module]: mod_market_survey_leasing_summary_server()")

      output$occupancy_chart <- apexcharter::renderApexchart({

      })

      output$rent_chart <- apexcharter::renderApexchart({

      })

      return(
        list(

        )
      )

    }
  )
}


# demo --------------------------------------------------------------------

#' @rdname mod_market_survey_insights
#' @export
#' @importFrom pkgload load_all
#' @importFrom bslib page_fluid bs_theme
#' @importFrom shiny shinyApp
mod_market_survey_insights_demo <- function() {

  pkgload::load_all()

  ui <- bslib::page_fluid(
    title = "Demo",
    theme = bslib::bs_theme(version = 5),
    lang = "en",
    mod_market_survey_insights_ui("demo")
  )

  server <- function(input, output, session) {
    mod_market_survey_insights_server("demo")
  }

  shiny::shinyApp(ui, server)
}
