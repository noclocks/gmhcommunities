
#  ------------------------------------------------------------------------
#
# Title : survey_insights_trends Shiny Module
#    By : Jimmy Briggs
#  Date : 2024-11-27
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' survey_insights_trends Shiny Module
#'
#' @name mod_survey_insights_trends
#'
#' @description
#' A Shiny Module for ...
#'
#' - `mod_survey_insights_trends_ui()`: User interface
#' - `mod_survey_insights_trends_server()`: Server logic
#'
#' @param id Module's namespace ID.
#'
#' @return
#' - `mod_survey_insights_trends_ui()`: UI HTML Output.
#' - `mod_survey_insights_trends_server()`: Reactive values returned from server logic.
#'
#' @examples
#' if (interactive()) {
#'   mod_survey_insights_trends_demo()
#' }
NULL


# UI ----------------------------------------------------------------------

#' @rdname mod_survey_insights_trends
#' @export
#' @importFrom shiny NS
#' @importFrom htmltools tagList tags
mod_survey_insights_trends_ui <- function(id) {

  ns <- shiny::NS(id)

  htmltools::tagList(
    bslib::card(
      shiny::sliderInput(
        ns("rent_range"),
        "Market Rent Range",
        min = 0, max = 2000, value = c(1000, 2000)
      )
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header(
          bsicons::bs_icon("calendar"),
          "Rent Comparison"
        ),
        bslib::card_body(
          apexcharter::apexchartOutput(ns("rent_comparison"))
        )
      ),
      bslib::card(
        bslib::card_header(
          bsicons::bs_icon("bar-chart-line"),
          "Occupancy Comparison"
        ),
        bslib::card_body(
          apexcharter::apexchartOutput(ns("occupancy_chart"))
        )
      )
    )

  )
}


# server ------------------------------------------------------------------

#' @rdname mod_survey_insights_trends
#' @export
#' @importFrom shiny moduleServer
#' @importFrom cli cat_rule
mod_survey_insights_trends_server <- function(id) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      cli::cat_rule("mod_survey_insights_trends_server")

      market_data <- data.frame(
        property = c("Property A", "Property B", "Property C", "Property D"),
        date = as.Date(c("2023-01-01", "2023-01-01", "2023-01-01", "2023-01-01")),
        market_rent = c(1500, 1600, 1400, 1700),
        effective_rent = c(1450, 1550, 1350, 1650),
        occupancy = c(95, 92, 98, 94),
        units = c(200, 150, 300, 250),
        stringsAsFactors = FALSE
      )


      filtered_data <- reactive({
        data <- market_data

        data |>
          filter(market_rent >= input$rent_range[1],
                 market_rent <= input$rent_range[2])
      })

      output$rent_comparison <- renderApexchart({
        apex(
          data = filtered_data(),
          mapping = aes(x = property, y = market_rent),
          type = "column"
        ) |>
          ax_title(text = "Market vs Effective Rent") |>
          ax_series(list(
            name = "Market Rent",
            data = filtered_data()$market_rent
          )) |>
          ax_series(list(
            name = "Effective Rent",
            data = filtered_data()$effective_rent
          )) |>
          ax_xaxis(title = list(text = "Property")) |>
          ax_yaxis(title = list(text = "Rent ($)"))
      })

      # Occupancy Chart
      output$occupancy_chart <- renderApexchart({
        apex(
          data = filtered_data(),
          mapping = aes(x = property, y = occupancy),
          type = "column"
        ) |>
          ax_title(text = "Occupancy Rates") |>
          ax_xaxis(title = list(text = "Property")) |>
          ax_yaxis(title = list(text = "Occupancy (%)"))
      })

      return(
        list(

        )
      )

    }
  )
}


# demo --------------------------------------------------------------------

#' @rdname mod_survey_insights_trends
#' @export
#' @importFrom pkgload load_all
#' @importFrom bslib page_fluid bs_theme
#' @importFrom shiny shinyApp
mod_survey_insights_trends_demo <- function() {

  pkgload::load_all()

  ui <- bslib::page_fluid(
    title = "Demo",
    theme = bslib::bs_theme(version = 5),
    lang = "en",
    mod_survey_insights_trends_ui("demo")
  )

  server <- function(input, output, session) {
    mod_survey_insights_trends_server("demo")
  }

  shiny::shinyApp(ui, server)
}
