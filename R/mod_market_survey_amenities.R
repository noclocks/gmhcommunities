
#  ------------------------------------------------------------------------
#
# Title : market_survey_amenities Shiny Module
#    By : Jimmy Briggs
#  Date : 2024-11-27
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' market_survey_amenities Shiny Module
#'
#' @name mod_market_survey_amenities
#'
#' @description
#' A Shiny Module for ...
#'
#' - `mod_market_survey_amenities_ui()`: User interface
#' - `mod_market_survey_amenities_server()`: Server logic
#'
#' @param id Module's namespace ID.
#'
#' @return
#' - `mod_market_survey_amenities_ui()`: UI HTML Output.
#' - `mod_market_survey_amenities_server()`: Reactive values returned from server logic.
#'
#' @examples
#' if (interactive()) {
#'   mod_market_survey_amenities_demo()
#' }
NULL


# UI ----------------------------------------------------------------------

#' @rdname mod_market_survey_amenities
#' @export
#' @importFrom shiny NS
#' @importFrom htmltools tagList tags
mod_market_survey_amenities_ui <- function(id) {

  ns <- shiny::NS(id)

  htmltools::tagList(
    bslib::card(
      bslib::card_title(
        "Property & Unit Amenities"
      ),
      bslib::layout_columns(
        bslib::card(
          bslib::card_header("Property Amenities"),
          rhandsontable::rHandsontableOutput(ns("property_amenities_table")) |>
            with_loader()
        ),
        bslib::card(
          bslib::card_header("Utilities Summary"),
          rhandsontable::rHandsontableOutput(ns("utilities_table")) |>
            with_loader()
        )
      )
    )
  )
}


# server ------------------------------------------------------------------

#' @rdname mod_market_survey_amenities
#' @export
#' @importFrom shiny moduleServer
#' @importFrom cli cat_rule
mod_market_survey_amenities_server <- function(id) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      cli::cat_rule("mod_market_survey_amenities_server")

      return(
        list(

        )
      )

    }
  )
}


# demo --------------------------------------------------------------------

#' @rdname mod_market_survey_amenities
#' @export
#' @importFrom pkgload load_all
#' @importFrom bslib page_fluid bs_theme
#' @importFrom shiny shinyApp
mod_market_survey_amenities_demo <- function() {

  pkgload::load_all()

  ui <- bslib::page_fluid(
    title = "Demo",
    theme = bslib::bs_theme(version = 5),
    lang = "en",
    mod_market_survey_amenities_ui("demo")
  )

  server <- function(input, output, session) {
    mod_market_survey_amenities_server("demo")
  }

  shiny::shinyApp(ui, server)
}
