
#  ------------------------------------------------------------------------
#
# Title : market_survey_notes Shiny Module
#    By : Jimmy Briggs
#  Date : 2024-11-27
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' market_survey_notes Shiny Module
#'
#' @name mod_market_survey_notes
#'
#' @description
#' A Shiny Module for ...
#'
#' - `mod_market_survey_notes_ui()`: User interface
#' - `mod_market_survey_notes_server()`: Server logic
#'
#' @param id Module's namespace ID.
#'
#' @return
#' - `mod_market_survey_notes_ui()`: UI HTML Output.
#' - `mod_market_survey_notes_server()`: Reactive values returned from server logic.
#'
#' @examples
#' if (interactive()) {
#'   mod_market_survey_notes_demo()
#' }
NULL


# UI ----------------------------------------------------------------------

#' @rdname mod_market_survey_notes
#' @export
#' @importFrom shiny NS
#' @importFrom htmltools tagList tags
mod_market_survey_notes_ui <- function(id) {

  ns <- shiny::NS(id)

  htmltools::tagList(
    bslib::card(
      bslib::card_header(icon_text("clipboard", "Notes")),
      bslib::card_body(
        bslib::layout_columns(
          htmltools::tags$div(
            shiny::textAreaInput(
              ns("leasing_notes"),
              label = icon_text("clipboard-list", "Leasing Notes"),
              height = "150px",
              width = "100%",
              resize = "vertical"
            ),
            shiny::actionButton(
              ns("submit_leasing_notes"),
              label = "Add New Leasing Note",
              icon = shiny::icon("plus")
            )
          ),

          htmltools::tags$div(
            shiny::textAreaInput(
              ns("property_notes"),
              label = icon_text("building", "Property Notes"),
              height = "150px",
              resize = "vertical",
              width = "100%"
            ),
            shiny::actionButton(
              ns("submit_property_notes"),
              label = "Add New Property Note",
              icon = shiny::icon("plus")
            )
          )
        )#,
        # # shiny::hr(),
        # reactable::reactableOutput(ns("notes_table")) |>
          # with_loader()
      )
    )
  )

}


# server ------------------------------------------------------------------

#' @rdname mod_market_survey_notes
#' @export
#' @importFrom shiny moduleServer
#' @importFrom cli cat_rule
mod_market_survey_notes_server <- function(id) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      cli::cat_rule("mod_market_survey_notes_server")

      return(
        list(

        )
      )

    }
  )
}


# demo --------------------------------------------------------------------

#' @rdname mod_market_survey_notes
#' @export
#' @importFrom pkgload load_all
#' @importFrom bslib page_fluid bs_theme
#' @importFrom shiny shinyApp
mod_market_survey_notes_demo <- function() {

  pkgload::load_all()

  ui <- bslib::page_fluid(
    title = "Demo",
    theme = bslib::bs_theme(version = 5),
    lang = "en",
    mod_market_survey_notes_ui("demo")
  )

  server <- function(input, output, session) {
    mod_market_survey_notes_server("demo")
  }

  shiny::shinyApp(ui, server)
}
