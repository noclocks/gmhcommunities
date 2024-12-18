
#  ------------------------------------------------------------------------
#
# Title : survey_insights_swot Shiny Module
#    By : Jimmy Briggs
#  Date : 2024-11-27
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' survey_insights_swot Shiny Module
#'
#' @name mod_survey_insights_swot
#'
#' @description
#' A Shiny Module for ...
#'
#' - `mod_survey_insights_swot_ui()`: User interface
#' - `mod_survey_insights_swot_server()`: Server logic
#'
#' @param id Module's namespace ID.
#'
#' @return
#' - `mod_survey_insights_swot_ui()`: UI HTML Output.
#' - `mod_survey_insights_swot_server()`: Reactive values returned from server logic.
#'
#' @examples
#' if (interactive()) {
#'   mod_survey_insights_swot_demo()
#' }
NULL


# UI ----------------------------------------------------------------------

#' @rdname mod_survey_insights_swot
#' @export
#' @importFrom shiny NS
#' @importFrom htmltools tagList tags
mod_survey_insights_swot_ui <- function(id) {

  ns <- shiny::NS(id)

  htmltools::tagList(
    bslib::layout_columns(
      col_widths = c(6, 6, 6, 6),
      bslib::card(
        bslib::card_header("Strengths"),
        style = "background-color: #d4edda;",
        shiny::textAreaInput(
          ns("strengths"),
          NULL,
          width = "100%",
          height = "150px",
          value = "1. Prime location\n2. High occupancy rates\n3. Quality amenities")
      ),
      bslib::card(
        bslib::card_header("Weaknesses"),
        style = "background-color: #f8d7da;",
        shiny::textAreaInput(
          "weaknesses",
          NULL,
          width = "100%",
          height = "150px",
          value = "1. Aging infrastructure\n2. Limited parking\n3. Higher operating costs"
        )
      ),
      bslib::card(
        bslib::card_header("Opportunities"),
        style = "background-color: #cce5ff;",
        shiny::textAreaInput(
          "opportunities",
          NULL,
          width = "100%",
          height = "150px",
          value = "1. Market expansion\n2. Renovation potential\n3. New target demographics"
        )
      ),
      bslib::card(
        bslib::card_header("Threats"),
        style = "background-color: #fff3cd;",
        shiny::textAreaInput(
          "threats",
          NULL,
          width = "100%",
          height = "150px",
          value = "1. New competition\n2. Economic downturn\n3. Changing regulations"
        )
      )
    )
  )
}


# server ------------------------------------------------------------------

#' @rdname mod_survey_insights_swot
#' @export
#' @importFrom shiny moduleServer
#' @importFrom cli cat_rule
mod_survey_insights_swot_server <- function(id) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      cli::cat_rule("mod_survey_insights_swot_server")

      return(
        list(

        )
      )

    }
  )
}


# demo --------------------------------------------------------------------

#' @rdname mod_survey_insights_swot
#' @export
#' @importFrom pkgload load_all
#' @importFrom bslib page_fluid bs_theme
#' @importFrom shiny shinyApp
mod_survey_insights_swot_demo <- function() {

  pkgload::load_all()

  ui <- bslib::page_fluid(
    title = "Demo",
    theme = bslib::bs_theme(version = 5),
    lang = "en",
    mod_survey_insights_swot_ui("demo")
  )

  server <- function(input, output, session) {
    mod_survey_insights_swot_server("demo")
  }

  shiny::shinyApp(ui, server)
}
