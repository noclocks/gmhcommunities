
#  ------------------------------------------------------------------------
#
# Title : survey_insights_results Shiny Module
#    By : Jimmy Briggs
#  Date : 2024-11-27
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' survey_insights_results Shiny Module
#'
#' @name mod_survey_insights_results
#'
#' @description
#' A Shiny Module for ...
#'
#' - `mod_survey_insights_results_ui()`: User interface
#' - `mod_survey_insights_results_server()`: Server logic
#'
#' @param id Module's namespace ID.
#'
#' @return
#' - `mod_survey_insights_results_ui()`: UI HTML Output.
#' - `mod_survey_insights_results_server()`: Reactive values returned from server logic.
#'
#' @examples
#' if (interactive()) {
#'   mod_survey_insights_results_demo()
#' }
NULL


# UI ----------------------------------------------------------------------

#' @rdname mod_survey_insights_results
#' @export
#' @importFrom shiny NS
#' @importFrom htmltools tagList tags
mod_survey_insights_results_ui <- function(id) {

  ns <- shiny::NS(id)

  htmltools::tagList(

  )
}


# server ------------------------------------------------------------------

#' @rdname mod_survey_insights_results
#' @export
#' @importFrom shiny moduleServer
#' @importFrom cli cat_rule
mod_survey_insights_results_server <- function(id) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      cli::cat_rule("mod_survey_insights_results_server")

      return(
        list(

        )
      )

    }
  )
}


# demo --------------------------------------------------------------------

#' @rdname mod_survey_insights_results
#' @export
#' @importFrom pkgload load_all
#' @importFrom bslib page_fluid bs_theme
#' @importFrom shiny shinyApp
mod_survey_insights_results_demo <- function() {

  pkgload::load_all()

  ui <- bslib::page_fluid(
    title = "Demo",
    theme = bslib::bs_theme(version = 5),
    lang = "en",
    mod_survey_insights_results_ui("demo")
  )

  server <- function(input, output, session) {
    mod_survey_insights_results_server("demo")
  }

  shiny::shinyApp(ui, server)
}
