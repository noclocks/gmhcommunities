
#  ------------------------------------------------------------------------
#
# Title : Analysis Shiny Module
#    By : Jimmy Briggs
#  Date : 2024-11-27
#
#  ------------------------------------------------------------------------


# topic -------------------------------------------------------------------

#' Analysis Shiny Module
#'
#' @name mod_analysis
#'
#' @description
#' analysis Shiny Module
#'
#' - `mod_analysis_ui()`: User interface
#' - `mod_analysis_server()`: Server logic
#'
#' @param id shiny module id
#'
#' @import shiny
NULL


# UI ----------------------------------------------------------------------

#' @rdname mod_analysis
#' @export
#' @importFrom shiny NS
#' @importFrom htmltools tagList
mod_analysis_ui <- function(id) {

  ns <- shiny::NS(id)

  htmltools::tagList(

  )
}


# server ------------------------------------------------------------------

#' @rdname mod_analysis
#' @export
#' @importFrom shiny moduleServer
#' @importFrom cli cat_rule
mod_analysis_server <- function(id) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      cli::cat_rule("mod_analysis_server")

      return(
        list(

        )
      )

    }
  )
}


# demo --------------------------------------------------------------------

#' @keywords internal
#' @noRd
mod_analysis_demo <- function() {

  ui <- bslib::page_fillable(
    theme = bslib::bs_theme(version = 5),
    title = "Demo",
    mod_analysis_ui("demo")
  )

  server <- function(input, output, session) {
    mod_analysis_server("demo")
  }

  shiny::shinyApp(ui, server)
}
