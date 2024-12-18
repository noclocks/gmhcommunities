
#  ------------------------------------------------------------------------
#
# Title : property_units Shiny Module
#    By : Jimmy Briggs
#  Date : 2024-11-27
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' property_units Shiny Module
#'
#' @name mod_property_units
#'
#' @description
#' A Shiny Module for ...
#'
#' - `mod_property_units_ui()`: User interface
#' - `mod_property_units_server()`: Server logic
#'
#' @param id Module's namespace ID.
#'
#' @return
#' - `mod_property_units_ui()`: UI HTML Output.
#' - `mod_property_units_server()`: Reactive values returned from server logic.
#'
#' @examples
#' if (interactive()) {
#'   mod_property_units_demo()
#' }
NULL


# UI ----------------------------------------------------------------------

#' @rdname mod_property_units
#' @export
#' @importFrom shiny NS
#' @importFrom htmltools tagList tags
mod_property_units_ui <- function(id) {

  ns <- shiny::NS(id)

  htmltools::tagList(

  )
}


# server ------------------------------------------------------------------

#' @rdname mod_property_units
#' @export
#' @importFrom shiny moduleServer
#' @importFrom cli cat_rule
mod_property_units_server <- function(id) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      cli::cat_rule("mod_property_units_server")

      return(
        list(

        )
      )

    }
  )
}


# demo --------------------------------------------------------------------

#' @rdname mod_property_units
#' @export
#' @importFrom pkgload load_all
#' @importFrom bslib page_fluid bs_theme
#' @importFrom shiny shinyApp
mod_property_units_demo <- function() {

  pkgload::load_all()

  ui <- bslib::page_fluid(
    title = "Demo",
    theme = bslib::bs_theme(version = 5),
    lang = "en",
    mod_property_units_ui("demo")
  )

  server <- function(input, output, session) {
    mod_property_units_server("demo")
  }

  shiny::shinyApp(ui, server)
}
