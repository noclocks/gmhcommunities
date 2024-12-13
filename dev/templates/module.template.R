
#  ------------------------------------------------------------------------
#
# Title : {{name}} Shiny Module
#    By : Jimmy Briggs
#  Date : 2024-11-27
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' {{name}} Shiny Module
#'
#' @name mod_{{name}}
#'
#' @description
#' A Shiny Module for ...
#'
#' - `mod_{{name}}_ui()`: User interface
#' - `mod_{{name}}_server()`: Server logic
#'
#' @param id Module's namespace ID.
#'
#' @return
#' - `mod_{{name}}_ui()`: UI HTML Output.
#' - `mod_{{name}}_server()`: Reactive values returned from server logic.
#'
#' @examples
#' if (interactive()) {
#'   mod_{{name}}_demo()
#' }
NULL


# UI ----------------------------------------------------------------------

#' @rdname mod_{{name}}
#' @export
#' @importFrom shiny NS
#' @importFrom htmltools tagList tags
mod_{{name}}_ui <- function(id) {

  ns <- shiny::NS(id)

  htmltools::tagList(

  )
}


# server ------------------------------------------------------------------

#' @rdname mod_{{name}}
#' @export
#' @importFrom shiny moduleServer
#' @importFrom cli cat_rule
mod_{{name}}_server <- function(id) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      cli::cat_rule("mod_{{name}}_server")

      return(
        list(

        )
      )

    }
  )
}


# demo --------------------------------------------------------------------

#' @rdname mod_{{name}}
#' @export
#' @importFrom pkgload load_all
#' @importFrom bslib page_fluid bs_theme
#' @importFrom shiny shinyApp
mod_{{name}}_demo <- function() {

  pkgload::load_all()

  ui <- bslib::page_fluid(
    title = "Demo",
    theme = bslib::bs_theme(version = 5),
    lang = "en",
    mod_{{name}}_ui("demo")
  )

  server <- function(input, output, session) {
    mod_{{name}}_server("demo")
  }

  shiny::shinyApp(ui, server)
}
