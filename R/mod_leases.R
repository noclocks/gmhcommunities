
#  ------------------------------------------------------------------------
#
# Title : leases Shiny Module
#    By : Jimmy Briggs
#  Date : 2024-11-27
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' leases Shiny Module
#'
#' @name mod_leases
#'
#' @description
#' A Shiny Module for ...
#'
#' - `mod_leases_ui()`: User interface
#' - `mod_leases_server()`: Server logic
#'
#' @param id Module's namespace ID.
#'
#' @return
#' - `mod_leases_ui()`: UI HTML Output.
#' - `mod_leases_server()`: Reactive values returned from server logic.
#'
#' @examples
#' if (interactive()) {
#'   mod_leases_demo()
#' }
NULL


# UI ----------------------------------------------------------------------

#' @rdname mod_leases
#' @export
#' @importFrom shiny NS
#' @importFrom htmltools tagList tags
mod_leases_ui <- function(id) {

  ns <- shiny::NS(id)

  htmltools::tagList(

  )
}


# server ------------------------------------------------------------------

#' @rdname mod_leases
#' @export
#' @importFrom shiny moduleServer
#' @importFrom cli cat_rule
mod_leases_server <- function(id) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      cli::cat_rule("mod_leases_server")

      return(
        list(

        )
      )

    }
  )
}


# demo --------------------------------------------------------------------

#' @rdname mod_leases
#' @export
#' @importFrom pkgload load_all
#' @importFrom bslib page_fluid bs_theme
#' @importFrom shiny shinyApp
mod_leases_demo <- function() {

  pkgload::load_all()

  ui <- bslib::page_fluid(
    title = "Demo",
    theme = bslib::bs_theme(version = 5),
    lang = "en",
    mod_leases_ui("demo")
  )

  server <- function(input, output, session) {
    mod_leases_server("demo")
  }

  shiny::shinyApp(ui, server)
}
