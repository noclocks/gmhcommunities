
#  ------------------------------------------------------------------------
#
# Title : pre_lease Shiny Module
#    By : Jimmy Briggs
#  Date : 2024-11-27
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' pre_lease Shiny Module
#'
#' @name mod_pre_lease
#'
#' @description
#' A Shiny Module for ...
#'
#' - `mod_pre_lease_ui()`: User interface
#' - `mod_pre_lease_server()`: Server logic
#'
#' @param id Module's namespace ID.
#'
#' @return
#' - `mod_pre_lease_ui()`: UI HTML Output.
#' - `mod_pre_lease_server()`: Reactive values returned from server logic.
#'
#' @examples
#' if (interactive()) {
#'   mod_pre_lease_demo()
#' }
NULL


# UI ----------------------------------------------------------------------

#' @rdname mod_pre_lease
#' @export
#' @importFrom shiny NS
#' @importFrom htmltools tagList tags
mod_pre_lease_ui <- function(id) {

  ns <- shiny::NS(id)

  htmltools::tagList(

  )
}


# server ------------------------------------------------------------------

#' @rdname mod_pre_lease
#' @export
#' @importFrom shiny moduleServer
#' @importFrom cli cat_rule
mod_pre_lease_server <- function(id) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      cli::cat_rule("mod_pre_lease_server")

      return(
        list(

        )
      )

    }
  )
}


# demo --------------------------------------------------------------------

#' @rdname mod_pre_lease
#' @export
#' @importFrom pkgload load_all
#' @importFrom bslib page_fluid bs_theme
#' @importFrom shiny shinyApp
mod_pre_lease_demo <- function() {

  pkgload::load_all()

  ui <- bslib::page_fluid(
    title = "Demo",
    theme = bslib::bs_theme(version = 5),
    lang = "en",
    mod_pre_lease_ui("demo")
  )

  server <- function(input, output, session) {
    mod_pre_lease_server("demo")
  }

  shiny::shinyApp(ui, server)
}
