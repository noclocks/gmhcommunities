
#  ------------------------------------------------------------------------
#
# Title : pre_lease_details Shiny Module
#    By : Jimmy Briggs
#  Date : 2024-11-27
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' pre_lease_details Shiny Module
#'
#' @name mod_pre_lease_details
#'
#' @description
#' A Shiny Module for ...
#'
#' - `mod_pre_lease_details_ui()`: User interface
#' - `mod_pre_lease_details_server()`: Server logic
#'
#' @param id Module's namespace ID.
#'
#' @return
#' - `mod_pre_lease_details_ui()`: UI HTML Output.
#' - `mod_pre_lease_details_server()`: Reactive values returned from server logic.
#'
#' @examples
#' if (interactive()) {
#'   mod_pre_lease_details_demo()
#' }
NULL


# UI ----------------------------------------------------------------------

#' @rdname mod_pre_lease_details
#' @export
#' @importFrom shiny NS
#' @importFrom htmltools tagList tags
mod_pre_lease_details_ui <- function(id) {

  ns <- shiny::NS(id)

  htmltools::tagList(

  )
}


# server ------------------------------------------------------------------

#' @rdname mod_pre_lease_details
#' @export
#' @importFrom shiny moduleServer
#' @importFrom cli cat_rule
mod_pre_lease_details_server <- function(id) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      cli::cat_rule("mod_pre_lease_details_server")

      return(
        list(

        )
      )

    }
  )
}


# demo --------------------------------------------------------------------

#' @rdname mod_pre_lease_details
#' @export
#' @importFrom pkgload load_all
#' @importFrom bslib page_fluid bs_theme
#' @importFrom shiny shinyApp
mod_pre_lease_details_demo <- function() {

  pkgload::load_all()

  ui <- bslib::page_fluid(
    title = "Demo",
    theme = bslib::bs_theme(version = 5),
    lang = "en",
    mod_pre_lease_details_ui("demo")
  )

  server <- function(input, output, session) {
    mod_pre_lease_details_server("demo")
  }

  shiny::shinyApp(ui, server)
}
