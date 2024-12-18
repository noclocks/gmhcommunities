
#  ------------------------------------------------------------------------
#
# Title : pre_lease_summary Shiny Module
#    By : Jimmy Briggs
#  Date : 2024-11-27
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' pre_lease_summary Shiny Module
#'
#' @name mod_pre_lease_summary
#'
#' @description
#' A Shiny Module for ...
#'
#' - `mod_pre_lease_summary_ui()`: User interface
#' - `mod_pre_lease_summary_server()`: Server logic
#'
#' @param id Module's namespace ID.
#'
#' @return
#' - `mod_pre_lease_summary_ui()`: UI HTML Output.
#' - `mod_pre_lease_summary_server()`: Reactive values returned from server logic.
#'
#' @examples
#' if (interactive()) {
#'   mod_pre_lease_summary_demo()
#' }
NULL


# UI ----------------------------------------------------------------------

#' @rdname mod_pre_lease_summary
#' @export
#' @importFrom shiny NS
#' @importFrom htmltools tagList tags
mod_pre_lease_summary_ui <- function(id) {

  ns <- shiny::NS(id)

  htmltools::tagList(
    bslib::card(
      bslib::card_header(
        bsicons::bs_icon("clipboard-data"), "Pre-Lease Summary Table"
      ),
      bslib::card_body(
        DT::DTOutput(ns("pre_lease_summary_table"))
      )
    )
  )
}


# server ------------------------------------------------------------------

#' @rdname mod_pre_lease_summary
#' @export
#' @importFrom shiny moduleServer
#' @importFrom cli cat_rule
mod_pre_lease_summary_server <- function(id) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      cli::cat_rule("mod_pre_lease_summary_server")

      return(
        list(

        )
      )

    }
  )
}


# demo --------------------------------------------------------------------

#' @rdname mod_pre_lease_summary
#' @export
#' @importFrom pkgload load_all
#' @importFrom bslib page_fluid bs_theme
#' @importFrom shiny shinyApp
mod_pre_lease_summary_demo <- function() {

  pkgload::load_all()

  ui <- bslib::page_fluid(
    title = "Demo",
    theme = bslib::bs_theme(version = 5),
    lang = "en",
    mod_pre_lease_summary_ui("demo")
  )

  server <- function(input, output, session) {
    mod_pre_lease_summary_server("demo")
  }

  shiny::shinyApp(ui, server)
}
