
#  ------------------------------------------------------------------------
#
# Title : Dashboard Shiny Module
#    By : Jimmy Briggs
#  Date : 2024-11-27
#
#  ------------------------------------------------------------------------


# topic -------------------------------------------------------------------

#' Dashboard Shiny Module
#'
#' @name mod_dashboard
#'
#' @description
#' dashboard Shiny Module
#'
#' - `mod_dashboard_ui()`: User interface
#' - `mod_dashboard_server()`: Server logic
#'
#' @param id shiny module id
#'
#' @import shiny
NULL


# UI ----------------------------------------------------------------------

#' @rdname mod_dashboard
#' @export
#' @importFrom shiny NS
#' @importFrom htmltools tagList
mod_dashboard_ui <- function(id) {

  ns <- shiny::NS(id)

  htmltools::tagList(
    bslib::layout_column_wrap(
      width = 1/3,
      heights_equal = "all",
      bslib::value_box(
        id = ns("valbox_properties"),
        title = "Properties",
        value = shiny::textOutput(ns("valbox_properties_value"), inline = TRUE),
        showcase = bsicons::bs_icon("building-check"),
        theme = "info"#,
        # shiny::uiOutput(ns("valbox_properties_details"))
      ),
      bslib::value_box(
        id = ns("valbox_leases"),
        title = "Leases",
        value = shiny::textOutput(ns("valbox_leases_value"), inline = TRUE),
        showcase = bsicons::bs_icon("file-earmark-text-fill"),
        theme = "success"#,
        # shiny::uiOutput(ns("valbox_leases_details"))
      ),
      bslib::value_box(
        id = ns("valbox_rates"),
        title = "Occupancy",
        value = shiny::textOutput(ns("valbox_rates_value"), inline = TRUE),
        showcase = bsicons::bs_icon("house-check-fill"),
        theme = "primary"#,
        # shiny::uiOutput(ns("valbox_rates_details"))
      )
    ),
    bslib::layout_columns(
      bslib::card(
        bslib::card_header(
          bsicons::bs_icon("graph-up"),
          "Key Metrics"
        ),
        bslib::card_body(
          htmltools::tags$p(
            "This is where key metrics and additional information will go."
          )
        ),
        bslib::card_footer(
          htmltools::tags$small(
            "Last updated: ", shiny::textOutput(ns("last_updated"))
          )
        )
      ),
      bslib::card(
        bslib::card_header(
          bsicons::bs_icon("clock-history"),
          "Recent Activity"
        ),
        bslib::card_body(
          htmltools::tags$p(
            "This is where recent activity or updates will go."
          )
        )
      )
    )
  )
}


# server ------------------------------------------------------------------

#' @rdname mod_dashboard
#' @export
#' @importFrom shiny moduleServer
#' @importFrom cli cat_rule
mod_dashboard_server <- function(id) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      cli::cat_rule("mod_dashboard_server")

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
mod_dashboard_demo <- function() {

  ui <- bslib::page_fillable(
    theme = bslib::bs_theme(version = 5),
    title = "Demo",
    mod_dashboard_ui("demo")
  )

  server <- function(input, output, session) {
    mod_dashboard_server("demo")
  }

  shiny::shinyApp(ui, server)
}
