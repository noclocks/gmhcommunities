
#  ------------------------------------------------------------------------
#
# Title : market_survey_hours Shiny Module
#    By : Jimmy Briggs
#  Date : 2024-11-27
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' market_survey_hours Shiny Module
#'
#' @name mod_market_survey_hours
#'
#' @description
#' A Shiny Module for ...
#'
#' - `mod_market_survey_hours_ui()`: User interface
#' - `mod_market_survey_hours_server()`: Server logic
#'
#' @param id Module's namespace ID.
#'
#' @return
#' - `mod_market_survey_hours_ui()`: UI HTML Output.
#' - `mod_market_survey_hours_server()`: Reactive values returned from server logic.
#'
#' @examples
#' if (interactive()) {
#'   mod_market_survey_hours_demo()
#' }
NULL


# UI ----------------------------------------------------------------------

#' @rdname mod_market_survey_hours
#' @export
#' @importFrom shiny NS
#' @importFrom htmltools tagList tags
mod_market_survey_hours_ui <- function(id) {

  ns <- shiny::NS(id)

  htmltools::tagList(
    bslib::card(
      bslib::card_header(icon_text("calendar-alt", "Market Survey - Property Hours")),
      bslib::card_body(
        rhandsontable::rHandsontableOutput(ns("hours_table")) |>
          with_loader()
      )
    )
  )
}


# server ------------------------------------------------------------------

#' @rdname mod_market_survey_hours
#' @export
#' @importFrom shiny moduleServer
#' @importFrom cli cat_rule
mod_market_survey_hours_server <- function(
    id,
    pool,
    selected_property = NULL,
    selected_leasing_week = NULL
) {

  # default property (commonwealth)
  if (is.null(selected_property)) {
    selected_property <- shiny::reactive({"739085"})
  }

  # default leasing week (current week)
  if (is.null(selected_leasing_week)) {
    selected_leasing_week <- shiny::reactive({
      get_weekly_period_start_date(as_of_date = lubridate::today() - lubridate::days(6))
    })
  }

  # validation
  stopifnot(shiny::is.reactive(selected_property))
  stopifnot(shiny::is.reactive(selected_leasing_week))
  check_db_conn(pool)

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
      cli::cat_rule("[Module]: mod_market_survey_hours_server()")

      # selected property -------------------------------------------------------
      shiny::observe({
        prop <- selected_property()
        week <- selected_leasing_week()
        cli::cli_alert_info(
          c(
            "Selected Property: {.field {prop}}\n",
            "Selected Leasing Week: {.field {week}}\n"
          )
        )
      })

      hours_data <- reactive({
        data.frame(
          Day = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
          Open = c("9:00 AM", "9:00 AM", "9:00 AM", "9:00 AM", "9:00 AM", "10:00 AM", "12:00 PM"),
          Close = c("6:00 PM", "6:00 PM", "6:00 PM", "6:00 PM", "6:00 PM", "5:00 PM", "4:00 PM"),
          stringsAsFactors = FALSE
        )
      })

      output$hours_table <- renderRHandsontable({
        rhandsontable(hours_data())
      })



    }
  )
}


# demo --------------------------------------------------------------------

#' @rdname mod_market_survey_hours
#' @export
#' @importFrom pkgload load_all
#' @importFrom bslib page_fluid bs_theme
#' @importFrom shiny shinyApp
mod_market_survey_hours_demo <- function() {

  pkgload::load_all()

  pool <- db_connect()

  ui <- bslib::page_fluid(
    title = "Demo",
    theme = app_theme(),
    lang = "en",
    mod_market_survey_hours_ui("demo")
  )

  server <- function(input, output, session) {
    mod_market_survey_hours_server("demo", pool)
  }

  shiny::shinyApp(ui, server)
}


db_get_office_hours <- function(conn, property_id) {

  check_db_conn(conn)

  dplyr::tbl(conn, I("mkt.office_hours")) |>
    dplyr::filter(property_id == property_id) |>
    dplyr::collect()

}
