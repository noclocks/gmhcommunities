
#  ------------------------------------------------------------------------
#
# Title : App Sidebar Module
#    By : Jimmy Briggs
#  Date : 2024-11-08
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' App Sidebar Module
#'
#' @name mod_sidebar
#'
#' @description
#' This module creates a sidebar for the app that contains filters, options, and about information.
#'
#' It includes the UI and server functions:
#'
#' - `mod_sidebar_ui()`
#' - `mod_sidebar_server()`
#'
#' @param id The module id.
#' @param ... Additional parameters.
#'
#' @return
#' - `mod_sidebar_ui()`: A shiny UI function.
#' - `mod_sidebar_server()`: A shiny server function.
NULL

# UI ----------------------------------------------------------------------

#' @rdname mod_sidebar
#' @export
#' @importFrom bsicons bs_icon
#' @importFrom bslib sidebar accordion accordion_panel
#' @importFrom htmltools tags
#' @importFrom shiny NS
#' @importFrom shinyWidgets pickerInput
mod_sidebar_ui <- function(id) {

  ns <- shiny::NS(id)

  bslib::sidebar(
    id = ns("sidebar"),
    width = 350,
    position = "left",
    open = TRUE,
    title = "GMH DataHub",
    bslib::input_task_button(
      ns("entrata_refresh"),
      label = "Refresh Data",
      icon = shiny::icon("refresh")
    ),
    mod_sidebar_filters_ui(ns("filters"))
  )

}

# server ------------------------------------------------------------------

#' @rdname mod_sidebar
#' @export
#' @importFrom shiny moduleServer reactive req
mod_sidebar_server <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      cli::cli_alert_info("mod_sidebar_server: Initializing...")

      # modules
      filters <- mod_sidebar_filters_server("filters")

      # return
      return(list(filters = filters))

    }
  )
}


# sidebar_user_profile ----------------------------------------------------



# filters -----------------------------------------------------------------

#' @rdname mod_sidebar
#' @export
#' @importFrom bsicons bs_icon
#' @importFrom bslib accordion accordion_panel
#' @importFrom htmltools tags
#' @importFrom lubridate floor_date days
#' @importFrom shiny NS
#' @importFrom shinyWidgets pickerInput airDatepickerInput
#' @importFrom rlang !!!
mod_sidebar_filters_ui <- function(id) {

  ns <- shiny::NS(id)

  current_week_start <- get_weekly_period_start_date()
  current_week_end <- get_weekly_period_end_date()
  initial_value <- c(current_week_start, current_week_end)

  filters <- list(
    shinyWidgets::pickerInput(
      ns("portfolio"),
      label = icon_text("building", "Portfolio", .function = shiny::icon),
      choices = app_choices$portfolios,
      selected = app_choices$portfolios,
      multiple = TRUE,
      options = shinyWidgets::pickerOptions(
        actionsBox = TRUE
      )
    ),
    shinyWidgets::pickerInput(
      ns("properties"),
      label = icon_text("building", "Properties", .function = shiny::icon),
      choices = app_choices$properties,
      selected = app_choices$properties,
      multiple = TRUE,
      options = shinyWidgets::pickerOptions(
        actionsBox = TRUE,
        header = "Select Properties",
        liveSearch = TRUE
      )
    ),
    shinyWidgets::airDatepickerInput(
      ns("leasing_week"),
      label = icon_text("calendar-alt", "Leasing Week", .function = shiny::icon),
      value = initial_value,
      range = TRUE,
      firstDay = 1,
      autoClose = TRUE,
      todayButton = TRUE,
      disabledDaysOfWeek = c(2, 3, 4, 5, 6),
      maxDate = Sys.Date(),
      addon = "none"
    )
  )

  bslib::accordion(
    bslib::accordion_panel(
      "Filters",
      icon = bsicons::bs_icon("filter"),
      !!!filters
    )
  )
}

#' @rdname mod_sidebar
#' @export
#' @importFrom dplyr filter select
#' @importFrom lubridate floor_date days
#' @importFrom shiny moduleServer reactive req bindEvent reactiveValues
#' @importFrom shinyWidgets updatePickerInput updateAirDateInput
#' @importFrom cli cli_alert_info
mod_sidebar_filters_server <- function(id) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      # reactive values for filters
      filters <- shiny::reactiveValues(
        portfolios = NULL,
        properties = NULL,
        leasing_week = NULL
      )

      data("portfolio_assignments", package = "gmhcommunities")

      # update properties based on selected portfolio
      shiny::observeEvent(input$portfolio, {
        shiny::req(input$portfolio)

        selected_properties_df <- portfolio_assignments |>
          dplyr::filter(.data$portfolio_id %in% input$portfolio) |>
          dplyr::select(c("property_id", "property_name"))

        selected_properties <- c(selected_properties_df$property_id)
        names(selected_properties) <- selected_properties_df$property_name

        shinyWidgets::updatePickerInput(
          session = session,
          inputId = "properties",
          selected = selected_properties,
          choices = selected_properties
        )

        cli::cli_alert_info(
          c(
            "Properties have been updated based on the selected portfolio.",
            "Selected Portfolio: {.field {input$portfolio}}",
            "Portfolio's Properties: {.field {selected_properties}}"
          )
        )
      }, ignoreInit = TRUE, ignoreNULL = TRUE)

      # adjust leasing week to weekly period
      shiny::observeEvent(input$leasing_week, {

        shiny::req(input$leasing_week)

        start_date <- input$leasing_week[1]
        end_date <- input$leasing_week[2]

        leasing_period_start_date <- get_weekly_period_start_date(start_date)
        leasing_period_end_date <- get_weekly_period_end_date(start_date)

        if (start_date != leasing_period_start_date || end_date != leasing_period_end_date) {

          shinyWidgets::updateAirDateInput(
            session = session,
            inputId = "leasing_week",
            value = c(leasing_period_start_date, leasing_period_end_date)
          )

          cli::cli_alert_info(
            c(
              "Selected week has been adjusted to the leasing week period of the selected date range.",
              "Selected week: {.field {start_date}} - {.field {end_date}}",
              "Adjusted week: {.field {leasing_period_start_date}} - {.field {leasing_period_end_date}}"
            )
          )
        }

      }, ignoreInit = FALSE, ignoreNULL = TRUE)

      # update reactive values
      shiny::observe({
        shiny::req(input$portfolio, input$properties, input$leasing_week)
        filters$portfolios <- input$portfolio
        filters$properties <- input$properties
        filters$leasing_week <- input$leasing_week
      }) |>
        shiny::bindEvent(input$portfolio, input$properties, input$leasing_week)

      # return
      return(filters)
    }
  )
}


# demo --------------------------------------------------------------------

#' @rdname mod_sidebar
#' @keywords internal
#' @importFrom bslib page_navbar bs_theme nav_panel
#' @importFrom shiny icon shinyApp
mod_sidebar_demo <- function() {

  ui <- bslib::page_navbar(
    theme = bslib::bs_theme(version = 5),
    sidebar = mod_sidebar_ui("app"),
    title = "Sidebar Module Demo",
    window_title = "Sidebar Module Demo",
    bslib::nav_panel(
      "Demo",
      icon = shiny::icon("home"),
      bslib::card()
    )
  )

  server <- function(input, output, session) {
    sidebar_data <- mod_sidebar_server("app")
  }

  shiny::shinyApp(ui, server)

}
