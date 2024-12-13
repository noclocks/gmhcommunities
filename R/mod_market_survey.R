
#  ------------------------------------------------------------------------
#
# Title : Market Survey Shiny Module
#    By : Jimmy Briggs
#  Date : 2024-11-27
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' Market Survey Shiny Module
#'
#' @name mod_market_survey
#'
#' @description
#' A Shiny Module for ...
#'
#' - `mod_market_survey_ui()`: User interface
#' - `mod_market_survey_server()`: Server logic
#'
#' @param id Module's namespace ID.
#'
#' @return
#' - `mod_market_survey_ui()`: UI HTML Output.
#' - `mod_market_survey_server()`: Reactive values returned from server logic.
#'
#' @examples
#' if (interactive()) {
#'   mod_market_survey_demo()
#' }
NULL


# UI ----------------------------------------------------------------------

#' @rdname mod_market_survey
#' @export
#' @importFrom shiny NS
#' @importFrom htmltools tagList tags
mod_market_survey_ui <- function(id) {

  ns <- shiny::NS(id)

  htmltools::tagList(
    bslib::layout_column_wrap(
      width = 1/3,
      bslib::value_box(
        title = "Properties",
        value = shiny::textOutput(ns("valbox_properties")),
        showcase = bsicons::bs_icon("building")
      ),
      bslib::value_box(
        title = "Competitors",
        value = shiny::textOutput(ns("valbox_competitors")),
        showcase = bsicons::bs_icon("graph-up")
      ),
      bslib::value_box(
        title = "Survey Responses",
        value = shiny::textOutput(ns("valbox_responses")),
        showcase = bsicons::bs_icon("clipboard-data")
      )
    ),
    bslib::layout_column_wrap(
      width = 1,
      bslib::card(
        bslib::card_header(icon_text("percent", "Progress")),
        bslib::card_body(
          shinyWidgets::progressBar(
            ns("total_progress"),
            value = 0,
            total = 100,
            display_pct = TRUE,
            striped = TRUE,
            title = "Total Progress"
          ),
          shinyWidgets::progressBar(
            ns("section_progress"),
            value = 0,
            total = 100,
            display_pct = TRUE,
            striped = TRUE,
            title = "Section Progress"
          )
        )
      )
    ),
    bslib::navset_card_tab(
      id = ns("survey_tabs"),
      title = "GMH Communities - Leasing Market Survey Sections",
      sidebar = bslib::sidebar(
        title = icon_text("funnel", "Filters"),
        width = 300,
        shinyWidgets::pickerInput(
          ns("property"),
          label = icon_text("building", "Select Property"),
          choices = app_choices$properties,
          selected = app_choices$properties[[1]]
        ),
        leasing_week_input(
          ns("leasing_week"),
          label = icon_text("calendar-alt", "Select Leasing Week"),
          inline = TRUE
        )
      ),
      bslib::nav_panel(
        title = "Property Summary",
        value = ns("property_summary"),
        mod_market_survey_property_summary_ui(ns("property_summary"))
      ),
      bslib::nav_panel(
        title = "Leasing Summary",
        value = ns("leasing_summary"),
        mod_market_survey_leasing_summary_ui(ns("leasing_summary"))
      ),
      bslib::nav_panel(
        title = "Short Term Leases",
        value = ns("short_term_leases"),
        mod_market_survey_short_term_leases_ui(ns("short_term_leases"))
      ),
      bslib::nav_panel(
        title = "Fees",
        value = ns("fees"),
        mod_market_survey_fees_ui(ns("fees"))
      ),
      bslib::nav_panel(
        title = "Amenities",
        value = ns("amenities"),
        mod_market_survey_amenities_ui(ns("amenities"))
      ),
      bslib::nav_panel(
        title = "Parking",
        value = ns("parking"),
        mod_market_survey_parking_ui(ns("parking"))
      ),
      bslib::nav_panel(
        title = "Utilities",
        value = ns("utilities"),
        mod_market_survey_utilities_ui(ns("utilities"))
      ),
      bslib::nav_panel(
        title = "Notes",
        value = ns("notes"),
        mod_market_survey_notes_ui(ns("notes"))
      )
    )
  )
}


# server ------------------------------------------------------------------

#' @rdname mod_market_survey
#' @export
#' @importFrom shiny moduleServer
#' @importFrom cli cat_rule
mod_market_survey_server <- function(id, pool = NULL, selected_property = NULL, selected_leasing_week = NULL) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
      cli::cat_rule("[Module]: mod_market_survey_server()")

      # selected property -------------------------------------------------------
      if (is.null(selected_property)) {
        selected_property <- shiny::reactive({"739085"})
      }

      shiny::observe({
        prop <- selected_property()
        cli::cli_alert_info(
          "Selected Property: {.field {prop}}"
        )
      })

      # selected leasing week ---------------------------------------------------
      if (is.null(selected_leasing_week)) {
        selected_leasing_week <- shiny::reactive({
          get_weekly_period_start_date()
        })
      } else {
        selected_leasing_week <- shiny::reactive({
          get_weekly_period_start_date(selected_leasing_week())
        })
      }

      # reactive values ---------------------------------------------------------
      db_data <- shiny::reactiveVal(NULL)
      app_data <- shiny::reactiveVal(NULL)
      changes <- shiny::reactiveVal(NULL)
      trigger_refresh <- shiny::reactiveVal(FALSE)

      survey_section_data <- shiny::reactive({



      })

      return(
        list(

        )
      )

    }
  )
}


# demo --------------------------------------------------------------------

#' @rdname mod_market_survey
#' @export
#' @importFrom pkgload load_all
#' @importFrom bslib page_fluid bs_theme
#' @importFrom shiny shinyApp
mod_market_survey_demo <- function() {

  pkgload::load_all()

  ui <- bslib::page_navbar(
    title = "Demo",
    theme = bslib::bs_theme(version = 5),
    lang = "en",
    window_title = "Demo",
    bslib::nav_panel(
      title = "Market Survey",
      value = "market_survey",
      icon = bsicons::bs_icon("file-earmark-bar-graph"),
      mod_market_survey_ui("demo")
    )
  )

  server <- function(input, output, session) {
    mod_market_survey_server("demo")
  }

  shiny::shinyApp(ui, server)
}
