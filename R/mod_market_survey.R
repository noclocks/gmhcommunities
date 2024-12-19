
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
#' A Shiny Module for the GMH Communities Leasing Market Survey.
#'
#' The following functions are implemented:
#'
#' - `mod_market_survey_ui()`: User interface (UI) definition
#' - `mod_market_survey_server()`: Server logic for the UI
#' - `mod_market_survey_demo()`: Demo application for the module
#'
#' @param id Module's namespace ID.
#' @param pool Database connection pool.
#' @param selected_property Selected property ID.
#' @param selected_leasing_week Selected leasing week.
#'
#' @returns
#' - `mod_market_survey_ui()`: UI HTML Output.
#' - `mod_market_survey_server()`: List of reactive values.
#' - `mod_market_survey_demo()`: `NULL`, used for the side-effect of a demo app.
#'
#' @seealso
#' Market Survey Section Modules:
#'
#' - [mod_market_survey_property_summary()]
#' - [mod_market_survey_leasing_summary()]
#' - [mod_market_survey_short_term_leases()]
#' - [mod_market_survey_fees()]
#' - [mod_market_survey_amenities()]
#' - [mod_market_survey_parking()]
#' - [mod_market_survey_utilities()]
#' - [mod_market_survey_notes()]
#' - [mod_market_survey_rents()]
#'
#' @examplesIf interactive()
#' mod_market_survey_demo()
NULL


# UI ----------------------------------------------------------------------

#' @rdname mod_market_survey
#' @export
#' @importFrom shiny NS
#' @importFrom htmltools tagList tags
mod_market_survey_ui <- function(id) {

  ns <- shiny::NS(id)

  htmltools::tagList(
    # Value Boxes -------------------------------------------------------------
    bslib::layout_columns(
      bslib::value_box(
        id = ns("properties_value_box"),
        title = "Properties",
        value = shiny::textOutput(ns("properties_count")),
        showcase = bsicons::bs_icon("building")
      ),
      bslib::value_box(
        id = ns("competitors_value_box"),
        title = "Competitors",
        value = shiny::textOutput("competitor_count"),
        showcase = bsicons::bs_icon("graph-up")
      ),
      bslib::value_box(
        id = ns("surveys_value_box"),
        title = "Survey Responses",
        value = shiny::textOutput("response_count"),
        showcase = bsicons::bs_icon("clipboard-data")
      )
    ),

    # progress ----------------------------------------------------------------
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
      ),
      bslib::nav_panel(
        title = "Rents",
        value = ns("rents"),
        mod_market_survey_rents_ui(ns("rents"))
      )
    )
  )
}


# server ------------------------------------------------------------------

#' @rdname mod_market_survey
#' @export
#' @importFrom shiny moduleServer
#' @importFrom cli cat_rule
mod_market_survey_server <- function(
  id,
  pool,
  selected_property = NULL,
  selected_leasing_week = NULL
) {

  # check database connection
  check_db_conn(pool)

  # validation of reactives
  if (!is.null(selected_property)) {
    stopifnot(shiny::is.reactive(selected_property))
  }

  if (!is.null(selected_leasing_week)) {
    stopifnot(shiny::is.reactive(selected_leasing_week))
  }

  # default property (commonwealth)
  if (is.null(selected_property)) {
    selected_property <- shiny::reactive({"739085"})
  }

  # default leasing week (current week)
  if (is.null(selected_leasing_week)) {
    selected_leasing_week <- shiny::reactive({ get_weekly_period_start_date() })
  } else {
    selected_leasing_week <- shiny::reactive({ get_weekly_period_start_date(selected_leasing_week()) })
  }

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




  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
      cli::cat_rule("[Module]: mod_market_survey_server()")

      # reactive values ---------------------------------------------------------
      db_metrics <- shiny::reactivePoll(
        intervalMillis = 30000,
        session = session,
        checkFunc = function() { Sys.time() },
        valueFunc = function() { get_survey_metrics(pool) }
      )

      # value boxes -------------------------------------------------------------

      output$properties_count <- shiny::renderText({
        db_metrics()$total_properties
      })

      output$competitor_count <- shiny::renderText({
        db_metrics()$total_competitors
      })

      output$response_count <- shiny::renderText({
        db_metrics()$total_responses
      })

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


# utilities ---------------------------------------------------------------

get_survey_metrics <- function(conn) {

  if (inherits("Pool", conn)) {
    conn <- pool::poolCheckout(conn)
    on.exit(pool::poolReturn(conn), add = TRUE)
  }

  tryCatch({

    total_properties_qry <- glue::glue_sql(
      "SELECT COUNT(*) AS count FROM {`schema`}.{`tbl`}",
      schema = "mkt",
      tbl = "properties",
      .con = conn
    )

    total_competitors_qry <- glue::glue_sql(
      "SELECT COUNT(*) AS count FROM {`schema`}.{`tbl`}",
      schema = "mkt",
      tbl = "competitors",
      .con = conn
    )

    total_surveys_qry <- glue::glue_sql(
      "SELECT COUNT(*) AS count FROM {`schema`}.{`tbl`}",
      schema = "mkt",
      tbl = "surveys",
      .con = conn
    )

    total_responses_qry <- glue::glue_sql(
      "SELECT COUNT(*) AS count FROM {`schema`}.{`tbl`}",
      schema = "mkt",
      tbl = "responses",
      .con = conn
    )

    total_properties <- DBI::dbGetQuery(conn, total_properties_qry) |>
      dplyr::pull("count") |>
      purrr::pluck(1) |>
      as.integer()

    total_competitors <- DBI::dbGetQuery(conn, total_competitors_qry) |>
      dplyr::pull("count") |>
      purrr::pluck(1) |>
      as.integer()

    total_surveys <- DBI::dbGetQuery(conn, total_surveys_qry) |>
      dplyr::pull("count") |>
      purrr::pluck(1) |>
      as.integer()

    total_responses <- DBI::dbGetQuery(conn, total_responses_qry) |>
      dplyr::pull("count") |>
      purrr::pluck(1) |>
      as.integer()

    cli::cli_alert_success("Successfully retrieved survey metrics from database.")

    return(
      list(
        total_properties = total_properties,
        total_competitors = total_competitors,
        total_surveys = total_surveys,
        total_responses = total_responses
      )
    )

  }, error = function(e) {

    cli::cli_alert_danger(
      c(
        "Failed to retrieve survey metrics from database.\n",
        "Error: {.error {conditionMessage(e)}}"
      )
    )

    return(
      list(
        total_properties = 0,
        total_competitors = 0,
        total_surveys = 0,
        total_responses = 0
      )
    )

  })

}

