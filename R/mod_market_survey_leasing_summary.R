
#  ------------------------------------------------------------------------
#
# Title : market_survey_leasing_summary Shiny Module
#    By : Jimmy Briggs
#  Date : 2024-11-27
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' market_survey_leasing_summary Shiny Module
#'
#' @name mod_market_survey_leasing_summary
#'
#' @description
#' A Shiny Module for ...
#'
#' - `mod_market_survey_leasing_summary_ui()`: User interface
#' - `mod_market_survey_leasing_summary_server()`: Server logic
#'
#' @param id Module's namespace ID.
#'
#' @return
#' - `mod_market_survey_leasing_summary_ui()`: UI HTML Output.
#' - `mod_market_survey_leasing_summary_server()`: Reactive values returned from server logic.
#'
#' @examples
#' if (interactive()) {
#'   mod_market_survey_leasing_summary_demo()
#' }
NULL


# UI ----------------------------------------------------------------------

#' @rdname mod_market_survey_leasing_summary
#' @export
#' @importFrom shiny NS
#' @importFrom htmltools tagList tags
mod_market_survey_leasing_summary_ui <- function(id) {

  ns <- shiny::NS(id)

  htmltools::tagList(
    # htmltools::tags$head(
    #   shinyjs::useShinyjs(),
    #   waiter::use_waiter(),
    #   shinyWidgets::useSweetAlert(),
    #   waiter::waiterShowOnLoad(waiter::spin_fading_circles())
    # ),
    bslib::card(
      bslib::card_header(
        htmltools::tags$span(
          bsicons::bs_icon("file-earmark-text"),
          "Market Survey - Leasing Summary: ",
          shiny::textOutput(ns("selected_property_title"), inline = TRUE)
        ),
        class = "bg-dark text-white"
      ),
      bslib::card_body(
        bslib::layout_column_wrap(
          width = 1/3,
          bslib::card(
            bslib::card_header(
              icon_text("rocket", "Launch Dates:")
            ),
            shinyWidgets::airDatepickerInput(
              ns("lease_launch_date"),
              label = "Lease Launch Date",
              value = NULL,
              placeholder = "Select Lease Launch Date"
            ) |>
              with_tooltip(
                "Select the Lease Launch Date Selected Property's Current Leasing Period"
              ),
            shinyWidgets::airDatepickerInput(
              ns("renewal_launch_date"),
              label = "Renewal Launch Date",
              value = NULL,
              placeholder = "Select Renewal Launch Date"
            ) |>
              with_tooltip(
                "Select the Renewal Launch Date Selected Property's Current Leasing Period"
              )
          ),
          bslib::card(
            bslib::card_header(
              icon_text("chart-line", "Occupancy & Pre-Lease:")
            ),
            bslib::layout_column_wrap(
              width = 1/2,
              bslib::card(
                bslib::card_header("Occupancy"),
                shinyWidgets::numericInputIcon(
                  ns("current_occupancy"),
                  label = "Current Occupancy (%)",
                  min = 0,
                  max = 100,
                  step = 1,
                  icon = shiny::icon("percent"),
                  help_text = "Value must be between 0 and 100",
                  value = 95
                ) |>
                  with_tooltip(
                    "Enter the Current Occupancy Percentage for the Selected Property"
                  ),
                shinyWidgets::numericInputIcon(
                  ns("prior_occupancy"),
                  label = "Prior Occupancy (%)",
                  min = 0,
                  max = 100,
                  step = 1,
                  icon = shiny::icon("percent"),
                  help_text = "Value must be between 0 and 100",
                  value = 93
                ) |>
                  with_tooltip(
                    "Enter the Prior Occupancy Percentage for the Selected Property"
                  ) |>
                  shinyjs::disabled()
              ),
              bslib::card(
                bslib::card_header("Pre-Lease"),
                shinyWidgets::numericInputIcon(
                  ns("current_prelease"),
                  label = "Current Pre-Lease (%)",
                  min = 0,
                  max = 100,
                  step = 1,
                  icon = shiny::icon("percent"),
                  help_text = "Value must be between 0 and 100",
                  value = 45
                ) |>
                  with_tooltip(
                    "Enter the Current Pre-Lease Percentage for the Selected Property"
                  ),
                shinyWidgets::numericInputIcon(
                  ns("prior_prelease"),
                  label = "Prior Pre-Lease (%)",
                  min = 0,
                  max = 100,
                  step = 1,
                  icon = shiny::icon("percent"),
                  help_text = "Value must be between 0 and 100",
                  value = 40
                ) |>
                  with_tooltip(
                    "Enter the Prior Pre-Lease Percentage for the Selected Property"
                  ) |>
                  shinyjs::disabled()
              )
            )
          ),
          bslib::card(
            bslib::card_header("Leasing Activity"),
            shinyWidgets::numericInputIcon(
              ns("total_renewals"),
              label = "Total Renewals",
              value = 50,
              icon = shiny::icon("sync"),
              min = 0,
              step = 1
            ) |>
              with_tooltip(
                "Enter the Total Number of Renewals for the Selected Property"
              ),
            shinyWidgets::numericInputIcon(
              ns("total_new_leases"),
              label = "Total New Leases",
              value = 25,
              icon = shiny::icon("plus"),
              min = 0,
              step = 1
            ) |>
              with_tooltip(
                "Enter the Total Number of New Leases for the Selected Property"
              ),
            shinyWidgets::numericInputIcon(
              ns("total_weekly_leases"),
              label = "Total Weekly Leases",
              value = 0,
              icon = shiny::icon("calendar-alt"),
              min = 0,
              step = 1
            ),
            shinyWidgets::numericInputIcon(
              ns("total_weekly_traffic"),
              label = "Total Weekly Traffic",
              value = 25,
              icon = shiny::icon("users"),
              min = 0,
              step = 1
            ) |>
              with_tooltip(
                "Enter the Total Weekly Traffic for the Selected Property"
              ),
            shiny::selectInput(
              ns("current_incentive"),
              label = "Current Incentive",
              choices = c("None", "Gift Card", "Monthly Concession", "One-Time Concession"),
              selected = "None",
              multiple = FALSE
            ),
            shinyWidgets::numericInputIcon(
              ns("incentive_amount"),
              label = "Incentive Amount",
              value = 0,
              icon = shiny::icon("dollar-sign"),
              min = 0,
              step = 1
            ) |>
              with_tooltip(
                "Enter the Incentive Amount for the Selected Property"
              )
          )
        ),
        htmltools::tags$div(
          class = "text-center",
          width = "33%",
          bslib::card(
            shinyWidgets::progressBar(
              ns("progress"),
              value = 0,
              display_pct = TRUE,
              size = "sm",
              status = "danger",
              striped = TRUE,
              title = "Progress"
            )
          )
        ),
        htmltools::tags$div(
          class = "text-center",
          shiny::textOutput(ns("last_updated_text")),
          htmltools::tags$br(),
          htmltools::tags$span(
            bslib::input_task_button(
              ns("save"),
              "Save",
              icon = shiny::icon("save"),
              class = "btn btn-success"
            ) |>
              shinyjs::disabled(),
            bslib::input_task_button(
              ns("refresh"),
              "Refresh",
              icon = shiny::icon("sync"),
              class = "btn btn-primary"
            )
          )
        )
      ),
      bslib::card_footer(
        shiny::actionButton(ns("back"), "Back", class = "btn btn-primary", style = "float: left"),
        shiny::actionButton(ns("next"), "Next", class = "btn btn-primary", style = "float: right")
      )
    )
  )
}


# server ------------------------------------------------------------------

#' @rdname mod_market_survey_leasing_summary
#' @export
#' @importFrom shiny moduleServer
#' @importFrom cli cat_rule
mod_market_survey_leasing_summary_server <- function(
  id,
  selected_property = NULL,
  pool = NULL
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
      cli::cat_rule("[Module]: mod_market_survey_leasing_summary_server()")

      # selected_property -------------------------------------------------------
      if (is.null(selected_property)) {
        selected_property <- shiny::reactive({"739085"})
      }
      shiny::observe({
        cli::cli_alert_info("Selected Property: {.field {selected_property()}}")
      })

      # cache -------------------------------------------------------------------
      local_cache <- shiny::reactiveVal(NULL)

      shiny::observeEvent(initial_data(), {
        local_cache(initial_data())
      }, priority = 1000)

      # reactive values ---------------------------------------------------------
      input_data <- shiny::reactiveVal(list())

      app_state <- shiny::reactiveValues(
        app_data = NULL,
        db_data = NULL,
        last_updated = NULL,
        changes = list()
      )

      # validation --------------------------------------------------------------
      iv <- shinyvalidate::InputValidator$new()
      iv$add_rule("current_occupancy", shinyvalidate::sv_required())
      iv$add_rule("current_occupancy", shinyvalidate::sv_between(0, 100))

      # get initial data --------------------------------------------------------
      initial_data <- shiny::reactive({
        shiny::withProgress(
          message = "Populating inputs with latest data...",
          {
            tryCatch({

              prop_id <- selected_property()

              dplyr::tbl(
                src = pool,
                I("mkt.leasing_summary")
              ) |>
                dplyr::filter(
                  .data$property_id == .env$prop_id
                ) |>
                dplyr::collect()

            }, error = function(e) {
              cli::cli_alert_danger("Failed to retrieve leasing summary data from database.")
              cli::cli_alert_danger("{.error {e$message}}")
              shiny::showNotification("Failed to load leasing summary data from the database",
                                      type = "error")
              NULL
            })
          }
        )
      })

      shiny::observe({
        app_state$db_data <- initial_data()
        app_state$last_updated <- Sys.time()
      })

      # populate inputs ---------------------------------------------------------
      shiny::observe({

        if (!is.null(app_state$db_data)) {

          # Lease Launch Date
          shinyWidgets::updateAirDateInput(
            session = session,
            inputId = "lease_launch_date",
            value = app_state$db_data$lease_launch_date
          )

          # Renewal Launch Date
          shinyWidgets::updateAirDateInput(
            session = session,
            inputId = "renewal_launch_date",
            value = app_state$db_data$renewal_launch_date
          )

          # Current Occupancy
          shinyWidgets::updateNumericInputIcon(
            session = session,
            inputId = "current_occupancy",
            value = app_state$db_data$current_occupancy
          )

          # Prior Occupancy
          shinyWidgets::updateNumericInputIcon(
            session = session,
            inputId = "prior_occupancy",
            value = app_state$db_data$prior_occupancy
          )

          # Current Pre-Lease
          shinyWidgets::updateNumericInputIcon(
            session = session,
            inputId = "current_prelease",
            value = app_state$db_data$current_prelease
          )

          # Prior Pre-Lease
          shinyWidgets::updateNumericInputIcon(
            session = session,
            inputId = "prior_prelease",
            value = app_state$db_data$prior_prelease
          )

          # Total Renewals
          shinyWidgets::updateNumericInputIcon(
            session = session,
            inputId = "total_renewals",
            value = app_state$db_data$total_renewals
          )

          # Total New Leases
          shinyWidgets::updateNumericInputIcon(
            session = session,
            inputId = "total_new_leases",
            value = app_state$db_data$total_new_leases
          )

          # Total Weekly Leases
          shinyWidgets::updateNumericInputIcon(
            session = session,
            inputId = "total_weekly_leases",
            value = app_state$db_data$total_weekly_leases
          )

          # Total Weekly Traffic
          shinyWidgets::updateNumericInputIcon(
            session = session,
            inputId = "total_weekly_traffic",
            value = app_state$db_data$total_weekly_traffic
          )

          # Current Incentive
          shiny::updateSelectInput(
            session = session,
            inputId = "current_incentive",
            selected = app_state$db_data$current_incentive
          )

          # Incentive Amount
          shinyWidgets::updateNumericInputIcon(
            session = session,
            inputId = "incentive_amount",
            value = app_state$db_data$incentive_amount
          )

        }

      })


      # observers -------------------------------------------------------------

      # observe changes in inputs and update input_data
      shiny::observe({
        shiny::req(input_data)
        input_data(
          list(
            lease_launch_date = input$lease_launch_date,
            renewal_launch_date = input$renewal_launch_date,
            current_occupancy = input$current_occupancy,
            prior_occupancy = input$prior_occupancy,
            current_prelease = input$current_prelease,
            prior_prelease = input$prior_prelease,
            total_renewals = input$total_renewals,
            total_new_leases = input$total_new_leases,
            total_weekly_leases = input$total_weekly_leases,
            total_weekly_traffic = input$total_weekly_traffic,
            current_incentive = input$current_incentive,
            incentive_amount = input$incentive_amount
          )
        )
      })







      return(
        list(
          input_data = input_data,
          is_valid = shiny::reactive({iv$is_valid()})
        )
      )

    }
  )
}


# demo --------------------------------------------------------------------

#' @rdname mod_market_survey_leasing_summary
#' @export
#' @importFrom pkgload load_all
#' @importFrom bslib page_fluid bs_theme
#' @importFrom shiny shinyApp
mod_market_survey_leasing_summary_demo <- function() {

  pkgload::load_all()

  pool <- db_connect(user_id = 1)

  ui <- bslib::page_fluid(
    title = "Demo",
    theme = app_theme(),
    lang = "en",
    mod_market_survey_leasing_summary_ui("demo")
  )

  server <- function(input, output, session) {

    session$userData$user <- shiny::reactive({
      user_id <- 1
      user_email <- "jimmy.briggs@noclocks.dev"
      user_role <- "admin"
      list(
        user_id = user_id,
        email = user_email,
        role = user_role
      )
    })

    mod_market_survey_leasing_summary_server(
      "demo",
      selected_property = shiny::reactive({ "739085" }),
      pool = pool
    )

  }

  bslib::run_with_themer(shinyApp(ui, server))
}


