
#  ------------------------------------------------------------------------
#
# Title : market_survey_property_summary Shiny Module
#    By : Jimmy Briggs
#  Date : 2024-11-27
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' market_survey_property_summary Shiny Module
#'
#' @name mod_market_survey_property_summary
#'
#' @description
#' A Shiny Module for ...
#'
#' - `mod_market_survey_property_summary_ui()`: User interface
#' - `mod_market_survey_property_summary_server()`: Server logic
#'
#' @param id Module's namespace ID.
#'
#' @return
#' - `mod_market_survey_property_summary_ui()`: UI HTML Output.
#' - `mod_market_survey_property_summary_server()`: Reactive values returned from server logic.
#'
#' @examples
#' if (interactive()) {
#'   mod_market_survey_property_summary_demo()
#' }
NULL


# UI ----------------------------------------------------------------------

#' @rdname mod_market_survey_property_summary
#' @export
#' @importFrom bsicons bs_icon
#' @importFrom bslib card card_header card_body layout_column_wrap card_footer input_task_button
#' @importFrom htmltools tagList tags
#' @importFrom shiny NS textOutput icon actionButton
#' @importFrom shinyjs useShinyjs disabled
#' @importFrom shinyWidgets useSweetAlert textInputIcon radioGroupButtons
#' @importFrom shinyWidgets noUiSliderInput airYearpickerInput airDatepickerInput numericInputIcon progressBar
#' @importFrom waiter use_waiter waiterShowOnLoad spin_fading_circles
mod_market_survey_property_summary_ui <- function(id) {

  ns <- shiny::NS(id)

  htmltools::tagList(
    htmltools::tags$head(
      shinyjs::useShinyjs(),
      waiter::use_waiter(),
      shinyWidgets::useSweetAlert(),
      waiter::waiterShowOnLoad(waiter::spin_fading_circles()),
      htmltools::HTML('<div data-fillout-id="rB4usRFt4Rus" data-fillout-embed-type="popup" data-fillout-button-text="Launch Survey Form" data-fillout-dynamic-resize data-fillout-button-size="medium" data-fillout-button-float="bottom-right" data-fillout-inherit-parameters data-fillout-popup-size="large"></div><script src="https://server.fillout.com/embed/v1/"></script>'
      )
    ),
    bslib::card(
      bslib::card_header(
        htmltools::tags$span(
          bsicons::bs_icon("building"),
          shiny::textOutput(ns("selected_property_title"), inline = TRUE)
        ),
        class = "bg-dark text-white"
      ),
      bslib::card_body(
        bslib::layout_column_wrap(
          width = 1/4,
          # property information
          bslib::card(
            bslib::card_header(
              htmltools::tags$span(
                bsicons::bs_icon("info-circle"),
                "Property Information"
              ),
              class = "bg-dark"
            ),
            bslib::card_body(
              shinyWidgets::textInputIcon(
                ns("property_name"),
                label = icon_text(shiny::icon("building"), "Property Name"),
                icon = shiny::icon("building"),
                placeholder = "Enter Property Name"
              ) |>
                with_tooltip("The name of the property."),

              shinyWidgets::textInputIcon(
                ns("property_website"),
                label = icon_text(shiny::icon("globe"), "Property Website"),
                icon = shiny::icon("globe"),
                placeholder = "Enter Property Website"
              ) |>
                with_tooltip("The website of the property."),

              shinyWidgets::textInputIcon(
                ns("property_address"),
                label = "Property Address",
                icon = shiny::icon("map-marker-alt"),
                placeholder = "Enter Property Address"
              ) |>
                with_tooltip("The address of the property."),

              shinyWidgets::textInputIcon(
                ns("property_phone_number"),
                label = "Phone Number",
                icon = shiny::icon("phone"),
                placeholder = "Enter Property Phone Number"
              ) |>
                with_tooltip("The phone number of the property."),

              shinyWidgets::textInputIcon(
                ns("property_image"),
                label = "Property Image",
                icon = shiny::icon("image"),
                value = NULL,
                placeholder = "www.example.com/image.jpg"
              ) |>
                with_tooltip("The image of the property.")

            ),
            bslib::card_footer(
              # TODO:
              # validation
            )
          ),

          # property management
          bslib::card(
            bslib::card_header(
              htmltools::tags$span(
                bsicons::bs_icon("tools"),
                "Property Management"
              ),
              class = "bg-dark"
            ),
            bslib::card_body(
              shinyWidgets::textInputIcon(
                ns("property_developer"),
                label = icon_text("hard-hat", "Developer"),
                icon = shiny::icon("hard-hat"),
                value = NULL,
                placeholder = "Enter Property Developer..."
              ) |>
                with_tooltip("The developer of the property."),

              shinyWidgets::textInputIcon(
                ns("property_manager"),
                label = icon_text("user-tie", "Manager"),
                icon = shiny::icon("user-tie"),
                value = NULL,
                placeholder = "Enter Property Manager..."
              ) |>
                with_tooltip("The manager of the property."),

              shinyWidgets::textInputIcon(
                ns("property_owner"),
                label = icon_text("user", "Owner"),
                icon = shiny::icon("user"),
                value = NULL,
                placeholder = "Enter Property Owner..."
              ) |>
                with_tooltip("The owner of the property.")
            ),
            bslib::card_footer(

            )
          ),

          # property details
          bslib::card(
            bslib::card_header(
              icon_text("house", "Property Details"),
              class = "bg-dark"
            ),
            bslib::card_body(
              shinyWidgets::radioGroupButtons(
                ns("property_type"),
                label = icon_text("check", "Property Type"),
                choices = c("Student", "Conventional", "Affordable", "Innovative"),
                selected = "Student",
                checkIcon = list(yes = shiny::icon("check")),
                individual = TRUE,
                size = "sm",
                justified = TRUE
              ) |>
                with_tooltip("The type of property."),

              shinyWidgets::radioGroupButtons(
                ns("property_status"),
                label = icon_text("building", "Property Status"),
                choices = c("Operational", "New Construction", "Undergoing Renovations"),
                selected = "Operational",
                checkIcon = list(yes = shiny::icon("check")),
                individual = TRUE,
                size = "sm",
                justified = TRUE
              ) |>
                with_tooltip("The status of the property."),

              shinyWidgets::noUiSliderInput(
                ns("property_rating"),
                label = icon_text("star", "Property Rating"),
                min = 0,
                max = 5,
                step = 0.5,
                value = 3
              ) |>
                with_tooltip("The rating of the property."),

              shinyWidgets::radioGroupButtons(
                ns("comp_status"),
                label = icon_text("check-circle", "Comp Status"),
                choices = c("Subject Property", "Tier 1", "Tier 2"),
                selected = "Subject Property",
                checkIcon = list(yes = shiny::icon("check")),
                individual = TRUE,
                size = "sm",
                justified = TRUE
              ) |>
                with_tooltip("The comp status of the property.")
            ),
            bslib::card_footer(

            )
          ),

          bslib::card(
            bslib::card_header(
              icon_text("list", "Additional Details"),
              class = "bg-dark"
            ),
            bslib::card_body(
              shinyWidgets::airYearpickerInput(
                ns("year_built_renovated"),
                label = icon_text("calendar", "Year Built/Renovated"),
                value = Sys.Date()
              ) |>
                with_tooltip("The year the property was built or renovated."),

              shinyWidgets::airDatepickerInput(
                ns("most_recent_sale"),
                label = icon_text("calendar", "Most Recent Sale"),
                placeholder = "Select Date",
                value = NULL,
                todayButton = TRUE,
                autoClose = TRUE,
                addon = "left"
              ) |>
                with_tooltip("The most recent sale of the property."),

              shinyWidgets::numericInputIcon(
                ns("distance_to_campus"),
                label = icon_text("map-marker-alt", "Distance to Campus"),
                value = 5.0,
                step = 0.1,
                min = 0
              ) |>
                with_tooltip("The distance to campus from the property.")
            ),
            bslib::card_footer(

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

#' @rdname mod_market_survey_property_summary
#' @export
#' @importFrom cli cat_rule cli_alert_info cli_alert_warning cli_alert_danger cli_alert_success
#' @importFrom dplyr pull filter collect tbl glimpse tibble
#' @importFrom pool dbIsValid
#' @importFrom purrr map_chr walk2 keep
#' @importFrom readr read_csv
#' @importFrom shiny moduleServer reactive observe reactiveVal observeEvent req showNotification renderText withProgress
#' @importFrom shinyjs enable disable
#' @importFrom shinyvalidate InputValidator sv_required sv_url sv_regex sv_gt
#' @importFrom shinyWidgets updateTextInputIcon updateRadioGroupButtons updateNumericInputIcon updateAirDateInput updateProgressBar
#' @importFrom waiter waiter_show waiter_hide
mod_market_survey_property_summary_server <- function(
    id,
    selected_property = NULL,
    pool = NULL,
    property_data = NULL
) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
      cli::cat_rule("[Module]: mod_market_survey_property_summary_server()")

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

      # reference data ----------------------------------------------------------
      property_summary_inputs <- readr::read_csv(
        system.file(
          "extdata/survey/property_summary_inputs.csv",
          package = "gmhcommunities"
        ),
        show_col_types = FALSE
      )

      property_summary_input_ids <- property_summary_inputs$id

      # store survey inputs data in a reactive value
      input_data <- shiny::reactive({
        list(
          property_name = input$property_name,
          property_website = input$property_website,
          property_address = input$property_address,
          property_phone_number = input$property_phone_number,
          property_image = input$property_image,
          property_developer = input$property_developer,
          property_manager = input$property_manager,
          property_owner = input$property_owner,
          property_type = input$property_type,
          property_status = input$property_status,
          property_rating = input$property_rating,
          comp_status = input$comp_status,
          year_built_renovated = input$year_built_renovated,
          most_recent_sale = input$most_recent_sale,
          distance_to_campus = input$distance_to_campus
        )
      }) |>
        shiny::bindCache(
          cache = "app",
          input$property_name,
          input$property_website,
          input$property_address,
          input$property_phone_number,
          input$property_image,
          input$property_developer,
          input$property_manager,
          input$property_owner,
          input$property_type,
          input$property_status,
          input$property_rating,
          input$comp_status,
          input$year_built_renovated,
          input$most_recent_sale,
          input$distance_to_campus
        )

      # validation ----------------------------------------------------------
      iv <- initialize_property_summary_validator()


      # pre-populate inputs -------------------------------------------------

      # reactive values ---------------------------------------------------------
      db_data <- shiny::reactiveVal(NULL)
      app_data <- shiny::reactiveVal(NULL)
      changes <- shiny::reactiveVal(NULL)
      trigger_refresh <- shiny::reactiveVal(FALSE)

      # get data ----------------------------------------------------------------
      shiny::observeEvent(list(selected_property(), input$refresh), {
        shiny::req(pool, selected_property())

        if (is.null(pool) || !pool::dbIsValid(pool)) {
          cli::cli_alert_warning(
            c(
              "Invalid or NULL database connection provided...",
              "Will use demo data."
            )
          )

          demo_data <- readr::read_csv(
            system.file(
              "extdata/data/property_summary_data.csv",
              package = "gmhcommunities"
            )
          )

          db_data(demo_data)

        }

        tryCatch({

          prop_id <- selected_property()

          data <- dplyr::tbl(pool, I("mkt.property_summary")) |>
            dplyr::filter(.data$property_id == .env$prop_id) |>
            dplyr::collect()

          db_data(data)

          shiny::showNotification("Data fetched successfully!", type = "message")

        }, error = function(e) {

          cli::cli_alert_danger(
            c(
              "Error fetching data from the database",
              "Error: {.error {conditionMessage(e)}}"
            )
          )

          shiny::showNotification(
            "Error fetching data from the database",
            type = "error"
          )

          db_data(NULL)

        })
      })

      # outputs -----------------------------------------------------------------
      output$selected_property_title <- shiny::renderText({
        req(db_data())
        paste0("Property Summary: ", db_data()$property_name, " (", selected_property(), ")")
      })

      output$last_updated_text <- shiny::renderText({
        shiny::req(db_data())

        if (nrow(db_data()) > 0) {
          last_updated <- db_data()$updated_at |>
            format(
              "%Y-%m-%d %H:%M:%S",
              tz = "America/New_York"
            )

          paste0("Last Updated: ", last_updated)
        } else {
          "Last Updated: N/A"
        }

      })

      # inputs ------------------------------------------------------------------
      shiny::observe({
        req(db_data())

        data <- db_data()

        shinyWidgets::updateTextInputIcon(
          session,
          "property_name",
          value = data$property_name
        )

        shinyWidgets::updateTextInputIcon(
          session,
          "property_website",
          value = data$website
        )

        shinyWidgets::updateTextInputIcon(
          session,
          "property_address",
          value = data$address
        )

        shinyWidgets::updateTextInputIcon(
          session,
          "property_phone_number",
          value = data$phone_number
        )

        shinyWidgets::updateTextInputIcon(
          session,
          "property_image",
          value = data$property_image
        )

        shinyWidgets::updateTextInputIcon(
          session,
          "property_developer",
          value = data$developer
        )

        shinyWidgets::updateTextInputIcon(
          session,
          "property_manager",
          value = data$manager
        )

        shinyWidgets::updateTextInputIcon(
          session,
          "property_owner",
          value = data$owner
        )

        shinyWidgets::updateRadioGroupButtons(
          session,
          "property_type",
          selected = data$property_type
        )

        shinyWidgets::updateRadioGroupButtons(
          session,
          "property_status",
          selected = data$property_status
        )

        shinyWidgets::updateNumericInputIcon(
          session,
          "property_rating",
          value = as.numeric(data$property_rating)
        )

        shinyWidgets::updateRadioGroupButtons(
          session,
          "comp_status",
          selected = data$comp_status
        )

        shinyWidgets::updateAirDateInput(
          session,
          "year_built_renovated",
          value = as.numeric(data$year_built)
        )

        shinyWidgets::updateAirDateInput(
          session,
          "most_recent_sale",
          value = as.Date(data$most_recent_sale)
        )

        shinyWidgets::updateNumericInputIcon(
          session,
          "distance_to_campus",
          value = as.numeric(data$distance_from_campus)
        )

        # Enable validation only after data is loaded
        iv$initialize()
        iv$enable()

        valid <- iv$is_valid()

        shinyWidgets::updateProgressBar(
          session = session,
          id = "progress",
          value = 100,
          status = if (valid) "success" else "danger"
        )

        cli::cli_alert_info(
          "Updated input values from data:"
        )

        dplyr::glimpse(data)

      }) |>
        shiny::bindEvent(db_data())

      # waiter
      shiny::observeEvent(db_data(), {
        shiny::req(db_data())
        waiter::waiter_hide()  # Hide waiter once data is loaded
      }, ignoreInit = FALSE)

      # track changes -----------------------------------------------------------
      shiny::observe({
        req(
          db_data(),
          input$property_name,
          input$property_website,
          input$property_address,
          input$property_phone_number,
          input$property_image,
          input$property_developer,
          input$property_manager,
          input$property_owner,
          input$property_type,
          input$property_status,
          input$property_rating,
          input$comp_status,
          input$year_built_renovated,
          input$most_recent_sale,
          input$distance_to_campus
        )

        current_inputs <- list(
          property_name = input$property_name,
          property_website = input$property_website,
          property_address = input$property_address,
          property_phone_number = input$property_phone_number,
          property_image = input$property_image,
          property_developer = input$property_developer,
          property_manager = input$property_manager,
          property_owner = input$property_owner,
          property_type = input$property_type,
          property_status = input$property_status,
          property_rating = input$property_rating,
          comp_status = input$comp_status,
          year_built_renovated = input$year_built_renovated,
          most_recent_sale = input$most_recent_sale,
          distance_to_campus = input$distance_to_campus
        )

        current_app_data <- app_data()
        new_app_data <- dplyr::tibble(
          property_name = current_inputs$property_name,
          website = current_inputs$property_website,
          address = current_inputs$property_address,
          phone_number = current_inputs$property_phone_number,
          property_image = current_inputs$property_image,
          developer = current_inputs$property_developer,
          manager = current_inputs$property_manager,
          owner = current_inputs$property_owner,
          property_type = current_inputs$property_type,
          property_status = current_inputs$property_status,
          property_rating = current_inputs$property_rating,
          comp_status = current_inputs$comp_status,
          year_built = current_inputs$year_built_renovated,
          most_recent_sale = current_inputs$most_recent_sale,
          distance_from_campus = current_inputs$distance_to_campus
        )

        changes_list <- purrr::keep(
          names(current_inputs),
          ~ {
            field_name <- .x
            if (!field_name %in% colnames(db_data())) return(FALSE) # Skip missing fields
            current_inputs[[field_name]] != db_data()[[field_name]]
          }
        )

        print(changes_list)

        changes(changes_list)
        app_data(new_app_data)

      })

      # save button
      shiny::observe({
        shiny::req(changes(), iv$is_valid())
        if (length(changes()) > 0 && iv$is_valid()) {
          shinyjs::enable("save")
        } else {
          shinyjs::disable("save")
        }
      })

      # progress bar
      shiny::observeEvent(changes(), {

        total_fields <- length(property_summary_input_ids)
        changed_fields <- length(changes())
        valid <- iv$is_valid()
        if (valid) { pct_complete <- 100 }
        else { pct_complete <- (changed_fields / total_fields) * 100 }

        shinyWidgets::updateProgressBar(
          session = session,
          id = "progress",
          value = pct_complete
        )

      }, ignoreInit = TRUE)


      # save --------------------------------------------------------------------
      shiny::observeEvent(input$save, {
        shiny::req(length(changes()) > 0, pool, selected_property())
        shiny::req(iv$is_valid())

        shiny::withProgress(
          message = "Saving changes...",
          {
            property_id <- selected_property()
            current_changes <- changes()

            success <- tryCatch({
              db_update_property_summary(
                pool,
                property_id,
                current_changes
              )
              TRUE
            }, error = function(e) {
              cli::cli_alert_danger(
                c(
                  "Error saving changes to the database: {conditionMessage(e)}"
                )
              )
              FALSE
            })

            if (success) {
              cli::cli_alert_success("Changes saved successfully!")
              updated_data <- db_refresh_property_data(pool, property_id)
              db_data(updated_data)
              app_data(updated_data)
              trigger_refresh(TRUE)
            } else {
              shiny::showNotification("Failed to save changes.", type = "error")
            }
          }
        )
      })

      # return
      return(
        list(
          db_data = db_data,
          app_data = app_data,
          changes = changes,
          trigger_refresh = trigger_refresh
        )
      )
    })
}

#' @rdname mod_market_survey_property_summary
#' @export
#' @importFrom bslib page_fluid bs_theme run_with_themer
#' @importFrom htmltools tagList tags
#' @importFrom pkgload load_all
#' @importFrom shiny reactive
#' @importFrom shinyjs useShinyjs
#' @importFrom waiter use_waiter
mod_market_survey_property_summary_demo <- function() {

  pkgload::load_all()

  ui <- bslib::page_fluid(
    title = "Demo",
    theme = bslib::bs_theme(
      version = 5,
      primary = "#0e2b4c"
    ),
    lang = "en",
    mod_market_survey_property_summary_ui("demo")
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

    pool <- db_connect(user_id = 1)

    mod_market_survey_property_summary_server(
      "demo",
      selected_property = shiny::reactive({ "739085" }),
      pool = pool
    )

  }

  bslib::run_with_themer(shinyApp(ui, server))
}


# helpers -----------------------------------------------------------------

initialize_property_summary_validator <- function() {

  if (!exists("property_summary_inputs")) {
    property_summary_inputs <- readr::read_csv(
      system.file(
        "extdata/survey/property_summary_inputs.csv",
        package = "gmhcommunities"
      )
    )
  }

  property_summary_input_ids <- property_summary_inputs$id
  property_summary_required_input_ids <- property_summary_inputs |>
    dplyr::filter(required == TRUE) |>
    dplyr::pull(id)
  property_summary_required_input_validation_messages <- property_summary_inputs |>
    dplyr::filter(required == TRUE) |>
    dplyr::pull(name) |>
    purrr::map_chr(~ paste0(.x, " is required."))

  iv <- shinyvalidate::InputValidator$new()

  # add all required inputs
  purrr::walk2(
    property_summary_required_input_ids,
    property_summary_required_input_validation_messages,
    ~ iv$add_rule(.x, shinyvalidate::sv_required(message = .y))
  )

  # add regex rules for phone and website
  phone_regex <- "^(?:\\(?[0-9]{3}\\)?[-. ]?[0-9]{3}[-. ]?[0-9]{4})$"

  iv$add_rule("property_website", shinyvalidate::sv_url(message = "Please enter a valid URL for the property website."))
  iv$add_rule("property_phone_number", shinyvalidate::sv_regex(phone_regex, "Enter a valid phone number."))

  # add rules for distance to campus and property rating
  iv$add_rule("distance_to_campus", shinyvalidate::sv_gt(0, "Distance must be greater than 0."))
  iv$add_rule("property_rating", shinyvalidate::sv_gt(0, "Rating must be greater than 0."))

  # return
  return(iv)
}

#' @importFrom cli cli_alert_danger cli_abort
#' @importFrom DBI dbExecute
#' @importFrom dbplyr in_schema
#' @importFrom pool dbIsValid poolCheckout poolReturn
db_update_property_summary <- function(pool, property_id, changes) {

  stopifnot(
    pool::dbIsValid(pool),
    is.character(property_id),
    length(changes) > 0
  )

  conn <- pool::poolCheckout(pool)

  tryCatch({
    fields <- names(changes)
    values <- unname(changes)
    set_clause <- paste(fields, "= ?", collapse = ", ")

    qry <- paste0(
      "UPDATE ",
      dbplyr::in_schema("mkt", "property_summary"),
      " SET ",
      set_clause,
      " WHERE property_id = ?"
    )

    DBI::dbExecute(
      conn,
      qry,
      params = c(values, property_id),
    )

  }, error = function(e) {
    cli::cli_alert_danger(
      c(
        "Error updating property summary data.",
        "Error: {.error_message {conditionMessage(e)}}"
      )
    )
    cli::cli_abort("{e}")
  }, finally = {
    pool::poolReturn(conn)
  })

}

#' @importFrom cli cli_alert_danger
#' @importFrom dbplyr in_schema
#' @importFrom dplyr collect filter tbl
db_refresh_property_data <- function(pool, property_id) {
  tryCatch({
    dplyr::tbl(pool, dbplyr::in_schema("mkt", "property_summary")) |>
      dplyr::filter(.data$property_id == .env$property_id) |>
      dplyr::collect()
  }, error = function(e) {
    cli::cli_alert_danger("Error fetching property data: {e$message}")
    NULL
  })
}


# update_inputs <- function(input_data, input_mappings, session = shiny::getDefaultReactiveDomain()) {
#
#   purrr::walk2(
#     names(input_data),
#     input_data,
#     ~ {
#       if (.x %in% names(input_mappings)) {
#         input_mapping <- input_mappings[[.x]]
#       }
#       do.call(
#         input_mappings$update_function,
#         c(list(session, .x), input_mapping$params, list(value = .y))
#       )
#     }
#   )
#
# }
