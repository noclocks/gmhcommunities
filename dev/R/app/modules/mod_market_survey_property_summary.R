
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
#' @importFrom shiny NS
#' @importFrom htmltools tagList tags
mod_market_survey_property_summary_ui <- function(id) {

  ns <- shiny::NS(id)

  htmltools::tagList(
    bslib::card(
      bslib::card_header(
        htmltools::tags$span(
          bsicons::bs_icon("building"), "Property Summary"
        ),
        class = "bg-dark"
      ),
      bslib::card_body(
        bslib::layout_column_wrap(
          bslib::card(
            bslib::card_header(
              icon_text("info-circle", "Property Information"),
              class = "bg-dark"
            ),
            bslib::card_body(
              shinyWidgets::textInputIcon(
                ns("property_name"),
                label = "Property Name",
                icon = shiny::icon("building"),
                value = NULL,
                placeholder = "1077 Commonwealth"
              ),
              shinyWidgets::textInputIcon(
                ns("website"),
                label = "Website",
                icon = shiny::icon("globe"),
                value = NULL,
                placeholder = "https://www.1077commonwealth.com"
              ),
              shinyWidgets::textInputIcon(
                ns("address"),
                label = "Address",
                icon = shiny::icon("house"),
                value = NULL,
                placeholder = "1077 Commonwealth Ave, Boston, MA 02215"
              ),
              shinyWidgets::textInputIcon(
                ns("phone"),
                label = "Phone",
                icon = shiny::icon("phone"),
                value = NULL,
                placeholder = "617-500-6481"
              ),
              shinyWidgets::textInputIcon(
                ns("property_image"),
                label = "Property Image",
                icon = shiny::icon("image"),
                value = NULL, # https://images1.forrent.com/i2/0ksd7Uwm-74efUhB-UiU1cEyVv3NVOiCCVuF4oQ7cZI/117/image.jpg
                placeholder = "www.example.com/image.jpg"
              )
            ),
            bslib::card_footer(
              shiny::actionButton(
                ns("submit_property_information"),
                label = "Submit",
                icon = shiny::icon("upload"),
                class = "btn btn-primary",
                style = "float: right;"
              )
            )
          ),
          bslib::card(
            bslib::card_header(
              icon_text("tools", "Property Management"),
              class = "bg-dark"
            ),
            bslib::card_body(
              shinyWidgets::pickerInput(
                ns("developer"),
                label = icon_text("tools", "Developer", .function = shiny::icon),
                choices = c("BPDA", "Other"),
                selected = "BPDA"
              ),
              shinyWidgets::pickerInput(
                ns("manager"),
                label = icon_text("person", "Manager", .function = shiny::icon),
                choices = c("GMH Communities", "Other"),
                selected = "GMH Communities"
              ),
              shinyWidgets::textInputIcon(
                ns("owner"),
                label = "Owner",
                icon = shiny::icon("person"),
                value = NULL,
                placeholder = "AGC + GMH Communities"
              )
            ),
            bslib::card_footer(
              shiny::actionButton(
                ns("submit_property_information"),
                label = "Submit",
                icon = shiny::icon("upload"),
                class = "btn btn-primary",
                style = "float: right;"
              )
            )
          ),
          bslib::card(
            bslib::card_header(
              icon_text("house", "Property Details"),
              class = "bg-dark"
            ),
            bslib::card_body(
              shinyWidgets::pickerInput(
                ns("property_type"),
                label = icon_text("house", "Property Type", .function = shiny::icon),
                choices = c("Student", "Conventional", "Affordable", "Innovative"),
                selected = "Student"
              ),
              shinyWidgets::noUiSliderInput(
                ns("property_rating"),
                label = icon_text("star", "Property Rating", .function = shiny::icon),
                min = 1,
                max = 5,
                value = 2,
                step = 1
              ),
              shinyWidgets::pickerInput(
                ns("property_status"),
                label = icon_text("info-circle", "Property Status"),
                choices = c("Operational", "New Construction", "Undergoing Renovation"),
                selected = "Operational"
              ),
              shinyWidgets::pickerInput(
                ns("comp_status"),
                label = icon_text("diagram-2", "Comp Status"),
                choices = c("Subject Property", "Tier 1", "Tier 2"),
                selected = "Subject Property"
              )
            ),
            bslib::card_footer(
              shiny::actionButton(
                ns("submit_property_information"),
                label = "Submit",
                icon = shiny::icon("upload"),
                class = "btn btn-primary",
                style = "float: right;"
              )
            )
          ),
          bslib::card(
            bslib::card_header(
              icon_text("list", "Additional Details"),
              class = "bg-dark"
            ),
            bslib::card_body(
              shinyWidgets::numericInputIcon(
                ns("year_built"),
                label = "Year Built",
                icon = shiny::icon("calendar"),
                value = NULL,
                min = 1900,
                max = lubridate::year(Sys.Date()),
                step = 1,
                help_text = "Enter the year the property was built."
              ),
              shinyWidgets::airDatepickerInput(
                ns("most_recent_sale"),
                label = icon_text("calendar-date", "Most Recent Sale"),
                value = NULL,
                placeholder = "YYYY-MM-DD",
                dateFormat = "yyyy-MM-dd",
                minDate = "1900-01-01",
                maxDate = lubridate::today(),
                view = "days"
              ),
              shinyWidgets::numericInputIcon(
                ns("distance_from_campus"),
                label = "Distance from Campus (Miles)",
                icon = shiny::icon("map"),
                value = NULL,
                min = 0,
                max = 100,
                step = 0.1,
                help_text = "Enter the distance from campus in miles."
              )
            ),
            bslib::card_footer(
              shiny::actionButton(
                ns("submit_property_information"),
                label = "Submit",
                icon = shiny::icon("upload"),
                class = "btn btn-primary",
                style = "float: right;"
              )
            )
          )
        )
      ),
      bslib::card_footer(
        shiny::actionButton(
          ns("back"),
          label = "Back",
          icon = shiny::icon("arrow-left"),
          class = "btn btn-primary",
          style = "float: left;"
        ), # |> shinyjs::disable(),
        # shiny::actionButton(
        #   ns("save"),
        #   "Save",
        #   icon = shiny::icon("save"),
        #   class = "btn btn-success",
        #   style = "align: center;"
        # ), # |> shinyjs::disable(),
        shiny::actionButton(
          ns("next"),
          label = "Next",
          icon = shiny::icon("arrow-right"),
          class = "btn btn-primary",
          style = "float: right;"
        )
      )
    )
  )
}


# server ------------------------------------------------------------------

#' @rdname mod_market_survey_property_summary
#' @export
#' @importFrom shiny moduleServer
#' @importFrom cli cat_rule
mod_market_survey_property_summary_server <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      cli::cat_rule("mod_property_summary_server")

      # get user's associated tenant id
      tenant_id <- shiny::reactive({
        if (!is.null(session$userData$tenant_id)) {
          return(session$userData$tenant_id)
        } else {
          cli::cli_alert_info("No tenant_id found in session$userData. Not using a tenant.")
          return(NULL)
        }
      })

      # get associated property id from tenant id
      tenant_property_id <- shiny::reactive({

        if (!is.null(tenant_id())) {
          # conn <- db_connect()
          # tenant_property_id <- dplyr::tbl(conn, I("auth.tenants")) %>%
          #   dplyr::filter(tenant_id == tenant_id()) %>%
          #   dplyr::pull(property_id)
          # return(tenant_property_id)
        } else {
          cli::cli_alert_info("No tenant_id found. Defaulting to Commonwealth Property ID.")
          return("739085")
        }

      })

      # database data
      property_summary_data_db <- shiny::reactive({

        shiny::req(tenant_property_id())

        if (!is.null(session$userData$db_pool)) {
          conn <- session$userData$db_pool
        } else {
          conn <- db_connect()
          session$userData$db_pool <- conn
        }

        dplyr::tbl(conn, I("survey.property_summary")) |>
          dplyr::filter(
            "property_id" == tenant_property_id()
          )

      }) |>
        shiny::bindEvent(tenant_property_id())

      # app data
      property_summary_data_app <- shiny::reactive({

        tibble::tibble(
          property_id = tenant_property_id(),
          property_name = input$property_name,
          website = input$website,
          address = input$address,
          phone = input$phone,
          property_image = input$property_image,
          developer = input$developer,
          manager = input$manager,
          owner = input$owner,
          property_type = input$property_type,
          property_rating = input$property_rating,
          property_status = input$property_status,
          comp_status = input$comp_status,
          year_built = input$year_built,
          year_last_renovated = input$year_built, # todo
          most_recent_sale = input$most_recent_sale,
          distance_from_campus = input$distance_from_campus
        )

      }) |>
        shiny::bindEvent(
          input$property_name,
          input$website,
          input$address,
          input$phone,
          input$property_image,
          input$developer,
          input$manager,
          input$owner,
          input$property_type,
          input$property_rating,
          input$property_status,
          input$comp_status,
          input$year_built,
          input$most_recent_sale,
          input$distance_from_campus
        )

      # enable save on change
      # shiny::observeEvent(property_summary_data_app(), {
      #   shinyjs::enable("save")
      # })

      # save data
      shiny::observeEvent(input$save, {

        # confirm
        shinyWidgets::ask_confirmation(
          ns("confirm_save"),
          title = "Save Property Summary",
          text = "Are you sure you want to save the Property Summary?",
          type = "question",
          showCloseButton = TRUE
        )

      })

      shiny::observeEvent(input$confirm_save, {

        # save to database
        conn <- session$userData$db_pool
        pool::poolWithTransaction(conn, function(conn) {
          pool::dbExecute(
            conn,
            "INSERT INTO survey.property_summary (
              property_id,
              property_name,
              website,
              address,
              phone,
              property_image,
              developer,
              manager,
              owner,
              property_type,
              property_rating,
              property_status,
              comp_status,
              year_built,
              year_last_renovated,
              most_recent_sale,
              distance_from_campus
            ) VALUES (
              $1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17
            )",
            property_summary_data_app()$property_id,
            property_summary_data_app()$property_name,
            property_summary_data_app()$website,
            property_summary_data_app()$address,
            property_summary_data_app()$phone,
            property_summary_data_app()$property_image,
            property_summary_data_app()$developer,
            property_summary_data_app()$manager,
            property_summary_data_app()$owner,
            property_summary_data_app()$property_type,
            property_summary_data_app()$property_rating,
            property_summary_data_app()$property_status,
            property_summary_data_app()$comp_status,
            property_summary_data_app()$year_built,
            property_summary_data_app()$year_built,
            property_summary_data_app()$most_recent_sale,
            property_summary_data_app()$distance_from_campus
          )
        })

        shinyWidgets::show_alert(
          title = "Success",
          text = "Property Summary saved successfully.",
          type = "success"
        )

        cli::cli_alert_success("Property Summary saved successfully.")

      })

      # setup percent complete
      # section_progress <- shiny::reactiveVal(0)

      # next button
      shiny::observeEvent(input[["next"]], {
        bslib::nav_select(
          id = "mod_market_survey-survey_sections-leasing_summary",
          session = session
        )
      })

      # return
      return(
        list(
          property_summary_data_db = property_summary_data_db,
          property_summary_data_app = property_summary_data_app
        )
      )

    }
  )
}



# demo --------------------------------------------------------------------

#' @rdname mod_market_survey_property_summary
#' @export
#' @importFrom pkgload load_all
#' @importFrom bslib page_fluid bs_theme
#' @importFrom shiny shinyApp
mod_market_survey_property_summary_demo <- function() {

  pkgload::load_all()

  ui <- bslib::page_fillable(
    title = "Demo",
    theme = bslib::bs_theme(version = 5),
    lang = "en",
    shinyjs::useShinyjs(),
    mod_market_survey_property_summary_ui("demo")
  )

  server <- function(input, output, session) {

    session$userData$db_pool <- db_connect()

    mod_market_survey_property_summary_server("demo")
  }

  shiny::shinyApp(ui, server)
}
