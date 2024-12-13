db_config <- config::get("db")

db_connection <- DBI::dbConnect(
  drv = RPostgres::Postgres(),
  dbname = db_config$dbname,
  host = db_config$host,
  port = db_config$port,
  user = db_config$user,
  password = db_config$password
)

mod_market_property_summary_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_column_wrap(
    width = 1/2, # Two cards per row
    bslib::card(
      bslib::card_header("Property Summary"),
      bslib::card_body(
        shiny::textInput(ns("property_name"), "Property Name", value = ""),
        shiny::textInput(ns("website"), "Website", value = ""),
        shiny::textInput(ns("address"), "Address", value = ""),
        shiny::textInput(ns("phone"), "Phone Number", value = ""),
        shiny::textInput(ns("developer"), "Developer", value = ""),
        shiny::textInput(ns("manager"), "Manager", value = ""),
        shiny::textInput(ns("owner"), "Owner", value = ""),
        shiny::selectInput(ns("property_type"), "Property Type",
                           choices = c("Student", "Conventional", "Affordable", "Innovative"),
                           selected = NULL),
        shiny::sliderInput(ns("property_rating"), "Property Rating (1-5 Stars)", min = 1, max = 5, value = 3),
        shiny::selectInput(ns("property_status"), "Property Status",
                           choices = c("New Construction", "Operational", "Undergoing Renovation"),
                           selected = NULL),
        shiny::selectInput(ns("comp_status"), "Comp Status",
                           choices = c("Subject Property", "Tier 1", "Tier 2"),
                           selected = NULL),
        shiny::numericInput(ns("year_built"), "Year Built", value = 2000),
        shiny::dateInput(ns("most_recent_sale"), "Most Recent Sale", value = Sys.Date()),
        shiny::sliderInput(ns("distance_from_campus"), "Distance from Campus (miles)", min = 0, max = 10, value = 1)
      ),
      bslib::card_footer(
        shiny::actionButton(ns("submit"), "Save Changes", class = "btn-primary")
      )
    )
  )
}

mod_market_property_summary_server <- function(id, db_connection) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    onStop(function() {
      dbDisconnect(db_connection)
    })

    # Pre-populate inputs
    observe({
      property_data <- DBI::dbGetQuery(
        db_connection,
        "SELECT * FROM mkt.properties WHERE property_id = 1 LIMIT 1" # Replace with dynamic query
      )

      if (nrow(property_data) > 0) {
        updateTextInput(session, ns("property_name"), value = property_data$property_name)
        updateTextInput(session, ns("website"), value = property_data$website)
        updateTextInput(session, ns("address"), value = property_data$address)
        updateTextInput(session, ns("phone"), value = property_data$phone)
        updateTextInput(session, ns("developer"), value = property_data$developer)
        updateTextInput(session, ns("manager"), value = property_data$manager)
        updateTextInput(session, ns("owner"), value = property_data$owner)
        updateSelectInput(session, ns("property_type"), selected = property_data$property_type)
        updateSliderInput(session, ns("property_rating"), value = property_data$property_rating)
        updateSelectInput(session, ns("property_status"), selected = property_data$property_status)
        updateSelectInput(session, ns("comp_status"), selected = property_data$comp_status)
        updateNumericInput(session, ns("year_built"), value = property_data$year_built)
        updateDateInput(session, ns("most_recent_sale"), value = property_data$most_recent_sale)
        updateSliderInput(session, ns("distance_from_campus"), value = property_data$distance_from_campus)
      }
    })

    # Validate and Save Changes
    observeEvent(input$submit, {
      # Validation
      if (input$property_name == "") {
        showNotification("Property Name is required.", type = "error")
        return()
      }

      # Save to Database
      tryCatch({
        DBI::dbExecute(
          db_connection,
          "UPDATE mkt.properties
           SET property_name = ?, website = ?, address = ?, phone = ?, developer = ?, manager = ?, owner = ?,
               property_type = ?, property_rating = ?, property_status = ?, comp_status = ?, year_built = ?,
               most_recent_sale = ?, distance_from_campus = ?
           WHERE property_id = 1",
          params = list(
            input$property_name,
            input$website,
            input$address,
            input$phone,
            input$developer,
            input$manager,
            input$owner,
            input$property_type,
            input$property_rating,
            input$property_status,
            input$comp_status,
            input$year_built,
            as.Date(input$most_recent_sale),
            input$distance_from_campus
          )
        )
        showNotification("Changes saved successfully.", type = "message")
      }, error = function(e) {
        showNotification("Failed to save changes. Error: " %||% e$message, type = "error")
      })
    })
  })
}

shinyApp(
  ui = fluidPage(
    mod_market_property_summary_ui("property_summary")
  ),
  server = function(input, output, session) {
    mod_market_property_summary_server("property_summary", db_connection)
  }
)
