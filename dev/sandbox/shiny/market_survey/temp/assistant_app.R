library(shiny)
library(bslib)
library(htmltools)

ui <- page_fillable(
  theme = bs_theme(version = 5, preset = "bootstrap"),

  # Survey Submission Form
  navset_card_pill(
    nav_panel(
      "Survey Submission",
      layout_column_wrap(
        width = 1/3,

        # Property Summary
        card(
          card_header(h3("Property Summary")),
          card_body(
            tags$table(
              class = "table table-bordered",
              tags$tbody(
                tags$tr(tags$td("Property Name:"), tags$td("1047 Commonwealth")),
                tags$tr(tags$td("Lease Launch Date:"), tags$td(dateInput("lease_launch", NULL, value = Sys.Date()))),
                tags$tr(tags$td("Renewal Launch Date:"), tags$td(dateInput("renewal_launch", NULL, value = Sys.Date()))),
                tags$tr(tags$td("Current Occupancy:"), tags$td(numericInput("occupancy", NULL, value = 99.5, step = 0.1))),
                tags$tr(tags$td("Current Pre-Lease (%):"), tags$td(numericInput("pre_lease", NULL, value = 38.8, step = 0.1))),
                tags$tr(tags$td("Total Renewals:"), tags$td(numericInput("renewals", NULL, value = 51))),
                tags$tr(tags$td("Total New Leases:"), tags$td(numericInput("new_leases", NULL, value = 20)))
              )
            )
          )
        ),

        # Utilities Summary
        card(
          card_header(h3("Utilities Summary")),
          card_body(
            tags$table(
              class = "table table-bordered",
              tags$tbody(
                tags$tr(tags$td("Electricity Amount:"), tags$td(textInput("electricity", NULL, value = "$25"))),
                tags$tr(tags$td("Gas Amount:"), tags$td(textInput("gas", NULL, value = "$0"))),
                tags$tr(tags$td("Water Amount:"), tags$td(textInput("water", NULL, value = "$0")))
              )
            )
          )
        ),

        # Parking
        card(
          card_header(h3("Parking")),
          card_body(
            tags$table(
              class = "table table-bordered",
              tags$tbody(
                tags$tr(tags$td("Reserved Parking Amount:"), tags$td(textInput("reserved_parking", NULL, value = "$275"))),
                tags$tr(tags$td("Covered Parking Amount:"), tags$td(textInput("covered_parking", NULL, value = "$175"))),
                tags$tr(tags$td("Garage Parking Amount:"), tags$td(textInput("garage_parking", NULL, value = "$0")))
              )
            )
          )
        )
      ),

      layout_column_wrap(
        width = 1,

        # Fees and Other Editable Fields
        card(
          card_header(h3("Additional Fees")),
          card_body(
            tags$table(
              class = "table table-bordered",
              tags$tbody(
                tags$tr(tags$td("Application Fee:"), tags$td(textInput("application_fee", NULL, value = "$0"))),
                tags$tr(tags$td("Lease Fee:"), tags$td(textInput("lease_fee", NULL, value = "$0"))),
                tags$tr(tags$td("Move-In Fee:"), tags$td(textInput("move_in_fee", NULL, value = "$0")))
              )
            )
          )
        ),

        # Submit Button
        card(
          card_header(h3("Submit Survey")),
          card_body(
            actionButton("submit", "Submit Survey", class = "btn-primary btn-lg")
          )
        )
      )
    )
  )
)


server <- function(input, output, session) {
    library(DBI)
    library(RPostgres)

    # Database Connection
    con <- DBI::dbConnect(
      drv = RPostgres::Postgres(),
      dbname = db_config$dbname,
      host = db_config$host,
      port = db_config$port,
      user = db_config$user,
      password = db_config$password
    )

    onStop(function() {
      dbDisconnect(con)
    })

    # Observe Submission Event
    observeEvent(input$submit, {
      # Collect Inputs
      lease_launch <- input$lease_launch
      renewal_launch <- input$renewal_launch
      occupancy <- input$occupancy
      pre_lease <- input$pre_lease
      renewals <- input$renewals
      new_leases <- input$new_leases
      electricity <- gsub("[^0-9.]", "", input$electricity) # Remove '$'
      gas <- gsub("[^0-9.]", "", input$gas)
      water <- gsub("[^0-9.]", "", input$water)
      reserved_parking <- gsub("[^0-9.]", "", input$reserved_parking)
      covered_parking <- gsub("[^0-9.]", "", input$covered_parking)
      garage_parking <- gsub("[^0-9.]", "", input$garage_parking)
      application_fee <- gsub("[^0-9.]", "", input$application_fee)
      lease_fee <- gsub("[^0-9.]", "", input$lease_fee)
      move_in_fee <- gsub("[^0-9.]", "", input$move_in_fee)

      # Update Leasing Summary
      dbExecute(con, "
      UPDATE mkt.leasing_summary
      SET lease_launch_date = $1, renewal_launch_date = $2, current_occupancy = $3,
          current_pre_lease = $4, total_renewals = $5, total_new_leases = $6
      WHERE property_id = 1
    ", params = list(lease_launch, renewal_launch, occupancy, pre_lease, renewals, new_leases))

      # Update Utilities
      dbExecute(con, "
      UPDATE mkt.utilities
      SET cost = $1 WHERE property_id = 1 AND utility_name = 'Electricity'
    ", params = list(electricity))
      dbExecute(con, "
      UPDATE mkt.utilities
      SET cost = $1 WHERE property_id = 1 AND utility_name = 'Gas'
    ", params = list(gas))
      dbExecute(con, "
      UPDATE mkt.utilities
      SET cost = $1 WHERE property_id = 1 AND utility_name = 'Water'
    ", params = list(water))

      # Update Parking
      dbExecute(con, "
      UPDATE mkt.parking
      SET amount = $1 WHERE property_id = 1 AND parking_type = 'Reserved'
    ", params = list(reserved_parking))
      dbExecute(con, "
      UPDATE mkt.parking
      SET amount = $1 WHERE property_id = 1 AND parking_type = 'Covered'
    ", params = list(covered_parking))
      dbExecute(con, "
      UPDATE mkt.parking
      SET amount = $1 WHERE property_id = 1 AND parking_type = 'Garage'
    ", params = list(garage_parking))

      # Update Fees
      dbExecute(con, "
      UPDATE mkt.additional_fees
      SET application_fee = $1, lease_fee = $2, move_in_fee = $3
      WHERE property_id = 1
    ", params = list(application_fee, lease_fee, move_in_fee))

      # Confirmation
      showModal(modalDialog(
        title = "Survey Submitted",
        "Your changes have been saved successfully!",
        easyClose = TRUE
      ))
    })

    # Disconnect from DB on app stop
    onStop(function() {
      dbDisconnect(con)
    })
  }


shinyApp(ui, server)
