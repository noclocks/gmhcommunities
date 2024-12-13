mod_hybrid_input_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::page_fillable(
    shiny::tabsetPanel(
      id = ns("input_mode"),
      type = "tabs",

      # Form-Based Input
      shiny::tabPanel(
        title = "Form Input",
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::textInput(ns("amenity_name"), "Amenity Name"),
            shinyWidgets::pickerInput(
              ns("amenity_available"),
              "Available",
              choices = c("Yes", "No"),
              selected = "Yes",
              multiple = FALSE
            ),
            shiny::actionButton(ns("add_amenity"), "Add Amenity")
          ),
          shiny::column(
            6,
            shinyWidgets::pickerInput(
              ns("edit_amenity"),
              "Edit Existing Amenity",
              choices = NULL
            ),
            shinyWidgets::pickerInput(
              ns("edit_available"),
              "Change Availability",
              choices = c("Yes", "No")
            ),
            shiny::actionButton(ns("update_amenity"), "Update Amenity")
          )
        )
      ),

      # Spreadsheet-Based Input
      shiny::tabPanel(
        title = "Spreadsheet Input",
        rhandsontable::rHandsontableOutput(ns("amenities_table")) |> shinycustomloader::withLoader()
      )
    )
  )
}

mod_hybrid_input_server <- function(id, initial_data) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # Reactive data source
      amenities_data <- shiny::reactiveVal(initial_data)

      # Update picker input choices dynamically
      shiny::observe({
        shinyWidgets::updatePickerInput(
          session,
          "edit_amenity",
          choices = amenities_data()$Amenity
        )
      })

      # Add new amenity via form input
      shiny::observeEvent(input$add_amenity, {
        new_row <- data.frame(
          Amenity = input$amenity_name,
          Available = input$amenity_available,
          stringsAsFactors = FALSE
        )
        amenities_data(dplyr::bind_rows(amenities_data(), new_row))
      })

      # Update existing amenity via form input
      shiny::observeEvent(input$update_amenity, {
        updated_data <- amenities_data()
        updated_data$Available[updated_data$Amenity == input$edit_amenity] <- input$edit_available
        amenities_data(updated_data)
      })

      # Render rhandsontable
      output$amenities_table <- rhandsontable::renderRHandsontable({
        rhandsontable::rhandsontable(amenities_data())
      })

      # Update data when rhandsontable is edited
      shiny::observeEvent(input$amenities_table, {
        amenities_data(rhandsontable::hot_to_r(input$amenities_table))
      })

      # Return the updated data
      return(amenities_data)
    }
  )
}

mod_hybrid_input_demo <- function() {
  initial_data <- data.frame(
    Amenity = c("Fitness Center", "Game Room", "Pool"),
    Available = c("Yes", "Yes", "No"),
    stringsAsFactors = FALSE
  )

  shiny::shinyApp(
    ui = bslib::page_fillable(
      theme = bslib::bs_theme(version = 5),
      mod_hybrid_input_ui("demo")
    ),
    server = function(input, output, session) {
      amenities_data <- mod_hybrid_input_server("demo", initial_data)

      shiny::observe({
        print(amenities_data())
      })
    }
  )
}

mod_hybrid_input_demo()


