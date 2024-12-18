library(shiny)
library(shinyWidgets)

# UI for Property Summary Section
property_summary_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 6,
        shinyWidgets::textInputIcon(
          inputId = ns("property_name"),
          label = "Property Name:",
          placeholder = "Enter property name",
          icon = icon("building")
        )
      ),
      column(
        width = 6,
        shinyWidgets::textInputIcon(
          inputId = ns("property_website"),
          label = "Website:",
          placeholder = "Enter website URL",
          icon = icon("globe")
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        shinyWidgets::textInputIcon(
          inputId = ns("property_address"),
          label = "Address:",
          placeholder = "Enter address",
          icon = icon("map-marker-alt")
        )
      ),
      column(
        width = 6,
        shinyWidgets::textInputIcon(
          inputId = ns("property_phone"),
          label = "Phone Number:",
          placeholder = "Enter phone number",
          icon = icon("phone")
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        shinyWidgets::textInputIcon(
          inputId = ns("developer"),
          label = "Developer:",
          placeholder = "Enter developer name",
          icon = icon("hard-hat")
        )
      ),
      column(
        width = 6,
        shinyWidgets::textInputIcon(
          inputId = ns("manager"),
          label = "Manager:",
          placeholder = "Enter manager name",
          icon = icon("user-tie")
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        shinyWidgets::textInputIcon(
          inputId = ns("owner"),
          label = "Owner:",
          placeholder = "Enter owner name",
          icon = icon("user")
        )
      ),
      column(
        width = 6,
        shinyWidgets::pickerInput(
          inputId = ns("property_type"),
          label = "Property Type:",
          choices = c("Student", "Residential", "Commercial", "Mixed-Use"),
          options = list(style = "btn-primary")
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        shinyWidgets::pickerInput(
          inputId = ns("property_status"),
          label = "Property Status:",
          choices = c("Operational", "Under Construction", "Planned"),
          options = list(style = "btn-success")
        )
      ),
      column(
        width = 6,
        shinyWidgets::pickerInput(
          inputId = ns("product_type"),
          label = "Product Type:",
          choices = c("Mid-Rise", "High-Rise", "Townhouse", "Other"),
          options = list(style = "btn-info")
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        shinyWidgets::sliderTextInput(
          inputId = ns("property_rating"),
          label = "Property Rating:",
          choices = 1:5,
          selected = 3,
          grid = TRUE
        )
      ),
      column(
        width = 6,
        shinyWidgets::textInputIcon(
          inputId = ns("comp_status"),
          label = "Comp Status:",
          placeholder = "Enter comp status",
          icon = icon("check-circle")
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        shinyWidgets::numericInputIcon(
          inputId = ns("year_built"),
          label = htmltools::tags$span(icon("calendar-alt"), " Year Built/Renovated:"),
          value = 2021,
          min = 1970,
          max = 2024,
          step = 1,
          help_text = "Enter year built or renovated"
        )
      ),
      column(
        width = 6,
        shinyWidgets::textInputIcon(
          inputId = ns("recent_sale"),
          label = "Most Recent Sale Date:",
          placeholder = "Enter sale date",
          icon = icon("dollar-sign")
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        shinyWidgets::textInputIcon(
          inputId = ns("distance_from_campus"),
          label = "Distance From Campus:",
          placeholder = "Enter distance (e.g., 0.1 Miles)",
          icon = icon("ruler-horizontal")
        )
      )
    )
  )
}

property_summary_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Reactive values for property summary inputs
    property_summary <- reactive({
      list(
        property_name = input$property_name,
        property_website = input$property_website,
        property_address = input$property_address,
        property_phone = input$property_phone,
        developer = input$developer,
        manager = input$manager,
        owner = input$owner,
        property_type = input$property_type,
        property_status = input$property_status,
        product_type = input$product_type,
        property_rating = input$property_rating,
        comp_status = input$comp_status,
        year_built = input$year_built,
        recent_sale = input$recent_sale,
        distance_from_campus = input$distance_from_campus
      )
    })

    # Observe and print the input values for debugging
    observe({
      print(property_summary())
    })

    # Return the reactive values for other modules if needed
    return(property_summary)
  })
}

# app
ui <- fluidPage(
  property_summary_ui("property_summary")
)

server <- function(input, output, session) {
  property_summary_server("property_summary")
}

shinyApp(ui, server)
