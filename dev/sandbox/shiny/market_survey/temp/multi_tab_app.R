library(shiny)
library(bslib)
library(reactable)
library(apexcharter)
library(dplyr)

# Sample data - In real app, this would come from a database
market_data <- data.frame(
  property = c("Property A", "Property B", "Property C", "Property D"),
  date = as.Date(c("2023-01-01", "2023-01-01", "2023-01-01", "2023-01-01")),
  market_rent = c(1500, 1600, 1400, 1700),
  effective_rent = c(1450, 1550, 1350, 1650),
  occupancy = c(95, 92, 98, 94),
  units = c(200, 150, 300, 250),
  stringsAsFactors = FALSE
)

ui <- page_sidebar(
  sidebar = sidebar(
    title = "Filters",
    dateRangeInput("date_range", "Select Date Range",
                   start = min(market_data$date),
                   end = max(market_data$date)),
    selectInput("property_filter", "Select Property",
                choices = c("All", unique(market_data$property))),
    sliderInput("rent_range", "Market Rent Range",
                min = 0, max = 2000, value = c(1000, 2000))
  ),

  navset_card_pill(
    nav_panel(
      title = "Market Data",
      reactableOutput("market_table")
    ),
    nav_panel(
      title = "Insights",
      layout_columns(
        col_widths = c(6, 6),
        card(
          apexchartOutput("rent_comparison")
        ),
        card(
          apexchartOutput("occupancy_chart")
        )
      )
    ),
    nav_panel(
      title = "Survey Results",
      card(
        card_header("Property Details"),
        layout_columns(
          col_widths = c(6, 6),
          card(
            textInput("property_name", "Property Name",
                      value = "1047 Commonwealth"),
            textInput("property_address", "Property Address",
                      value = "1047 Commonwealth Ave, Boston, MA 02215"),
            textInput("phone_number", "Phone Number"),
            textInput("developer", "Developer")
          ),
          card(
            textInput("manager", "Property Manager"),
            textInput("owner", "Owner"),
            selectInput("type", "Property Type",
                        choices = c("Garden", "Mid-Rise", "High-Rise", "Mixed-Use")),
            selectInput("status", "Property Status",
                        choices = c("Stabilized", "Lease-Up", "Under Construction", "Planned"))
          )
        ),
        actionButton("save_property_details", "Save Property Details",
                     class = "btn-primary mt-3")
      )
    ),
    nav_panel(
      title = "SWOT Analysis",
      layout_columns(
        col_widths = c(6, 6, 6, 6),
        card(
          card_header("Strengths"),
          style = "background-color: #d4edda;",
          textAreaInput("strengths", NULL, width = "100%", height = "150px",
                        value = "1. Prime location\n2. High occupancy rates\n3. Quality amenities")
        ),
        card(
          card_header("Weaknesses"),
          style = "background-color: #f8d7da;",
          textAreaInput("weaknesses", NULL, width = "100%", height = "150px",
                        value = "1. Aging infrastructure\n2. Limited parking\n3. Higher operating costs")
        ),
        card(
          card_header("Opportunities"),
          style = "background-color: #cce5ff;",
          textAreaInput("opportunities", NULL, width = "100%", height = "150px",
                        value = "1. Market expansion\n2. Renovation potential\n3. New target demographics")
        ),
        card(
          card_header("Threats"),
          style = "background-color: #fff3cd;",
          textAreaInput("threats", NULL, width = "100%", height = "150px",
                        value = "1. New competition\n2. Economic downturn\n3. Changing regulations")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Filtered data
  filtered_data <- reactive({
    data <- market_data

    if (input$property_filter != "All") {
      data <- data %>% filter(property == input$property_filter)
    }

    data %>%
      filter(market_rent >= input$rent_range[1],
             market_rent <= input$rent_range[2])
  })

  # Reactive table
  output$market_table <- renderReactable({
    reactable(
      filtered_data(),
      filterable = TRUE,
      sortable = TRUE,
      resizable = TRUE,
      defaultColDef = colDef(
        align = "left",
        minWidth = 100
      ),
      columns = list(
        date = colDef(
          format = list(
            cell = colFormat(date = TRUE)
          )
        ),
        market_rent = colDef(
          format = list(
            cell = colFormat(prefix = "$")
          )
        ),
        effective_rent = colDef(
          format = list(
            cell = colFormat(prefix = "$")
          )
        ),
        occupancy = colDef(
          format = list(
            cell = colFormat(suffix = "%")
          )
        )
      )
    )
  })

  # Rent Comparison Chart
  output$rent_comparison <- renderApexchart({
    apex(
      data = filtered_data(),
      mapping = aes(x = property, y = market_rent),
      type = "column"
    ) %>%
      ax_title(text = "Market vs Effective Rent") %>%
      ax_series(list(
        name = "Market Rent",
        data = filtered_data()$market_rent
      )) %>%
      ax_series(list(
        name = "Effective Rent",
        data = filtered_data()$effective_rent
      )) %>%
      ax_xaxis(title = list(text = "Property")) %>%
      ax_yaxis(title = list(text = "Rent ($)"))
  })

  # Occupancy Chart
  output$occupancy_chart <- renderApexchart({
    apex(
      data = filtered_data(),
      mapping = aes(x = property, y = occupancy),
      type = "column"
    ) %>%
      ax_title(text = "Occupancy Rates") %>%
      ax_xaxis(title = list(text = "Property")) %>%
      ax_yaxis(title = list(text = "Occupancy (%)"))
  })

  # Save property details
  observeEvent(input$save_property_details, {
    # Add logic here to save the property details
    showNotification("Property details saved successfully", type = "success")
  })
}

shinyApp(ui, server)
