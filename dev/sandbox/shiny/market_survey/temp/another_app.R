library(shiny)
library(bslib)
library(reactable)
library(rhandsontable)
library(DT)
library(apexcharter)
library(lubridate)

ui <- page_navbar(
  title = "Leasing Market Survey",
  theme = bs_theme(version = 5),

  # Overview Tab
  nav_panel(
    "Overview",
    layout_columns(
      value_box(
        title = "Properties",
        value = textOutput("property_count"),
        showcase = bsicons::bs_icon("building")
      ),
      value_box(
        title = "Competitors",
        value = textOutput("competitor_count"),
        showcase = bsicons::bs_icon("graph-up")
      ),
      value_box(
        title = "Survey Responses",
        value = textOutput("response_count"),
        showcase = bsicons::bs_icon("clipboard-data")
      )
    ),

    # Property Summary as a vertical card
    card(
      card_header("Property Summary"),
      card(
        card_header("Basic Information"),
        textInput("property_name", "Property Name", value = "1077 Commonwealth"),
        textInput("website", "Website"),
        textInput("address", "Address"),
        textInput("phone", "Phone")
      ),
      card(
        card_header("Management Information"),
        textInput("developer", "Developer"),
        textInput("manager", "Manager"),
        textInput("owner", "Owner")
      ),
      card(
        card_header("Property Details"),
        selectInput("property_type", "Property Type",
                    choices = c("Student", "Conventional", "Affordable", "Innovative")),
        sliderInput("property_rating", "Property Rating", 1, 5, 3, step = 1),
        selectInput("property_status", "Property Status",
                    choices = c("New Construction", "Operational", "Undergoing Renovation")),
        selectInput("comp_status", "Comp Status",
                    choices = c("Subject Property", "Tier 1", "Tier 2"))
      ),
      card(
        card_header("Additional Information"),
        numericInput("year_built", "Year Built", value = 2020),
        dateInput("recent_sale", "Most Recent Sale"),
        sliderInput("distance", "Distance from Campus (miles)", 0, 10, 2, step = 0.1)
      )
    )
  ),

  # Leasing Tab
  nav_panel(
    "Leasing",
    card(
      card_header("Leasing Summary"),
      layout_columns(
        dateInput("lease_launch", "Lease Launch Date"),
        dateInput("renewal_launch", "Renewal Launch Date"),
        numericInput("current_occupancy", "Current Occupancy (%)", value = 95),
        numericInput("last_year_occupancy", "Last Year Occupancy (%)", value = 93),
        numericInput("current_prelease", "Current Pre-Lease (%)", value = 45),
        numericInput("last_year_prelease", "Last Year Pre-Lease (%)", value = 40),
        numericInput("total_renewals", "Total Renewals", value = 100),
        numericInput("total_new_leases", "Total New Leases", value = 50),
        numericInput("total_traffic", "Total Weekly Traffic", value = 25),
        selectInput("current_incentive", "Current Incentive",
                    choices = c("None", "Gift Card", "Monthly Concession", "One-Time Concession")),
        numericInput("incentive_amount", "Incentive Amount", value = 0),
        dateInput("data_updated", "Data Last Updated")
      )
    )
  ),

  # Amenities & Utilities Tab
  nav_panel(
    "Amenities & Utilities",
    layout_columns(
      card(
        card_header("Property Amenities"),
        rHandsontableOutput("amenities_table")
      ),
      card(
        card_header("Utilities Summary"),
        rHandsontableOutput("utilities_table")
      )
    )
  ),

  # Rents Tab
  nav_panel(
    "Rents",
    layout_columns(
      card(
        card_header("Rents by Floorplan"),
        rHandsontableOutput("rents_table")
      ),
      card(
        card_header("Average Rents by Unit Type"),
        apexchartOutput("rents_chart")
      )
    )
  ),

  # Notes & Hours Tab
  nav_panel(
    "Notes & Hours",
    layout_columns(
      card(
        card_header("Notes"),
        textAreaInput("leasing_notes", "Special Leasing Notes", height = "150px"),
        textAreaInput("property_notes", "Property Notes / Operational Changes", height = "150px")
      ),
      card(
        card_header("Office Hours"),
        rHandsontableOutput("hours_table")
      )
    )
  )
)

server <- function(input, output) {
  # Mock data for value boxes
  output$property_count <- renderText("25")
  output$competitor_count <- renderText("12")
  output$response_count <- renderText("150")

  # Initialize amenities table
  amenities_data <- reactive({
    data.frame(
      Amenity = c("Fitness Center", "Pool", "Parking", "Study Rooms", "Game Room"),
      Available = c("Yes", "Yes", "No", "Yes", "No"),
      stringsAsFactors = FALSE
    )
  })

  output$amenities_table <- renderRHandsontable({
    rhandsontable(amenities_data())
  })

  # Initialize utilities table
  utilities_data <- reactive({
    data.frame(
      Utility = c("Electricity", "Gas", "Water", "Internet"),
      Included = c("No", "Yes", "Yes", "No"),
      Cost = c(50, 0, 0, 75),
      stringsAsFactors = FALSE
    )
  })

  output$utilities_table <- renderRHandsontable({
    rhandsontable(utilities_data())
  })

  # Initialize rents table
  rents_data <- reactive({
    data.frame(
      Unit_Type = c("Studio", "1 Bed", "2 Bed", "3 Bed"),
      Count = c(10, 20, 30, 15),
      SF_Per_Bed = c(400, 600, 450, 400),
      Market_Rent = c(1200, 1500, 1100, 950),
      Concessions = c(0, 100, 50, 75),
      Effective_Rent = c(1200, 1400, 1050, 875),
      Additional_Expenses = c(100, 100, 100, 100),
      Availability = c(2, 5, 8, 3),
      stringsAsFactors = FALSE
    )
  })

  output$rents_table <- renderRHandsontable({
    rhandsontable(rents_data())
  })

  # Initialize office hours table
  hours_data <- reactive({
    data.frame(
      Day = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
      Open = c("9:00 AM", "9:00 AM", "9:00 AM", "9:00 AM", "9:00 AM", "10:00 AM", "12:00 PM"),
      Close = c("6:00 PM", "6:00 PM", "6:00 PM", "6:00 PM", "6:00 PM", "5:00 PM", "4:00 PM"),
      stringsAsFactors = FALSE
    )
  })

  output$hours_table <- renderRHandsontable({
    rhandsontable(hours_data())
  })

  # Render average rents chart
  output$rents_chart <- renderApexchart({
    data <- rents_data()
    apex(
      data = data,
      type = "bar",
      mapping = aes(x = Unit_Type, y = Market_Rent)
    ) %>%
      ax_title(text = "Average Market Rent by Unit Type") %>%
      ax_yaxis(title = list(text = "Rent ($)"))
  })
}

shinyApp(ui, server)
