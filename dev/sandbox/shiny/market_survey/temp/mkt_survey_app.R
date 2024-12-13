library(purrr)
library(shiny)
library(bslib)
library(shinyWidgets)
library(shinyjs)
library(reactable)
library(rhandsontable)
library(DT)
library(apexcharter)

# Initialize default data for tables
amenities_data <- data.frame(
  amenity_name = c("Fitness Center", "Pool", "Game Room", "Study Lounge", "Package Room"),
  available = c(TRUE, FALSE, TRUE, TRUE, TRUE),
  stringsAsFactors = FALSE
)

utilities_data <- data.frame(
  utility_type = c("Electricity", "Gas", "Water", "Sewer"),
  all_inclusive = c(FALSE, TRUE, TRUE, TRUE),
  cap = c(50, NA, NA, NA),
  allowance = c(75, NA, NA, NA),
  cost = c(100, 45, 35, 25),
  stringsAsFactors = FALSE
)

other_utilities_data <- data.frame(
  utility = c("Internet", "Trash Service", "Valet Trash", "Cable TV"),
  included = c(TRUE, TRUE, FALSE, FALSE),
  amount = c(65, 25, 35, 89),
  stringsAsFactors = FALSE
)

ui <- page_navbar(
  title = div(
    style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
    tags$img(src = "https://raw.githubusercontent.com/noclocks/gmhdatahub/e82374c372348060e900357859f7f3831a5397bb/inst/www/images/gmh/logos/gmh-logo.svg", height = "40px")
  ),
  theme = bs_theme(version = 5),

  nav_panel(
    "Market Survey",
    # [Previous code remains the same until navset_card_tab]

    navset_card_tab(
      title = span(bsicons::bs_icon("buildings"), "Property Detail: 1077 Commonwealth"),

      # Previous nav_panel remains the same
      nav_panel(
        title = span(bsicons::bs_icon("clipboard-data"), "Property & Leasing Summary"),
        layout_column_wrap(
          width = 1/2,
          style = "max-height: calc(100vh - 350px); overflow-y: auto;",

          # Property Summary
          card(
            card_header(
              span(bsicons::bs_icon("building"), "Property Summary"),
              class = "bg-light"
            ),
            card_body(
              class = "p-2",
              tags$div(
                style = "margin-bottom: 10px;",
                tags$label(bsicons::bs_icon("building-fill"), "Property Name", style = "font-size: 0.9rem;"),
                tags$div("1077 Commonwealth", style = "padding: 4px 8px; border: 1px solid #ccc; border-radius: 4px; background-color: #e9ecef;")
              ),
              div(
                class = "container-fluid p-0",
                style = "font-size: 0.9rem;",
                div(
                  class = "row g-2",
                  textInput("website", label = span(bsicons::bs_icon("globe"), "Website"), value = "http://www.1077commonwealth.com"),
                  textInput("address", label = span(bsicons::bs_icon("geo-alt"), "Address"), value = "1077 Commonwealth Ave, Boston, MA 02215"),
                  textInput("phone", label = span(bsicons::bs_icon("telephone"), "Phone"), value = "617-500-6481"),
                  selectInput("developer", label = span(bsicons::bs_icon("tools"), "Developer"), choices = c("BPDA", "Other"), selected = "BPDA"),
                  selectInput("manager", label = span(bsicons::bs_icon("person-workspace"), "Manager"), choices = c("GMH Communities", "Other"), selected = "GMH Communities"),
                  textInput("owner", label = span(bsicons::bs_icon("person"), "Owner"), value = "AGC + GMH Communities"),
                  selectInput("property_type", label = span(bsicons::bs_icon("houses"), "Property Type"), choices = c("Student", "Conventional", "Affordable", "Innovative"), selected = "Student"),
                  sliderInput("property_rating", label = span(bsicons::bs_icon("star"), "Property Rating"), min = 1, max = 5, value = 2, step = 1),
                  selectInput("property_status", label = span(bsicons::bs_icon("info-circle"), "Property Status"), choices = c("New Construction", "Operational", "Undergoing Renovation"), selected = "Operational"),
                  selectInput("comp_status", label = span(bsicons::bs_icon("diagram-2"), "Comp Status"), choices = c("Subject Property", "Tier 1", "Tier 2"), selected = "Subject Property"),
                  numericInput("year_built", label = span(bsicons::bs_icon("calendar"), "Year Built"), value = 2017),
                  dateInput("most_recent_sale", label = span(bsicons::bs_icon("calendar-date"), "Most Recent Sale"), value = "2019-01-01"),
                  sliderInput("distance", label = span(bsicons::bs_icon("map"), "Distance from Campus (miles)"), min = 0, max = 5, value = 0.1, step = 0.1)
                )
              )
            )
          ),

          # Leasing Summary
          card(
            card_header(
              span(bsicons::bs_icon("file-earmark-text"), "Leasing Summary"),
              class = "bg-light"
            ),
            card_body(
              class = "p-2",
              div(
                class = "container-fluid p-0",
                style = "font-size: 0.9rem;",
                div(
                  class = "row g-2",
                  selectInput("reporting_cycle", label = span(bsicons::bs_icon("calendar-week"), "Reporting Cycle"), choices = c("Saturday-Friday", "Sunday-Saturday"), selected = "Saturday-Friday"),
                  dateInput("lease_launch_date", label = span(bsicons::bs_icon("calendar-check"), "Lease Launch Date"), value = "2023-11-27"),
                  dateInput("renewal_launch_date", label = span(bsicons::bs_icon("calendar-plus"), "Renewal Launch Date"), value = "2023-10-23"),
                  numericInput("current_occupancy", label = span(bsicons::bs_icon("percent"), "Current Occupancy (%)"), value = 99.5),
                  numericInput("last_year_occupancy", label = span(bsicons::bs_icon("clock-history"), "Last Year Occupancy (%)"), value = 0),
                  numericInput("current_prelease", label = span(bsicons::bs_icon("graph-up"), "Current Pre-Lease (%)"), value = 38.8),
                  numericInput("last_year_prelease", label = span(bsicons::bs_icon("clock-history"), "Last Year Pre-Lease (%)"), value = NA),
                  numericInput("total_renewals", label = span(bsicons::bs_icon("arrow-repeat"), "Total Renewals"), value = 51),
                  numericInput("total_new_leases", label = span(bsicons::bs_icon("file-earmark-plus"), "Total New Leases"), value = 20),
                  numericInput("weekly_traffic", label = span(bsicons::bs_icon("people"), "Total Weekly Traffic"), value = 58),
                  selectInput("current_incentive", label = span(bsicons::bs_icon("gift"), "Current Incentive"), choices = c("None", "Gift Card", "Monthly Concession", "One-Time Concession"), selected = "None"),
                  numericInput("incentive_amount", label = span(bsicons::bs_icon("cash-stack"), "Incentive Amount ($)"), value = 0),
                  dateInput("data_last_updated", label = span(bsicons::bs_icon("clock"), "Data Last Updated"), value = "2024-02-22")
                )
              )
            )
          )
        )
      ),

      # New nav_panel for Amenities & Utilities
      nav_panel(
        title = span(bsicons::bs_icon("house-gear"), "Amenities & Utilities"),
        layout_column_wrap(
          width = 1/1,
          style = "max-height: calc(100vh - 350px); overflow-y: auto;",

          card(
            card_header(
              span(bsicons::bs_icon("list-check"), "Amenities"),
              class = "bg-light"
            ),
            card_body(
              class = "p-2",
              rHandsontableOutput("amenities_table")
            )
          ),

          card(
            card_header(
              span(bsicons::bs_icon("lightning-charge"), "Utilities Summary"),
              class = "bg-light"
            ),
            card_body(
              class = "p-2",
              rHandsontableOutput("utilities_table")
            )
          ),

          card(
            card_header(
              span(bsicons::bs_icon("tools"), "Other Utilities"),
              class = "bg-light"
            ),
            card_body(
              class = "p-2",
              rHandsontableOutput("other_utilities_table")
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Reactive values for tables
  amenities_rv <- reactiveVal(amenities_data)
  utilities_rv <- reactiveVal(utilities_data)
  other_utilities_rv <- reactiveVal(other_utilities_data)

  # Render Amenities table
  output$amenities_table <- renderRHandsontable({
    rhandsontable(amenities_rv(), stretchH = "all") %>%
      hot_col("amenity_name", "Amenity Name") %>%
      hot_col("available", "Available", type = "checkbox")
  })

  # Render Utilities table
  output$utilities_table <- renderRHandsontable({
    rhandsontable(utilities_rv(), stretchH = "all") %>%
      hot_col("utility_type", "Utility Type") %>%
      hot_col("all_inclusive", "All Inclusive", type = "checkbox") %>%
      hot_col("cap", "Cap", type = "numeric") %>%
      hot_col("allowance", "Allowance", type = "numeric") %>%
      hot_col("cost", "Cost", type = "numeric")
  })

  # Render Other Utilities table
  output$other_utilities_table <- renderRHandsontable({
    rhandsontable(other_utilities_rv(), stretchH = "all") %>%
      hot_col("utility", "Utility") %>%
      hot_col("included", "Included", type = "checkbox") %>%
      hot_col("amount", "Amount", type = "numeric")
  })

  # Observers for table changes
  observeEvent(input$amenities_table, {
    if (!is.null(input$amenities_table)) {
      amenities_rv(hot_to_r(input$amenities_table))
    }
  })

  observeEvent(input$utilities_table, {
    if (!is.null(input$utilities_table)) {
      utilities_rv(hot_to_r(input$utilities_table))
    }
  })

  observeEvent(input$other_utilities_table, {
    if (!is.null(input$other_utilities_table)) {
      other_utilities_rv(hot_to_r(input$other_utilities_table))
    }
  })
}

shinyApp(ui, server)
