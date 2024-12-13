library(shiny)
library(bslib)

# Theme customization
theme <- bs_theme(bootswatch = "flatly")

ui <- fluidPage(
  theme = theme,

  titlePanel("GMH Communities - Market Survey"),

  # Progress bar section
  fluidRow(
    column(
      width = 12,
      h3("Completion Progress"),
      tags$div(
        class = "progress",
        style = "height: 25px;",
        tags$div(
          class = "progress-bar",
          style = "width: 75%;",
          "75%"
        )
      )
    )
  ),

  # Key metrics as value boxes
  fluidRow(
    column(
      width = 3,
      tags$div(
        class = "card text-center",
        h4("Total Units"),
        tags$h2("180"),
        tags$i(class = "fas fa-building fa-2x")
      )
    ),
    column(
      width = 3,
      tags$div(
        class = "card text-center",
        h4("Average Rent"),
        tags$h2("$2,500"),
        tags$i(class = "fas fa-dollar-sign fa-2x")
      )
    ),
    column(
      width = 3,
      tags$div(
        class = "card text-center",
        h4("Occupancy Rate"),
        tags$h2("95%"),
        tags$i(class = "fas fa-percent fa-2x")
      )
    )
  ),

  # Property & Leasing Summary section
  fluidRow(
    column(
      width = 6,
      tags$h4("Property Summary"),
      wellPanel(
        fluidRow(
          column(6, textInput("property_name", "Property Name", "1077 Commonwealth")),
          column(6, textInput("address", "Address", "http://www.1077commonwealth.com"))
        ),
        fluidRow(
          column(6, textInput("developer", "Developer", "BPDA")),
          column(6, textInput("manager", "Manager", "GMH Communities"))
        )
      )
    ),
    column(
      width = 6,
      tags$h4("Leasing Summary"),
      wellPanel(
        fluidRow(
          column(6, textInput("reporting_cycle", "Reporting Cycle", "Saturday-Friday")),
          column(6, textInput("lease_launch_date", "Lease Launch Date", "2023-11-27"))
        ),
        fluidRow(
          column(6, numericInput("occupancy_rate", "Occupancy Rate (%)", 95)),
          column(6, numericInput("pre_lease_rate", "Current Pre-Lease (%)", 38.8))
        )
      )
    )
  )
)

server <- function(input, output, session) {}

shinyApp(ui, server)
