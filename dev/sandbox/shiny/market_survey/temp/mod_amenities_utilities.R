amenitiesUtilitiesUI <- function(id) {
  ns <- NS(id)

  div(
    style = "width: 100%; gap: 20px; display: flex; flex-direction: column;",
    card(
      min_height = "250px",
      card_header(
        span(bs_icon("list-check"), "Amenities"),
        class = "bg-light"
      ),
      card_body(
        class = "p-3",
        div(style = "height: 200px;",
            rHandsontableOutput(ns("amenities_table"))
        )
      )
    ),

    card(
      min_height = "250px",
      card_header(
        span(bs_icon("lightning-charge"), "Utilities Summary"),
        class = "bg-light"
      ),
      card_body(
        class = "p-3",
        div(style = "height: 200px;",
            rHandsontableOutput(ns("utilities_table"))
        )
      )
    ),

    card(
      min_height = "250px",
      card_header(
        span(bs_icon("tools"), "Other Utilities"),
        class = "bg-light"
      ),
      card_body(
        class = "p-3",
        div(style = "height: 200px;",
            rHandsontableOutput(ns("other_utilities_table"))
        )
      )
    )
  )
}

amenitiesUtilitiesServer <- function(id, amenities_data, utilities_data, other_utilities_data) {
  moduleServer(id, function(input, output, session) {
    # Reactive values for tables
    amenities_rv <- reactiveVal(amenities_data)
    utilities_rv <- reactiveVal(utilities_data)
    other_utilities_rv <- reactiveVal(other_utilities_data)

    # Render Amenities table
    output$amenities_table <- renderRHandsontable({
      rhandsontable(amenities_rv(), rowHeaders = FALSE) %>%
        hot_table(stretchH = "all") %>%
        hot_col("amenity_name", "Amenity Name", readOnly = TRUE) %>%
        hot_col("available", "Available", type = "checkbox") %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
    })

    # Render Utilities table
    output$utilities_table <- renderRHandsontable({
      rhandsontable(utilities_rv(), rowHeaders = FALSE) %>%
        hot_table(stretchH = "all") %>%
        hot_col("utility_type", "Utility Type", readOnly = TRUE) %>%
        hot_col("all_inclusive", "All Inclusive", type = "checkbox") %>%
        hot_col("cap", "Cap ($)", type = "numeric", format = "$0,0.00") %>%
        hot_col("allowance", "Allowance ($)", type = "numeric", format = "$0,0.00") %>%
        hot_col("cost", "Cost ($)", type = "numeric", format = "$0,0.00") %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
    })

    # Render Other Utilities table
    output$other_utilities_table <- renderRHandsontable({
      rhandsontable(other_utilities_rv(), rowHeaders = FALSE) %>%
        hot_table(stretchH = "all") %>%
        hot_col("utility", "Utility", readOnly = TRUE) %>%
        hot_col("included", "Included", type = "checkbox") %>%
        hot_col("amount", "Amount ($)", type = "numeric", format = "$0,0.00") %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
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

    # Return reactive values
    list(
      amenities = amenities_rv,
      utilities = utilities_rv,
      other_utilities = other_utilities_rv
    )
  })
}


# library(shiny)
# library(bslib)
# library(rhandsontable)
# library(bsicons)
#
# # Initialize default data for tables
# amenities_data <- data.frame(
#   amenity_name = c("Fitness Center", "Pool", "Game Room", "Study Lounge", "Package Room"),
#   available = c(TRUE, FALSE, TRUE, TRUE, TRUE),
#   stringsAsFactors = FALSE
# )
#
# utilities_data <- data.frame(
#   utility_type = c("Electricity", "Gas", "Water", "Sewer"),
#   all_inclusive = c(FALSE, TRUE, TRUE, TRUE),
#   cap = c(50, NA, NA, NA),
#   allowance = c(75, NA, NA, NA),
#   cost = c(100, 45, 35, 25),
#   stringsAsFactors = FALSE
# )
#
# other_utilities_data <- data.frame(
#   utility = c("Internet", "Trash Service", "Valet Trash", "Cable TV"),
#   included = c(TRUE, TRUE, FALSE, FALSE),
#   amount = c(65, 25, 35, 89),
#   stringsAsFactors = FALSE
# )
#
#
# ui <- page_fluid(
#   theme = bs_theme(version = 5),
#
#   h1("Property Amenities & Utilities"),
#
#   div(
#     style = "max-width: 1200px; margin: 0 auto; padding: 20px;",
#     amenitiesUtilitiesUI("amenities_module")
#   )
# )
#
# server <- function(input, output, session) {
#   # Initialize the module
#   amenities_utils_data <- amenitiesUtilitiesServer(
#     "amenities_module",
#     amenities_data,
#     utilities_data,
#     other_utilities_data
#   )
# }
#
# shinyApp(ui, server)
