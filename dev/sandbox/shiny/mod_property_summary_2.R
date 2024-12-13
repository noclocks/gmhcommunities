library(shiny)
library(shinyWidgets)
library(tibble)
library(dplyr)
library(purrr)
library(rlang)

# UI Module
property_summary_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("dynamic_property_summary_ui"))
}

# Server Module
property_summary_server <- function(id, property_summary_inputs) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Render Dynamic Inputs
    output$dynamic_property_summary_ui <- renderUI({
      property_summary_inputs |>
        arrange(order) |>
        pmap(function(name, id, label, placeholder, icon, icon_function,
                      help, type, input_function, required, choices, default, prepopulate, prepopulate_method) {
          # Dynamic input generation
          input_id <- ns(id)
          input_args <- list(
            inputId = input_id,
            label = label,
            placeholder = placeholder,
            value = if (prepopulate) default else NULL
          )

          # Add specific properties
          if (!is.null(icon)) {
            input_args$icon <- do.call(eval(parse(text = icon_function)), list(icon))
          }

          if (!is.null(choices)) {
            input_args$choices <- choices
          }

          # Handle specific input types
          switch(input_function,
                 `shinyWidgets::textInputIcon` = do.call(shinyWidgets::textInputIcon, input_args),
                 `shinyWidgets::radioGroupButtons` = do.call(shinyWidgets::radioGroupButtons, c(input_args, list(
                   selected = default,
                   direction = "horizontal",
                   individual = TRUE
                 ))),
                 `shinyWidgets::noUiSliderInput` = do.call(shinyWidgets::noUiSliderInput, c(input_args, list(
                   min = 1,
                   max = 5,
                   step = 1,
                   value = as.numeric(default)
                 ))),
                 `shinyWidgets::airYearpickerInput` = do.call(shinyWidgets::airYearpickerInput, c(input_args, list(
                   value = if (prepopulate) as.character(default) else NULL
                 ))),
                 `shinyWidgets::airDatepickerInput` = do.call(shinyWidgets::airDatepickerInput, c(input_args, list(
                   value = if (prepopulate) as.character(default) else NULL,
                   clearButton = TRUE
                 ))),
                 `shinyWidgets::numericInputIcon` = do.call(shinyWidgets::numericInputIcon, c(input_args, list(
                   value = as.numeric(default),
                   min = 0
                 )))
          )
        })
    })

    # Collect all input values
    reactive({
      property_summary_inputs |>
        arrange(order) |>
        pull(id) |>
        setNames(property_summary_inputs$id) |>
        lapply(function(x) input[[ns(x)]])
    })
  })
}

# Define UI
ui <- fluidPage(
  titlePanel("Dynamic Property Summary"),
  sidebarLayout(
    sidebarPanel(
      property_summary_ui("property_summary")
    ),
    mainPanel(
      verbatimTextOutput("summary_values")
    )
  )
)

# Define Server
server <- function(input, output, session) {
  # Dynamic choices for select/radio inputs
  market_survey_input_choices <- list(
    property_summary = list(
      property_type = c("Student", "Conventional", "Affordable", "Innovative"),
      property_status = c("Operational", "New Construction", "Undergoing Renovations"),
      product_type = c("High-Rise", "Mid-Rise", "Wrap", "Garden", "Cottage", "SFR"),
      comp_status = c("Subject Property", "Tier 1", "Tier 2")
    )
  )

  # Populate property summary inputs tibble
  property_summary_inputs <- tibble::tibble(
    name = c(
      "Property Name", "Property Website", "Property Address",
      "Property Phone Number", "Property Developer", "Property Manager",
      "Property Owner", "Property Type", "Property Status", "Product Type",
      "Property Rating", "Comp Status", "Year Built/Renovated",
      "Most Recent Sale Date", "Distance to Campus"
    )
  ) |>
    dplyr::mutate(
      id = snakecase::to_snake_case(name),
      order = 1:nrow(property_summary_inputs),
      label = name,
      placeholder = paste("Enter", name),
      icon = c(
        "building", "globe", "map-marker-alt", "phone", "hard-hat", "user-tie",
        "user", "check", "building", "star", "check-circle", "calendar-alt",
        "dollar-sign", "ruler-horizontal", "map-marker-alt"
      ),
      icon_function = rep("shiny::icon", nrow(property_summary_inputs)),
      help = c(
        "Enter the name of the property.", "Enter the website of the property.",
        "Enter the address of the property.", "Enter the phone number of the property.",
        "Enter the developer of the property.", "Enter the manager of the property.",
        "Enter the owner of the property.", "Select the type of property.",
        "Select the status of the property.", "Select the product type of the property.",
        "Enter the rating of the property.", "Select the comp status of the property.",
        "Enter the year the property was built.", "Enter the most recent sale date of the property.",
        "Enter the distance from campus in miles."
      ),
      type = c(
        "text", "text", "text", "text", "text", "text", "text", "mc", "mc", "mc",
        "numeric", "mc", "date", "date", "numeric"
      ),
      input_function = c(
        "shinyWidgets::textInputIcon", "shinyWidgets::textInputIcon",
        "shinyWidgets::textInputIcon", "shinyWidgets::textInputIcon",
        "shinyWidgets::textInputIcon", "shinyWidgets::textInputIcon",
        "shinyWidgets::textInputIcon", "shinyWidgets::radioGroupButtons",
        "shinyWidgets::radioGroupButtons", "shinyWidgets::radioGroupButtons",
        "shinyWidgets::noUiSliderInput", "shinyWidgets::radioGroupButtons",
        "shinyWidgets::airYearpickerInput", "shinyWidgets::airDatepickerInput",
        "shinyWidgets::numericInputIcon"
      ),
      required = c(
        TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE,
        TRUE, TRUE, TRUE, FALSE, TRUE
      ),
      choices = list(
        NULL, NULL, NULL, NULL, NULL, NULL, NULL,
        market_survey_input_choices$property_summary$property_type,
        market_survey_input_choices$property_summary$property_status,
        market_survey_input_choices$property_summary$product_type,
        NULL, market_survey_input_choices$property_summary$comp_status,
        NULL, NULL, NULL
      ),
      default = c(
        NA, NA, NA, NA, NA, "GMH Communities", "GMH Communities",
        "Student", "Operational", "Mid-Rise", "3", "Subject Property", NA, NA, NA
      ),
      prepopulate = c(
        TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE
      ),
      prepopulate_method = c(
        "prior", "prior", "prior", "prior", "prior", "prior", "prior",
        "prior", "prior", "prior", "prior", "prior", "prior", "current", "prior"
      )
    )

  # Call Module
  summary_values <- property_summary_server("property_summary", property_summary_inputs)

  # Display Collected Values
  output$summary_values <- renderPrint({
    summary_values()
  })
}

# Run App
shinyApp(ui, server)
