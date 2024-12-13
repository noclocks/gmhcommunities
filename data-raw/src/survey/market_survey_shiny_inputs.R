

# property summary --------------------------------------------------------

property_summary_inputs <- tibble::tibble(
  name = c(
    "Property Name",
    "Property Website",
    "Property Address",
    "Property Phone Number",
    "Property Developer",
    "Property Manager",
    "Property Owner",
    "Property Type",
    "Property Status",
    "Product Type",
    "Property Rating",
    "Comp Status",
    "Year Built/Renovated",
    "Most Recent Sale Date",
    "Distance to Campus"
  )
)


property_summary_inputs <- property_summary_inputs |>
  dplyr::mutate(
    id = snakecase::to_snake_case(name),
    order = 1:nrow(property_summary_inputs),
    label = name,
    placeholder = paste("Enter", name),
    icon = c(
      "building",
      "globe",
      "map-marker-alt",
      "phone",
      "hard-hat",
      "user-tie",
      "user",
      "check",
      "building",
      "star",
      "check-circle",
      "calendar-alt",
      "dollar-sign",
      "ruler-horizontal",
      "map-marker-alt"
    ),
    icon_function = rep("shiny::icon", nrow(property_summary_inputs)),
    help = c(
      "Enter the name of the property.",
      "Enter the website of the property.",
      "Enter the address of the property.",
      "Enter the phone number of the property.",
      "Enter the developer of the property.",
      "Enter the manager of the property.",
      "Enter the owner of the property.",
      "Select the type of property.",
      "Select the status of the property.",
      "Select the product type of the property.",
      "Enter the rating of the property.",
      "Select the comp status of the property.",
      "Enter the year the property was built.",
      "Enter the most recent sale date of the property.",
      "Enter the distance from campus in miles."
    ),
    type = c(
      "text",    # Property Name
      "text",    # Property Website
      "text",    # Property Address
      "text",    # Property Phone Number
      "text",    # Property Developer
      "text",    # Property Manager
      "text",    # Property Owner
      "mc",      # Property Type
      "mc",      # Property Status
      "mc",      # Product Type
      "numeric", # Property Rating
      "mc",      # Comp Status
      "date",    # Year Built
      "date",    # Most Recent Sale Date
      "numeric"  # Distance to Campus
    ),
    input_function = c(
      "shinyWidgets::textInputIcon",
      "shinyWidgets::textInputIcon",
      "shinyWidgets::textInputIcon",
      "shinyWidgets::textInputIcon",
      "shinyWidgets::textInputIcon",
      "shinyWidgets::textInputIcon",
      "shinyWidgets::textInputIcon",
      "shinyWidgets::radioGroupButtons",
      "shinyWidgets::radioGroupButtons",
      "shinyWidgets::radioGroupButtons",
      "shinyWidgets::noUiSliderInput",
      "shinyWidgets::radioGroupButtons",
      "shinyWidgets::airYearpickerInput",
      "shinyWidgets::airDatepickerInput",
      "shinyWidgets::numericInputIcon"
    ),
    required = c(
      TRUE,  # Property Name
      FALSE, # Property Website
      TRUE,  # Property Address
      TRUE,  # Property Phone Number
      FALSE, # Property Developer
      FALSE, # Property Manager
      FALSE, # Property Owner
      TRUE,  # Property Type
      TRUE,  # Property Status
      TRUE,  # Product Type
      TRUE,  # Property Rating
      TRUE,  # Comp Status
      TRUE,  # Year Built
      FALSE, # Most Recent Sale Date
      TRUE   # Distance to Campus
    ),
    choices = c(
      list(NULL), # Property Name
      list(NULL), # Property Website
      list(NULL), # Property Address
      list(NULL), # Property Phone Number
      list(NULL), # Property Developer
      list(NULL), # Property Manager
      list(NULL), # Property Owner
      list(market_survey_input_choices$property_summary$property_type),
      list(market_survey_input_choices$property_summary$property_status),
      list(market_survey_input_choices$property_summary$product_type),
      list(NULL), # Property Rating
      list(market_survey_input_choices$property_summary$comp_status),
      list(NULL), # Year Built
      list(NULL), # Most Recent Sale Date
      list(NULL)  # Distance to Campus
    ),
    default = c(
      NA, # Property Name
      NA, # Property Website
      NA, # Property Address
      NA, # Property Phone Number
      NA, # Property Developer
      "GMH Communities", # Property Manager
      "GMH Communities", # Property Owner
      "Student", # Property Type
      "Operational", # Property Status
      "Mid-Rise", # Product Type
      "3", # Property Rating
      "Subject Property", # Comp Status
      NA, # Year Built
      NA, # Most Recent Sale Date
      NA  # Distance to Campus
    ),
    prepopulate = c(
      TRUE,  # Property Name
      TRUE,  # Property Website
      TRUE,  # Property Address
      TRUE,  # Property Phone Number
      TRUE,  # Property Developer
      TRUE,  # Property Manager
      TRUE,  # Property Owner
      TRUE,  # Property Type
      TRUE,  # Property Status
      TRUE,  # Product Type
      TRUE,  # Property Rating
      TRUE,  # Comp Status
      TRUE,  # Year Built
      FALSE, # Most Recent Sale Date
      TRUE   # Distance to Campus
    ),
    prepopulate_method = c(
      "prior", # Property Name
      "prior", # Property Website
      "prior", # Property Address
      "prior", # Property Phone Number
      "prior", # Property Developer
      "prior", # Property Manager
      "prior", # Property Owner
      "prior", # Property Type
      "prior", # Property Status
      "prior", # Product Type
      "prior", # Property Rating
      "prior", # Comp Status
      "prior", # Year Built
      "current", # Most Recent Sale Date
      "prior"  # Distance to Campus
    )
  )

property_summary_inputs |>
  dplyr::arrange(order) |>
  purrr::pmap(
    function(name, id, label, placeholder, icon, icon_function, help, type, input_function, required, choices, default, prepopulate, prepopulate_method) {

      # Dynamic input generation
      input_id <- id
      input_args <- list(
        inputId = input_id,
        label = label,
        placeholder = placeholder,
        value = if (prepopulate) default else NULL
      )

      list(
        name = name,
        id = id,
        label = label,
        placeholder = placeholder,
        icon = icon,
        icon_function = icon_function,
        help = help,
        type = type,
        input_function = input_function,
        required = required,
        choices = choices,
        default = default,
        prepopulate = prepopulate,
        prepopulate_method = prepopulate_method
      )
    }
  )



# create a function to derive the input from its id
get_property_summary_input <- function(id) {
  input_data <- property_summary_inputs |>
    dplyr::filter(.data$id == .env$id)

  input_func_str <- input_data$input_function
  pkg_name <- strsplit(input_func_str, "::")[[1]][1]
  requireNamespace(pkg_name)
  func_name <- strsplit(input_func_str, "::")[[1]][2]

  input_func <- getExportedValue(pkg_name, func_name)

  icon_func_str <- input_data$icon_function
  icon_pkg_name <- strsplit(icon_func_str, "::")[[1]][1]
  requireNamespace(icon_pkg_name)
  icon_func_name <- strsplit(icon_func_str, "::")[[1]][2]

  icon_func <- getExportedValue(icon_pkg_name, icon_func_name)

  input_id <- input_data$id
  input_label <- input_data$label
  input_placeholder <- input_data$placeholder
  input_icon <- input_data$icon
  input_required <- input_data$required
  input_choices <- input_data$choices
  input_default <- input_data$default

  actual_input_icon <- icon_func(input_icon)
  actual_input_label <- htmltools::tags$span(actual_input_icon, input_label)

  input_args_initial <- list(
    inputId = input_id,
    label = actual_input_label,
    placeholder = input_placeholder,
    icon = actual_input_icon,
    choices = unlist(input_choices),
    value = input_default,
    selected = input_default
  )

  func_args <- names(formals(input_func))

  input_args <- input_args_initial[names(input_args_initial) %in% func_args]

  do.call(input_func, input_args)
}


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


# property / competitor ---------------------------------------------------

market_survey_property_choices <- list(
  "1047 Commmonwealth" = "commonwealth",
  "1330 Boylston" = "boylston",
  "Van Ness" = "vanness",
  "Bower" = "bower"
)

market_survey_property_choice_options <- list(
  disabled = c(
    FALSE,
    FALSE,
    FALSE,
    FALSE
  ),
  icon = c(
    "fa-solid fa-building", # property icon
    "fa-solid fa-building-flag", # competitor icon
    "fa-solid fa-building-flag",
    "fa-solid fa-building-flag"
  ),
  subtext = c(
    "Property",
    "Competitor",
    "Competitor",
    "Competitor"
  )
)

market_survey_property_picker_options <- shinyWidgets::pickerOptions(
  header = "Select a Property/Competitor",
  hideDelected = TRUE,
  actionsBox = TRUE,
  liveSearch = TRUE,
  noneSelectedText = "No Property Selected",
  selectOnTab = TRUE,
  showContent = TRUE,
  showIcon = TRUE,
  selectedTextFormat = "count > 3",
  iconBase = "fas"
)


# leasing period ----------------------------------------------------------



# trash -------------------------------------------------------------------

#     min = c(
#       NULL, # Property Name
#       NULL, # Property Website
#       NULL, # Property Address
#       NULL, # Property Phone Number
#       NULL, # Property Developer
#       NULL, # Property Manager
#       NULL, # Property Owner
#       NULL, # Property Type
#       NULL, # Property Status
#       NULL, # Product Type
#       0,    # Property Rating
#       NULL, # Comp Status
#       1970, # Year Built
#       as.Date("1970-01-01"), # Most Recent Sale Date
#       0  # Distance to Campus
#     ),
#     max = c(
#       NULL, # Property Name
#       NULL, # Property Website
#       NULL, # Property Address
#       NULL, # Property Phone Number
#       NULL, # Property Developer
#       NULL, # Property Manager
#       NULL, # Property Owner
#       NULL, # Property Type
#       NULL, # Property Status
#       NULL, # Product Type
#       5,    # Property Rating
#       NULL, # Comp Status
#       as.numeric(format(Sys.Date(), "%Y")), # Year Built
#       Sys.Date(), # Most Recent Sale Date
#       100  # Distance to Campus
#     ),
#     step = c(
#       NULL, # Property Name
#       NULL, # Property Website
#       NULL, # Property Address
#       NULL, # Property Phone Number
#       NULL, # Property Developer
#       NULL, # Property Manager
#       NULL, # Property Owner
#       NULL, # Property Type
#       NULL, # Property Status
#       NULL, # Product Type
#       0.5,  # Property Rating
#       NULL, # Comp Status
#       1,    # Year Built
#       1,    # Most Recent Sale Date
#       0.1   # Distance to Campus
#     ),
#
#
#     validation = c(
#       NULL, # Property Name
#       NULL, # Property Website
#       NULL, # Property Address
#       NULL, # Property Phone Number
#       NULL, # Property Developer
#       NULL, # Property Manager
#       NULL, # Property Owner
#       NULL, # Property Type
#       NULL, # Property Status
#       NULL, # Product Type
#       NULL, # Property Rating
#       NULL, # Comp Status
#       NULL, # Year Built
#       NULL, # Most Recent Sale Date
#       NULL  # Distance to Campus
#     )
#   )
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# property_summary_shiny_inputs <- tibble::tibble(
#   section = rep("property_summary", 14),
#   order = c(1:14),
#   id = c(
#     "property_name",
#     "property_website",
#     "property_address",
#     "property_phone",
#     "property_developer",
#     "property_manager",
#     "property_owner",
#     "property_status",
#     "product_type",
#     "property_rating",
#     "comp_status",
#     "year_built_or_renovated",
#     "most_recent_sale",
#     "distance_from_campus"
#   ),
#   name = c(
#     "Property Name",
#     "Property Website",
#     "Property Address",
#     "Property Phone",
#     "Property Developer",
#     "Property Manager",
#     "Property Owner",
#     "Property Status",
#     "Product Type",
#     "Property Rating",
#     "Comp Status",
#     "Year Built or Renovated",
#     "Most Recent Sale",
#     "Distance from Campus"
#   ),
#   label = c(
#     "Property Name",
#     "Property Website",
#     "Property Address",
#     "Property Phone",
#     "Property Developer",
#     "Property Manager",
#     "Property Owner",
#     "Property Status",
#     "Product Type",
#     "Property Rating",
#     "Comp Status",
#     "Year Built or Renovated",
#     "Most Recent Sale",
#     "Distance from Campus"
#   ),
#   placeholder = c(
#     "Enter Property Name",
#     "Enter Property Website",
#     "Enter Property Address",
#     "Enter Property Phone",
#     "Enter Property Developer",
#     "Enter Property Manager",
#     "Enter Property Owner",
#     "Select Property Status",
#     "Select Product Type",
#     "Enter Property Rating",
#     "Select Comp Status",
#     "Enter Year Built or Renovated",
#     "Enter Most Recent Sale",
#     "Enter Distance from Campus"
#   ),
#   icon = c(
#     "building",
#     "globe",
#     "map-marker-alt",
#     "phone",
#     "hard-hat",
#     "user-tie",
#     "user",
#     "check",
#     "building",
#     "star",
#     "check-circle",
#     "calendar-alt",
#     "dollar-sign",
#     "ruler-horizontal"
#   ),
#   type = c(
#     "text",    # Property Name
#     "text",    # Property Website
#     "text",    # Property Address
#     "text",    # Property Phone Number
#     "text",    # Property Developer
#     "text",    # Property Manager
#     "text",    # Property Owner
#     "picker",  # Property Status
#     "picker",  # Product Type
#     "slider",  # Property Rating
#     "picker",  # Comp Status
#     "numeric", # Year Built/Renovated
#     "date",    # Most Recent Sale Date
#     "numeric"  # Distance to Campus
#   ),
#   required = c(
#     TRUE,  # Property Name
#     TRUE,  # Property Website
#     TRUE,  # Property Address
#     TRUE,  # Property Phone Number
#     TRUE,  # Property Developer
#     TRUE,  # Property Manager
#     TRUE,  # Property Owner
#     TRUE,  # Property Status
#     TRUE,  # Product Type
#     TRUE,  # Property Rating
#     TRUE,  # Comp Status
#     TRUE,  # Year Built
#     TRUE,  # Most Recent Sale Date
#     TRUE   # Distance to Campus
#   ),
#   prepopulate = c(
#     TRUE,  # Property Name
#     TRUE,  # Property Website
#     TRUE,  # Property Address
#     TRUE,  # Property Phone Number
#     TRUE,  # Property Developer
#     TRUE,  # Property Manager
#     TRUE,  # Property Owner
#     TRUE,  # Property Status
#     TRUE,  # Product Type
#     TRUE,  # Property Rating
#     TRUE,  # Comp Status
#     TRUE,  # Year Built
#     FALSE, # Most Recent Sale Date
#     TRUE   # Distance to Campus
#   ),
#   prepopulate_method = c(
#     "prior", # Property Name
#     "prior", # Property Website
#     "prior", # Property Address
#     "prior", # Property Phone Number
#     "prior", # Property Developer
#     "prior", # Property Manager
#     "prior", # Property Owner
#     "prior", # Property Status
#     "prior", # Product Type
#     "prior", # Property Rating
#     "prior", # Comp Status
#     "prior", # Year Built
#     "current", # Most Recent Sale Date
#     "prior"  # Distance to Campus
#   ),
#   options = c(
#     NULL, # Property Name
#     NULL, # Property Website
#     NULL, # Property Address
#     NULL, # Property Phone Number
#     NULL, # Property Developer
#     NULL, # Property Manager
#     NULL, # Property Owner
#     market_survey_input_choices$property_summary$property_type, # Property Type
#     market_survey_input_choices$property_summary$property_status, # Property Status
#     market_survey_input_choices$property_summary$product_type, # Product Type
#     NULL, # Property Rating
#     market_survey_input_choices$property_summary$comp_status, # Comp Status
#     NULL, # Year Built
#     NULL, # Year Last Renovated
#     NULL, # Most Recent Sale Date
#     NULL  # Distance to Campus
#   ),
#   default = c(
#     NULL, # Property Name
#     NULL, # Property Website
#     NULL, # Property Address
#     NULL, # Property Phone Number
#     NULL, # Property Developer
#     NULL, # Property Manager
#     NULL, # Property Owner
#     "Student", # Property Type
#     "Operational", # Property Status
#     "Mid-Rise", # Product Type
#     NULL, # Property Rating
#     "Subject Property", # Comp Status
#     NULL, # Year Built
#     NULL, # Year Last Renovated
#     NULL, # Most Recent Sale Date
#     NULL  # Distance to Campus
#   ),
#   min = c(
#     NULL, # Property Name
#     NULL, # Property Website
#     NULL, # Property Address
#     NULL, # Property Phone Number
#     NULL, # Property Developer
#     NULL, # Property Manager
#     NULL, # Property Owner
#     NULL, # Property Type
#     NULL, # Property Status
#     NULL, # Product Type
#     0,    # Property Rating
#     NULL, # Comp Status
#     1970, # Year Built
#     1970, # Year Last Renovated
#     as.Date("1970-01-01"), # Most Recent Sale Date
#     NULL  # Distance to Campus
#   ),
#   max = c(
#     NULL, # Property Name
#     NULL, # Property Website
#     NULL, # Property Address
#     NULL, # Property Phone Number
#     NULL, # Property Developer
#     NULL, # Property Manager
#     NULL, # Property Owner
#     NULL, # Property Type
#     NULL, # Property Status
#     NULL, # Product Type
#     5,    # Property Rating
#     NULL, # Comp Status
#     as.numeric(format(Sys.Date(), "%Y")), # Year Built
#     as.numeric(format(Sys.Date(), "%Y")), # Year Last Renovated
#     Sys.Date(), # Most Recent Sale Date
#     NULL  # Distance to Campus
#   ),
#   step = c(
#     NULL, # Property Name
#     NULL, # Property Website
#     NULL, # Property Address
#     NULL, # Property Phone Number
#     NULL, # Property Developer
#     NULL, # Property Manager
#     NULL, # Property Owner
#     NULL, # Property Type
#     NULL, # Property Status
#     NULL, # Product Type
#     0.5,  # Property Rating
#     NULL, # Comp Status
#     1,    # Year Built
#     1,    # Year Last Renovated
#     1,    # Most Recent Sale Date
#     0.1   # Distance to Campus
#   ),
#   help = c(
#     "Enter the name of the property.",
#     "Enter the website of the property.",
#     "Enter the address of the property.",
#     "Enter the phone number of the property.",
#     "Enter the developer of the property.",
#     "Enter the manager of the property.",
#     "Enter the owner of the property.",
#     "Select the type of property.",
#     "Select the status of the property.",
#     "Select the product type of the property.",
#     "Enter the rating of the property.",
#     "Select the comp status of the property.",
#     "Enter the year the property was built.",
#     "Enter the year the property was last renovated.",
#     "Enter the most recent sale date of the property.",
#     "Enter the distance from campus in miles."
#   )
# )
#
#
#
#
#
#
#
#
#
#
#
# property_summary_inputs <- property_summary_inputs |>
#   dplyr::mutate(
#     order = 1:nrow(property_summary_inputs),
#     id = snakecase::to_snake_case(name),
#     label = name,
#     placeholder = paste("Enter", name),
#     icon = c(
#       "building",
#       "globe",
#       "map-marker-alt",
#       "phone",
#       "hard-hat",
#       "user-tie",
#       "user",
#       "check",
#       "building",
#       "star",
#       "check-circle",
#       "calendar-alt",
#       "dollar-sign",
#       "ruler-horizontal",
#       "map-marker-alt"
#     ),
#     type = c(
#       "text",    # Property Name
#       "text",    # Property Website
#       "text",    # Property Address
#       "text",    # Property Phone Number
#       "text",    # Property Developer
#       "text",    # Property Manager
#       "text",    # Property Owner
#       "mc",      # Property Type
#       "mc",      # Property Status
#       "mc",      # Product Type
#       "numeric", # Property Rating
#       "mc",      # Comp Status
#       "numeric", # Year Built
#       "date",    # Most Recent Sale Date
#       "numeric"  # Distance to Campus
#     ),
#     required = c(
#       TRUE,  # Property Name
#       TRUE, # Property Website
#       TRUE,  # Property Address
#       TRUE,  # Property Phone Number
#       TRUE, # Property Developer
#       TRUE, # Property Manager
#       TRUE, # Property Owner
#       TRUE,  # Property Type
#       TRUE,  # Property Status
#       TRUE,  # Product Type
#       TRUE,  # Property Rating
#       TRUE,  # Comp Status
#       TRUE,  # Year Built
#       TRUE, # Most Recent Sale Date
#       TRUE   # Distance to Campus
#     ),
#
#
#
#
#
#
#
#
#
#   type = c(
#     "text",    # Property Name
#     "text",    # Property Website
#     "text",    # Property Address
#     "text",    # Property Phone Number
#     "text",    # Property Developer
#     "text",    # Property Manager
#     "text",    # Property Owner
#     "mc",      # Property Type
#     "mc",      # Property Status
#     "mc",      # Product Type
#     "numeric", # Property Rating
#     "mc",      # Comp Status
#     "numeric", # Year Built
#     "numeric", # Year Last Renovated
#     "date",    # Most Recent Sale Date
#     "numeric"  # Distance to Campus
#   ),
#   id = c(
#     "property_name",
#     "property_website",
#     "property_address",
#     "property_phone_number",
#     "property_developer",
#     "property_manager",
#     "property_owner",
#     "property_type",
#     "property_status",
#     "product_type",
#     "property_rating",
#     "comp_status",
#     "year_built",
#     "year_last_renovated",
#     "most_recent_sale_date",
#     "distance_to_campus"
#   ),
#   label = c(
#     "Property Name",
#     "Property Website",
#     "Property Address",
#     "Property Phone Number",
#     "Property Developer",
#     "Property Manager",
#     "Property Owner",
#     "Property Type",
#     "Property Status",
#     "Product Type",
#     "Property Rating",
#     "Comp Status",
#     "Year Built",
#     "Year Last Renovated",
#     "Most Recent Sale Date",
#     "Distance to Campus"
#   ),
#   required = c(
#     TRUE,  # Property Name
#     FALSE, # Property Website
#     TRUE,  # Property Address
#     TRUE,  # Property Phone Number
#     FALSE, # Property Developer
#     FALSE, # Property Manager
#     FALSE, # Property Owner
#     TRUE,  # Property Type
#     TRUE,  # Property Status
#     TRUE,  # Product Type
#     TRUE,  # Property Rating
#     TRUE,  # Comp Status
#     TRUE,  # Year Built
#     FALSE, # Year Last Renovated
#     FALSE, # Most Recent Sale Date
#     TRUE   # Distance to Campus
#   ),
#   options = NULL
# )
#
# property_summary_inputs <- dplyr::mutate(
#   property_summary_inputs,
#   options = dplyr::case_when(
#     name == "Property Type" ~ list(c(
#       "Student" = "0",
#       "Conventional" = "1",
#       "Affordable" = "2",
#       "Innovative" = "3"
#     )),
#     name == "Property Status" ~ list(c(
#       "Operational" = "0",
#       "New Construction" = "1",
#       "Undergoing Renovations" = "2"
#     )),
#     name == "Product Type" ~ list(c(
#       "High-Rise" = "0",
#       "Mid-Rise" = "1",
#       "Wrap" = "2",
#       "Garden" = "3",
#       "Cottage" = "4",
#       "SFR" = "5"
#     )),
#     name == "Comp Status" ~ list(c(
#       "Subject Property" = "0",
#       "Tier 1" = "1",
#       "Tier 2" = "2"
#     )),
#     TRUE ~ list(NULL)
#   )
# )
#
# property_summary_questions <- purrr::map(
#   1:nrow(property_summary_inputs),
#   function(i) {
#     args <- property_summary_inputs[i, ]
#     surveydown::sd_question(
#       id = as.character(args$id),
#       type = args$type,
#       label = args$label,
#       option = unlist(args$options)
#     )
#   }
# )
