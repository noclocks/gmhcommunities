
#  ------------------------------------------------------------------------
#
# Title : amenities Shiny Module
#    By : Jimmy Briggs
#  Date : 2024-11-27
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' amenities Shiny Module
#'
#' @name mod_amenities
#'
#' @description
#' A Shiny Module for ...
#'
#' - `mod_amenities_ui()`: User interface
#' - `mod_amenities_server()`: Server logic
#'
#' @param id Module's namespace ID.
#'
#' @return
#' - `mod_amenities_ui()`: UI HTML Output.
#' - `mod_amenities_server()`: Reactive values returned from server logic.
#'
#' @examples
#' if (interactive()) {
#'   mod_amenities_demo()
#' }
NULL


# UI ----------------------------------------------------------------------

#' @rdname mod_amenities
#' @export
#' @importFrom bslib card card_header card_body card_title layout_column_wrap
#' @importFrom htmltools tags
#' @importFrom rhandsontable rHandsontableOutput
#' @importFrom shiny NS actionButton icon
#' @importFrom shinycustomloader withLoader
#' @importFrom shinyWidgets noUiSliderInput
mod_amenities_ui <- function(id) {

  ns <- shiny::NS(id)

  common_area_rating_input <- shinyWidgets::noUiSliderInput(
    inputId = ns("common_area_rating"),
    label = icon_text("star", "Common Area Rating"),
    min = 0,
    max = 5,
    step = 0.5,
    value = 3,
    tooltips = TRUE,
    connect = TRUE
  )

  unit_rating_input <- shinyWidgets::noUiSliderInput(
    inputId = ns("unit_rating"),
    label = icon_text("star", "Unit Rating"),
    min = 0,
    max = 5,
    step = 0.5,
    value = 3,
    tooltips = TRUE,
    connect = TRUE
  )

  property_amenities_card <- bslib::card(
    bslib::card_header(
      class = "bg-dark",
      htmltools::tags$h5(icon_text("house", "Property Amenities"))
    ),
    bslib::card_body(
      shiny::actionButton(
        ns("submit_property_amenities"),
        "Submit Property Amenities",
        icon = shiny::icon("check"),
        disabled = TRUE,
        class = "btn-success"
      ),
      common_area_rating_input,
      bslib::card_title("Property Amenities Table"),
      rhandsontable::rHandsontableOutput(ns("property_amenities_table")) |>
        shinycustomloader::withLoader()
    )
  )

  unit_amenities_card <- bslib::card(
    full_screen = TRUE,
    bslib::card_header(
      class = "bg-dark",
      htmltools::tags$h5(icon_text("building", "Unit Amenities"))
    ),
    bslib::card_body(
      shiny::actionButton(
        ns("submit_unit_amenities"),
        "Submit Unit Amenities",
        icon = shiny::icon("check"),
        disabled = TRUE,
        class = "btn-success"
      ),
      unit_rating_input,
      bslib::card_title("Unit Amenities Table"),
      rhandsontable::rHandsontableOutput(ns("unit_amenities_table")) |>
        shinycustomloader::withLoader()
    )
  )

  bslib::layout_column_wrap(
    width = 1/2,
    property_amenities_card,
    unit_amenities_card
  )

}


# server ------------------------------------------------------------------

#' @rdname mod_amenities
#' @export
#' @importFrom cli cat_rule
#' @importFrom rhandsontable renderRHandsontable rhandsontable hot_to_r
#' @importFrom shiny moduleServer reactiveVal observeEvent
mod_amenities_server <- function(
    id,
    property_amenities = NULL,
    unit_amenities = NULL
) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      cli::cat_rule("mod_amenities_server")

      # Reactive values for tables
      property_amenities_rv <- shiny::reactiveVal(property_amenities)
      unit_amenities_rv <- shiny::reactiveVal(unit_amenities)

      # Render Property Amenities
      output$property_amenities_table <- rhandsontable::renderRHandsontable({
        rhandsontable::rhandsontable(
          property_amenities_rv(),
          width = "100%"
        )
      })

      # Render Unit Amenities
      output$unit_amenities_table <- rhandsontable::renderRHandsontable({
        rhandsontable::rhandsontable(
          unit_amenities_rv(),
          width = "100%"
        )
      })

      # Update reactive values on table edits
      shiny::observeEvent(input$property_amenities_table, {
        property_amenities_rv(rhandsontable::hot_to_r(input$property_amenities_table))
      })

      shiny::observeEvent(input$unit_amenities_table, {
        unit_amenities_rv(rhandsontable::hot_to_r(input$unit_amenities_table))
      })

      # Return all reactive values
      return(list(
        property_amenities = property_amenities_rv,
        unit_amenities = unit_amenities_rv
      ))

    }
  )
}


# demo --------------------------------------------------------------------

#' @rdname mod_amenities
#' @export
#' @importFrom pkgload load_all
#' @importFrom bslib page_fluid bs_theme
#' @importFrom shiny shinyApp
mod_amenities_demo <- function() {

  pkgload::load_all()

  common_area_rating_data <- tibble::tibble(
    Amenity = c("Common Area Rating"),
    Rating = c(3)
  )

  property_amenities_data <- tibble::tibble(
    Amenity = c(
      "University Shuttle", "Private Shuttle", "Limited Access Gates",
      "Fitness Center", "Computer Lounge", "Game Room", "Spray Tanning",
      "UV Tanning", "Pool", "Hot Tub", "24hr Package System",
      "EV Charging Stations", "Car Sharing Services", "Smart Vending",
      "Mini Market", "Movie Theatre", "Co-Working/Study Spaces", "Free Printing",
      "Coffee Bar", "Retail", "Sauna/Spa", "Cycling/Yoga Studio",
      "Rentable Guest Suite", "Wellness Classes", "24hr Concierge",
      "Outdoor Grill Area", "Sand Volleyball Court", "Basketball Court",
      "Pets Allowed", "Dog Wash", "Dog Park"
    ),
    Available = rep(
      c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
      c(3L, 3L, 4L, 1L, 2L, 1L, 2L, 4L, 5L, 1L, 5L)
    ),
  )

  unit_amenities_data <- unit_amenities_data_lst$unit_amenities_availability

  ui <- bslib::page_fluid(
    title = "Demo",
    theme = bslib::bs_theme(version = 5),
    lang = "en",
    mod_amenities_ui("demo")
  )

  server <- function(input, output, session) {
    mod_amenities_server("demo", property_amenities = property_amenities_data, unit_amenities = unit_amenities_data)
  }

  shiny::shinyApp(ui, server)
}

