
#  ------------------------------------------------------------------------
#
# Title : amenities_utilities Shiny Module
#    By : Jimmy Briggs
#  Date : 2024-11-27
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' amenities_utilities Shiny Module
#'
#' @name mod_amenities_utilities
#'
#' @description
#' A Shiny Module for ...
#'
#' - `mod_amenities_utilities_ui()`: User interface
#' - `mod_amenities_utilities_server()`: Server logic
#'
#' @param id Module's namespace ID.
#'
#' @return
#' - `mod_amenities_utilities_ui()`: UI HTML Output.
#' - `mod_amenities_utilities_server()`: Reactive values returned from server logic.
#'
#' @examples
#' if (interactive()) {
#'   mod_amenities_utilities_demo()
#' }
NULL


# UI ----------------------------------------------------------------------

#' @rdname mod_amenities_utilities
#' @export
#' @importFrom shiny NS
#' @importFrom htmltools tagList tags
mod_amenities_utilities_ui <- function(id) {

  ns <- shiny::NS(id)

  bslib::layout_column_wrap(
    width = 1,
    gap = "20px",
    bslib::card(
      bslib::card_header(
        htmltools::tags$h5(icon_text("house", "Property Amenities"))
      ),
      bslib::card_body(
        rhandsontable::rHandsontableOutput(ns("property_amenities_table")) |>
          shinycustomloader::withLoader()
      )
    ),
    bslib::card(
      bslib::card_header(
        icon_text("building", "Unit Amenities")
      ),
      bslib::card_body(
        rhandsontable::rHandsontableOutput(ns("unit_amenities_table")) |>
          shinycustomloader::withLoader()
      )
    ),
    bslib::card(
      bslib::card_header(
        icon_text("lightning-charge", "Utilities Summary")
      ),
      bslib::card_body(
        rhandsontable::rHandsontableOutput(ns("utilities_summary_table")) |>
          shinycustomloader::withLoader()
      )
    ),
    bslib::card(
      bslib::card_header(
        icon_text("tools", "Other Utilities")
      ),
      bslib::card_body(
        rhandsontable::rHandsontableOutput(ns("other_utilities_table")) |>
          shinycustomloader::withLoader()
      )
    )
  )

}


# server ------------------------------------------------------------------

#' @rdname mod_amenities_utilities
#' @export
#' @importFrom shiny moduleServer
#' @importFrom cli cat_rule
mod_amenities_utilities_server <- function(
    id,
    property_amenities = NULL,
    unit_amenities = NULL,
    utilities_summary = NULL,
    other_utilities = NULL
) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      cli::cat_rule("mod_amenities_utilities_server")

      # Reactive values for tables
      property_amenities_rv <- shiny::reactiveVal(property_amenities)
      unit_amenities_rv <- shiny::reactiveVal(unit_amenities)
      utilities_summary_rv <- shiny::reactiveVal(utilities_summary)
      other_utilities_rv <- shiny::reactiveVal(other_utilities)

      # Render Property Amenities
      output$property_amenities_table <- rhandsontable::renderRHandsontable({
        rhandsontable::rhandsontable(property_amenities_rv())
      })

      # Render Unit Amenities
      output$unit_amenities_table <- rhandsontable::renderRHandsontable({
        rhandsontable::rhandsontable(unit_amenities_rv())
      })

      # Render Utilities Summary
      output$utilities_summary_table <- rhandsontable::renderRHandsontable({
        rhandsontable::rhandsontable(utilities_summary_rv())
      })

      # Render Other Utilities
      output$other_utilities_table <- rhandsontable::renderRHandsontable({
        rhandsontable::rhandsontable(other_utilities_rv())
      })

      # Update reactive values on table edits
      shiny::observeEvent(input$property_amenities_table, {
        property_amenities_rv(rhandsontable::hot_to_r(input$property_amenities_table))
      })

      shiny::observeEvent(input$unit_amenities_table, {
        unit_amenities_rv(rhandsontable::hot_to_r(input$unit_amenities_table))
      })

      shiny::observeEvent(input$utilities_summary_table, {
        utilities_summary_rv(rhandsontable::hot_to_r(input$utilities_summary_table))
      })

      shiny::observeEvent(input$other_utilities_table, {
        other_utilities_rv(rhandsontable::hot_to_r(input$other_utilities_table))
      })

      # Return all reactive values
      return(list(
        property_amenities = property_amenities_rv,
        unit_amenities = unit_amenities_rv,
        utilities_summary = utilities_summary_rv,
        other_utilities = other_utilities_rv
      ))
    }
  )
}

# demo --------------------------------------------------------------------

#' @rdname mod_amenities_utilities
#' @export
#' @importFrom pkgload load_all
#' @importFrom bslib page_fluid bs_theme
#' @importFrom shiny shinyApp
mod_amenities_utilities_demo <- function() {

  pkgload::load_all()

  property_amenities <- tibble::tibble(
    Amenity = c("Fitness Center", "Game Room", "Pool"),
    Available = c(TRUE, TRUE, FALSE)
  )

  unit_amenities <- tibble::tibble(
    Amenity = c("Walk-in Closets", "Private Bathrooms"),
    Available = c(TRUE, TRUE)
  )

  utilities_summary <- tibble::tibble(
    Utility = c("Electricity", "Gas", "Water"),
    All_Inclusive = c(FALSE, TRUE, TRUE),
    Amount = c(25, 0, 0)
  )

  other_utilities <- tibble::tibble(
    Utility = c("Internet", "Trash Service"),
    Amount = c(0, 0)
  )

  ui <- bslib::page_fillable(
    title = "Demo",
    theme = bslib::bs_theme(version = 5),
    lang = "en",
    mod_amenities_utilities_ui("demo")
  )

  server <- function(input, output, session) {
    mod_amenities_utilities_server(
      "demo",
      property_amenities,
      unit_amenities,
      utilities_summary,
      other_utilities
    )
  }

  shiny::shinyApp(ui, server)
}

# demo data ---------------------------------------------------------------





