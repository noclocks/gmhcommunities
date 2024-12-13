
#  ------------------------------------------------------------------------
#
# Title : market_survey_overview Shiny Module
#    By : Jimmy Briggs
#  Date : 2024-11-27
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' market_survey_overview Shiny Module
#'
#' @name mod_market_survey_overview
#'
#' @description
#' A Shiny Module for ...
#'
#' - `mod_market_survey_overview_ui()`: User interface
#' - `mod_market_survey_overview_server()`: Server logic
#'
#' @param id Module's namespace ID.
#'
#' @return
#' - `mod_market_survey_overview_ui()`: UI HTML Output.
#' - `mod_market_survey_overview_server()`: Reactive values returned from server logic.
#'
#' @examples
#' if (interactive()) {
#'   mod_market_survey_overview_demo()
#' }
NULL


# UI ----------------------------------------------------------------------

#' @rdname mod_market_survey_overview
#' @export
#' @importFrom bsicons bs_icon
#' @importFrom bslib layout_column_wrap value_box layout_columns card card_header
#' @importFrom DT DTOutput
#' @importFrom htmltools tagList
#' @importFrom leaflet leafletOutput
#' @importFrom shiny NS textOutput
mod_market_survey_overview_ui <- function(id) {

  ns <- shiny::NS(id)

  htmltools::tagList(
    bslib::layout_column_wrap(
      width = 1/2,
      bslib::value_box(
        title = "Total Properties",
        value = shiny::textOutput(ns("total_properties")),
        showcase = bsicons::bs_icon("building")
      ),
      bslib::value_box(
        title = "Average Rating",
        value = shiny::textOutput(ns("avg_rating")),
        showcase = bsicons::bs_icon("star")
      )
    ),
    bslib::layout_columns(
      col_widths = c(8, 4),
      row_heights = c("auto", "auto"),
      bslib::card(
        id = ns("map_card"),
        padding = 0,
        min_height = "500px",
        full_screen = TRUE,
        bslib::card_header(icon_text("map", "Property Locations")),
        leaflet::leafletOutput(ns("map"), height = "600px") |>
          with_loader()
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Property Details"),
        DT::DTOutput(ns("table"))
      )
    )
  )

}


# server ------------------------------------------------------------------

#' @rdname mod_market_survey_overview
#' @export
#' @importFrom cli cat_rule cli_alert_info
#' @importFrom dplyr select
#' @importFrom DT renderDT datatable
#' @importFrom leaflet renderLeaflet awesomeIcons setView addLegend addAwesomeMarkers addTiles leaflet popupOptions
#' @importFrom shiny reactive is.reactive moduleServer observe renderText req
mod_market_survey_overview_server <- function(
    id,
    pool,
    selected_property = NULL,
    selected_leasing_week = NULL
) {

  # default property (commonwealth)
  if (is.null(selected_property)) {
    selected_property <- shiny::reactive({"739085"})
  }

  # validation
  stopifnot(shiny::is.reactive(selected_property))
  check_db_conn(pool)

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
      cli::cat_rule("[Module]: mod_market_survey_overview_server()")

      # selected property -------------------------------------------------------
      shiny::observe({
        prop <- selected_property()
        cli::cli_alert_info("Selected Property: {.field {prop}}")
      })

      # database locations ---------------------------------------------------
      db_locations <- shiny::reactive({
        prop_id <- selected_property()
        db_get_mkt_map_locations(conn = pool, property_id = prop_id)
      })

      # values ------------------------------------------------------------------

      # total properties
      output$total_properties <- shiny::renderText({
        shiny::req(db_locations())
        nrow(db_locations())
      })

      # average rating
      output$avg_rating <- shiny::renderText({
        shiny::req(db_locations())
        round(mean(db_locations()$gmaps_rating), 1)
      })

      # map ---------------------------------------------------------------------
      output$map <- leaflet::renderLeaflet({

        shiny::req(db_locations())

        map_data <- db_locations()

        # create custom icons
        icons <- leaflet::awesomeIcons(
          icon = map_data$map_marker_icon,
          iconColor = 'white',
          library = 'fa',
          markerColor = map_data$map_marker_color
        )

        leaflet::leaflet(map_data) |>
          leaflet::addTiles() |>
          leaflet::addAwesomeMarkers(
            ~longitude, ~latitude,
            icon = icons,
            popup = map_data$map_popup_html,
            popupOptions = leaflet::popupOptions(
              maxHeight = "calc(50vh - 80px)", # 50% of viewport height minus some  padding
              maxWidth = 300,
              autoPan = TRUE,
              keepInView = TRUE,
              closeButton = TRUE,
              closeOnClick = TRUE
            )
          ) |>
          leaflet::addLegend(
            position = "bottomright",
            colors = c("blue", "red"),
            labels = c("Subject Property", "Competitor"),
            title = "Property Type"
          ) |>
          leaflet::setView(
            lng = mean(map_data$longitude),
            lat = mean(map_data$latitude),
            zoom = 14
          )
      })

      # Table output
      output$table <- DT::renderDT({
        shiny::req(db_locations())
        db_locations() |>
          dplyr::select(
            Property = property_name,
            Address = address
          ) |>
          DT::datatable()
      })

      return(
        list(
          shiny::reactive({ db_locations() })
        )
      )

    }
  )
}


# demo --------------------------------------------------------------------

#' @rdname mod_market_survey_overview
#' @export
#' @importFrom bslib page_fluid bs_theme
#' @importFrom pkgload load_all
#' @importFrom shiny shinyApp
mod_market_survey_overview_demo <- function() {

  pkgload::load_all()

  pool <- db_connect()

  ui <- bslib::page_fluid(
    title = "Demo",
    theme = bslib::bs_theme(version = 5),
    lang = "en",
    mod_market_survey_overview_ui("demo")
  )

  server <- function(input, output, session) {
    mod_market_survey_overview_server("demo", pool)
  }

  shiny::shinyApp(ui, server)
}
