
#  ------------------------------------------------------------------------
#
# Title : Pre-Lease Shiny Module
#    By : Jimmy Briggs
#  Date : 2024-11-27
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' Pre-Lease Shiny Module
#'
#' @name mod_pre_lease
#'
#' @description
#' A Shiny Module for the GMH Communities Pre-Lease Summary & Details reports
#' data visualization and review.
#'
#' The following functions are implemented:
#'
#' - `mod_pre_lease_ui()`: User interface (UI) definition
#' - `mod_pre_lease_server()`: Server logic
#' - `mod_pre_lease_demo()`: Demo application for the module
#'
#' @param id Module's namespace ID.
#' @param pool Database connection pool.
#' @param selected_property Selected property ID.
#' @param report_date Report date.
#'
#' @returns
#' - `mod_pre_lease_ui()`: UI HTML Output.
#' - `mod_pre_lease_server()`: List of reactive values.
#' - `mod_pre_lease_demo()`: `NULL`, used for the side-effect of a demo app.
#'
#' @examplesIf interactive()
#' mod_pre_lease_demo()
NULL

# UI ----------------------------------------------------------------------

#' @rdname mod_pre_lease
#' @export
mod_pre_lease_ui <- function(id) {

  ns <- shiny::NS(id)

  bslib::page_fluid(
    bslib::card(
      bslib::card_header(
        htmltools::tags$h1("Pre-Lease Summary", class = "text-center mb-4")
      )
    ),
    bslib::layout_columns(
      col_widths = c(4, 4, 4),
      bslib::value_box(
        title = "Average Occupancy %",
        value = shiny::textOutput(ns("value_avg_occupancy_pct"), inline = TRUE),
        showcase = bsicons::bs_icon("percent")
      ),
      bslib::value_box(
        title = "Average Pre-Lease %",
        value = shiny::textOutput(ns("value_avg_pre_lease_pct"), inline = TRUE),
        showcase = bsicons::bs_icon("percent")
      ),
      bslib::value_box(
        title = "Year-Over-Year Change (%)",
        value = shiny::textOutput(ns("value_yoy_pct_change"), inline = TRUE),
        showcase = bsicons::bs_icon("bar-chart-line")
      )
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      min_height = "400px",
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Current vs. Prior Pre-Lease Comparison", class = "bg-dark"),
        bslib::card_body(
          plotly::plotlyOutput(ns("pre_lease_comparison_chart"), height = "500px") |>
            with_loader()
        ),
        bslib::card_footer(

        )
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Beds Left by Property", class = "bg-dark"),
        bslib::card_body(
          apexcharter::apexchartOutput(ns("beds_left_by_property_chart"), height = "500px") |>
            with_loader()
        ),
        bslib::card_footer(

        )
      )
    ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        icon_text("table", "Pre-Lease Summary by Property"),
        shiny::downloadButton(ns("download"), "Export to Excel", class = "primary float-end"),
        class = "bg-dark"
      ),
      bslib::card_body(
        reactable::reactableOutput(ns("summary_table")) |>
          with_loader()
      ),
      bslib::card_footer(

      )
    )
  )

}


# server ------------------------------------------------------------------

#' @rdname mod_pre_lease
#' @export
#' @importFrom shiny moduleServer
#' @importFrom cli cat_rule
mod_pre_lease_server <- function(
  id,
  pool,
  selected_properties = NULL,
  report_date = NULL
) {

  # check database connection
  check_db_conn(pool)

  # validation of reactives
  if (!is.null(selected_properties)) {
    stopifnot(shiny::is.reactive(selected_properties))
  } else {
    selected_properties <- shiny::reactive({
      mem_get_entrata_property_ids()
    })
  }

  if (is.null(report_date)) {
    report_date <- shiny::reactive({ Sys.Date() })
  }

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      cli::cat_rule("[Module]: mod_pre_lease_server()")

      # initial database data ---------------------------------------------------

      pre_lease_summary_data <- reactive({

        shiny::req(selected_properties())

        property_ids <- selected_properties()

        db_get_pre_lease_summary_data(
          pool = pool,
          property_ids = property_ids
        )

      })

      # value boxes -------------------------------------------------------------

      output$value_avg_occupancy_pct <- shiny::renderText({
        shiny::req(pre_lease_summary_data())
        pre_lease_summary_data() |>
          dplyr::pull(current_occupancy) |>
          mean(na.rm = TRUE) |>
          scales::percent(accuracy = 0.1)
      })

      output$value_avg_pre_lease_pct <- shiny::renderText({
        shiny::req(pre_lease_summary_data())
        pre_lease_summary_data() |>
          dplyr::pull(current_preleased_percent) |>
          mean(na.rm = TRUE) |>
          scales::percent(accuracy = 0.1)
      })

      output$value_yoy_pct_change <- shiny::renderText({
        shiny::req(pre_lease_summary_data())
        pre_lease_summary_data() |>
          dplyr::pull(yoy_variance_percent) |>
          mean(na.rm = TRUE) |>
          scales::percent(accuracy = 0.1)
      })

      output$pre_lease_comparison_chart <- plotly::renderPlotly({

        shiny::req(pre_lease_summary_data())

        df <- pre_lease_summary_data() |>
          dplyr::collect()

        plotly::plot_ly(
          data = df
        ) |>
          plotly::add_trace(
            x = ~property_name,
            y = ~current_preleased_percent,
            type = "scatter",
            mode = "lines+markers",
            name = "Current Pre-Lease %",
            line = list(color = "#18BC9C")
          ) |>
          plotly::add_trace(
            x = ~property_name,
            y = ~prior_preleased_percent,
            type = "scatter",
            mode = "lines+markers",
            name = "Prior Pre-Lease %",
            line = list(color = "#2C3E50")
          ) |>
          plotly::layout(
            xaxis = list(tickangle = 45),
            yaxis = list(tickformat = ".0%"),
            showlegend = TRUE,
            margin = list(b = 100)
          )
      })

      output$beds_left_by_property_chart <- apexcharter::renderApexchart({

        shiny::req(pre_lease_summary_data())

        df <- pre_lease_summary_data() |>
          dplyr::collect()

        apexcharter::apex(
          data = df,
          mapping = list(
            x = "property_name",
            y = "beds_left"
          ),
          type = "bar"
        )

      })

      output$summary_table <- reactable::renderReactable({

        shiny::req(pre_lease_summary_data())

        df <- pre_lease_summary_data() |>
          dplyr::collect() |>
          dplyr::mutate(
            property_id = entrata_property_id
          ) |>
          dplyr::select(
            property_id,
            property_name,
            total_beds,
            current_preleased_percent,
            prior_preleased_percent,
            yoy_variance_percent,
            beds_left
          )

        reactable::reactable(
          data = df,
          filterable = TRUE,
          searchable = TRUE,
          bordered = TRUE,
          striped = TRUE,
          defaultSorted = list(property_name = "asc"),
          defaultPageSize = 25,
          columns = list(
            property_id = reactable::colDef(show = FALSE),
            property_name = reactable::colDef(
              header = "Property",
              sortable = TRUE,
              searchable = TRUE,
              minWidth = 150
            ),
            total_beds = reactable::colDef(
              header = "Total Beds",
              align = "right"
            ),
            current_preleased_percent = reactable::colDef(
              header = "Current Pre-Lease %",
              format = reactable::colFormat(percent = TRUE, digits = 1),
              style = function(value) {
                if (value >= 0.9) {
                  color <- "#18BC9C"
                } else if (value >= 0.7) {
                  color <- "#F39C12"
                } else {
                  color <- "#E74C3C"
                }
                list(color = color, fontWeight = "bold")
              }
            ),
            prior_preleased_percent = reactable::colDef(
              header = "Prior Pre-Lease %",
              format = reactable::colFormat(percent = TRUE, digits = 1)
            ),
            yoy_variance_percent = reactable::colDef(
              header = "YoY % Change",
              format = reactable::colFormat(percent = TRUE, digits = 1),
              style = function(value) {
                if (value >= 0) {
                  color <- "#18BC9C"
                } else {
                  color <- "#E74C3C"
                }
                list(color = color, fontWeight = "bold")
              }
            ),
            beds_left = reactable::colDef(
              header = "Beds Left",
              align = "right"
            )
          )
        )
      })

      # Download Handler
      output$download <- shiny::downloadHandler(
        filename = function() {
          paste0(Sys.Date(), "Pre-Lease-Summary-Table.xlsx")
        },
        content = function(file) {

          df <- pre_lease_summary_data() |>
            dplyr::collect()

          writexl::write_xlsx(
            x = df,
            path = file
          )
        }
      )

      return(
        list(
          pre_lease_summary_data = shiny::reactive({ dplyr::collect(pre_lease_summary_data()) })
        )
      )

    }
  )
}


# demo --------------------------------------------------------------------

#' @rdname mod_pre_lease
#' @export
#' @importFrom pkgload load_all
#' @importFrom bslib page_fluid bs_theme
#' @importFrom shiny shinyApp
mod_pre_lease_demo <- function() {

  pkgload::load_all()

  ui <- bslib::page_navbar(
    title = "Demo",
    theme = bslib::bs_theme(version = 5),
    lang = "en",
    window_title = "Demo",
    bslib::nav_panel(
      title = "Pre-Lease",
      value = "pre_lease",
      icon = bsicons::bs_icon("file-earmark-bar-graph"),
      mod_pre_lease_ui("demo")
    )
  )

  server <- function(input, output, session) {

    pool <- db_connect()

    selected_properties <- shiny::reactive({
      mem_get_entrata_property_ids()
    })

    mod_pre_lease_server("demo", pool, selected_properties)
  }

  shiny::shinyApp(ui, server)
}

db_get_pre_lease_summary_data <- function(pool, property_ids) {

  check_db_conn(pool)

  dplyr::tbl(conn, I("entrata.pre_lease_summary_tbl_view")) |>
    dplyr::filter(.data$entrata_property_id %in% .env$property_ids)

}
