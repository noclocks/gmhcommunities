
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

    # header ------------------------------------------------------------------
    htmltools::tags$h1(
      bsicons::bs_icon("building"),
      "Pre-Lease Summary",
      class = "text-center mb-4"
    ),

    # value boxes -------------------------------------------------------------

    bslib::layout_columns(
      col_widths = c(3, 3, 3, 3),
      bslib::value_box(
        title = "Average Occupancy %",
        value = shiny::textOutput(ns("value_avg_occupancy_pct"), inline = TRUE),
        showcase = bsicons::bs_icon("percent"),
        theme = "primary"
      ),
      bslib::value_box(
        title = "New Leases",
        value = shiny::textOutput(ns("value_new_leases"), inline = TRUE),
        showcase = bsicons::bs_icon("file-earmark-plus"),
        theme = "success"
      ),
      bslib::value_box(
        title = "Renewals",
        value = shiny::textOutput(ns("value_renewals"), inline = TRUE),
        showcase = bsicons::bs_icon("arrow-repeat"),
        theme = "info"
      ),
      bslib::value_box(
        title = "Year-Over-Year Change (%)",
        value = shiny::textOutput(ns("value_yoy_pct_change"), inline = TRUE),
        showcase = bsicons::bs_icon("graph-up-arrow")
      )
    ),

    # about -------------------------------------------------------------------

    bslib::card(
      bslib::card_header(
        htmltools::tags$span(
          bsicons::bs_icon("info-circle"),
          "About"
        ),
        class = "bg-dark text-white"
      ),
      bslib::card_body(
        # htmltools::includeMarkdown("www/content/pre_lease/about.md"),
        htmltools::tags$p(
          bsicons::bs_icon("clipboard-data"),
          "This pre-lease summary dashboard provides a comprehensive overview of property leasing performance across multiple locations.
      The data includes detailed metrics on current occupancy, lease types, and velocity targets.",
          class = "mb-2"
        ),
        htmltools::tags$ul(
          htmltools::tags$li(bsicons::bs_icon("house-check"), "Current Occupancy: Shows the current percentage of occupied beds"),
          htmltools::tags$li(bsicons::bs_icon("file-earmark-text"), "Lease Types: Breakdown between new leases and renewals"),
          htmltools::tags$li(bsicons::bs_icon("graph-up"), "YOY Performance: Year-over-year comparison of leasing metrics"),
          htmltools::tags$li(bsicons::bs_icon("speedometer"), "Velocity Targets: Progress towards 90%, 95%, and 100% occupancy goals")
        ),
        class = "mb-4"
      )
    ),

    # charts ------------------------------------------------------------------

    bslib::layout_columns(
      col_widths = c(6, 6),
      min_height = "400px",
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          htmltools::tags$span(
            bsicons::bs_icon("bar-chart"),
            "Occupancy vs. Target"
          ),
          class = "bg-dark"
        ),
        bslib::card_body(
          plotly::plotlyOutput(ns("occupancy_chart"), height = "500px") |>
            with_loader()
        )
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          htmltools::tags$span(
            bsicons::bs_icon("pie-chart"),
            "New vs. Renewal Distribution"
          ),
          class = "bg-dark"
        ),
        bslib::card_body(
          plotly::plotlyOutput(ns("lease_type_chart"), height = "500px") |>
            with_loader()
        )
      )
    ),

    # summary table -----------------------------------------------------------

    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        htmltools::tags$span(
          bsicons::bs_icon("table"),
          "Pre-Lease Summary"
        ),
        shiny::downloadButton(ns("download"), "Export to Excel", class = "success float-end"),
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

      output$value_new_leases <- shiny::renderText({
        shiny::req(pre_lease_summary_data())

        pre_lease_summary_data() |>
          dplyr::pull(current_total_new) |>
          sum(na.rm = TRUE) |>
          scales::comma()

      })

      output$value_renewals <- shiny::renderText({
        shiny::req(pre_lease_summary_data())

        pre_lease_summary_data() |>
          dplyr::pull(current_total_renewals) |>
          sum(na.rm = TRUE) |>
          scales::comma()

      })

      output$value_yoy_pct_change <- shiny::renderText({
        shiny::req(pre_lease_summary_data())

        pre_lease_summary_data() |>
          dplyr::pull(yoy_variance_percent) |>
          mean(na.rm = TRUE) |>
          scales::percent(accuracy = 0.1)

      })

      # occupancy chart ---------------------------------------------------------

      output$occupancy_chart <- plotly::renderPlotly({

        shiny::req(pre_lease_summary_data())

        df <- pre_lease_summary_data() |>
          dplyr::collect()

        plotly::plot_ly(
          data = df
        ) |>
          plotly::add_trace(
            x = ~property_name,
            y = ~current_occupancy,
            type = "bar",
            name = "Current Occupancy %",
            marker = list(color = "#2C3E50")
          ) |>
          plotly::add_trace(
            x = ~property_name,
            y = ~rep(0.90, nrow(df)),
            type = "scatter",
            mode = "lines",
            name = "90% Target",
            line = list(color = "#E74C3C", dash = "dash")
          ) |>
          plotly::layout(
            xaxis = list(tickangle = 45),
            yaxis = list(tickformat = ".0%"),
            showlegend = TRUE,
            margin = list(b = 100),
            barmode = "group"
          )

      })

      # lease type chart --------------------------------------------------------

      output$lease_type_chart <- plotly::renderPlotly({

        shiny::req(pre_lease_summary_data())

        df <- pre_lease_summary_data() |>
          dplyr::collect()

        plotly::plot_ly(
          data = df
        ) |>
          plotly::add_trace(
            x = ~property_name,
            y = ~current_total_new,
            type = "bar",
            name = "New Leases",
            marker = list(color = "#2C3E50")
          ) |>
          plotly::add_trace(
            x = ~property_name,
            y = ~current_total_renewals,
            type = "bar",
            name = "Renewals",
            marker = list(color = "#18BC9C")
          ) |>
          plotly::layout(
            xaxis = list(tickangle = 45),
            showlegend = TRUE,
            margin = list(b = 100),
            barmode = "stack"
          )

      })

      # pre-lease comparison chart ----------------------------------------------

      # output$pre_lease_comparison_chart <- plotly::renderPlotly({
      #
      #   shiny::req(pre_lease_summary_data())
      #
      #   df <- pre_lease_summary_data() |>
      #     dplyr::collect()
      #
      #   plotly::plot_ly(
      #     data = df
      #   ) |>
      #     plotly::add_trace(
      #       x = ~property_name,
      #       y = ~current_preleased_percent,
      #       type = "scatter",
      #       mode = "lines+markers",
      #       name = "Current Pre-Lease %",
      #       line = list(color = "#18BC9C")
      #     ) |>
      #     plotly::add_trace(
      #       x = ~property_name,
      #       y = ~prior_preleased_percent,
      #       type = "scatter",
      #       mode = "lines+markers",
      #       name = "Prior Pre-Lease %",
      #       line = list(color = "#2C3E50")
      #     ) |>
      #     plotly::layout(
      #       xaxis = list(tickangle = 45),
      #       yaxis = list(tickformat = ".0%"),
      #       showlegend = TRUE,
      #       margin = list(b = 100)
      #     )
      # })

      # summary table -----------------------------------------------------------

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
            investment_partner,
            model_beds,
            total_beds,
            current_occupied,
            current_occupancy,
            current_total_new,
            current_total_renewals,
            current_total_leases,
            current_preleased_percent,
            prior_total_new,
            prior_total_renewals,
            prior_total_leases,
            prior_preleased_percent,
            yoy_variance_count,
            yoy_variance_percent,
            weekly_new,
            weekly_renewal,
            weekly_total,
            weekly_percent_gained,
            beds_left,
            vel_90,
            vel_95,
            vel_100
          )

        reactable::reactable(
          data = df,
          filterable = TRUE,
          searchable = TRUE,
          bordered = TRUE,
          striped = TRUE,
          defaultSorted = list(property_name = "asc"),
          pagination = FALSE,
          columns = list(
            property_id = reactable::colDef(show = FALSE),
            property_name = reactable::colDef(
              header = "Property",
              sortable = TRUE,
              searchable = TRUE,
              minWidth = 150
            ),
            investment_partner = reactable::colDef(
              header = "Investment Partner",
              sortable = TRUE,
              searchable = TRUE,
              minWidth = 150
            ),
            model_beds = reactable::colDef(
              header = "Model Beds",
              align = "right"
            ),
            total_beds = reactable::colDef(
              header = "Total Beds",
              align = "right"
            ),
            current_occupied = reactable::colDef(
              header = "Current Occupied",
              align = "right"
            ),
            current_occupancy = reactable::colDef(
              header = "Current Occupancy %",
              format = reactable::colFormat(percent = TRUE, digits = 1),
              style = function(value) {
                if (value >= 0.95) {
                  color <- "#18BC9C"
                } else if (value >= 0.90) {
                  color <- "#F39C12"
                } else {
                  color <- "#E74C3C"
                }
                list(color = color, fontWeight = "bold")
              }
            ),
            current_total_new = reactable::colDef(name = "New Leases", align = "right"),
            current_total_renewals = reactable::colDef(name = "Renewals", align = "right"),
            current_total_leases = reactable::colDef(name = "Total Leases", align = "right"),
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
            prior_total_new = reactable::colDef(name = "Prior New Leases", align = "right"),
            prior_total_renewals = reactable::colDef(name = "Prior Renewals", align = "right"),
            prior_total_leases = reactable::colDef(name = "Prior Total Leases", align = "right"),
            prior_preleased_percent = reactable::colDef(
              header = "Prior Pre-Lease %",
              format = reactable::colFormat(percent = TRUE, digits = 1)
            ),
            yoy_variance_count = reactable::colDef(name = "YOY Variance (Count)", align = "right"),
            yoy_variance_percent = reactable::colDef(
              name = "YOY Variance (%)",
              format = reactable::colFormat(percent = TRUE, digits = 1),
              style = function(value) {
                if (value > 0) {
                  list(color = "#18BC9C")
                } else if (value < 0) {
                  list(color = "#E74C3C")
                }
              }
            ),
            weekly_new = reactable::colDef(name = "Weekly New", align = "right"),
            weekly_renewal = reactable::colDef(name = "Weekly Renewals", align = "right"),
            weekly_total = reactable::colDef(name = "Weekly Total", align = "right"),
            weekly_percent_gained = reactable::colDef(
              name = "Weekly % Gained",
              format = reactable::colFormat(percent = TRUE, digits = 1),
              style = function(value) {
                if (value > 0) {
                  list(color = "#18BC9C")
                } else if (value < 0) {
                  list(color = "#E74C3C")
                }
              }
            ),
            beds_left = reactable::colDef(
              header = "Beds Left",
              align = "right"
            ),
            vel_90 = reactable::colDef(name = "90% Velocity", format = colFormat(digits = 1)),
            vel_95 = reactable::colDef(name = "95% Velocity", format = colFormat(digits = 1)),
            vel_100 = reactable::colDef(name = "100% Velocity", format = colFormat(digits = 1))
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

  shiny::addResourcePath("www", pkg_sys("www"))

  ui <- bslib::page_navbar(
    title = "Demo",
    theme = bslib::bs_theme(version = 5),
    lang = "en",
    window_title = "Demo",
    bslib::nav_panel(
      title = "Pre-Lease",
      value = "pre_lease",
      icon = bsicons::bs_icon("file-earmark-bar-graph"),
      add_external_resources(),
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

  dplyr::tbl(conn, I("entrata.pre_lease_summary_tbl")) |>
    dplyr::filter(.data$entrata_property_id %in% .env$property_ids)

}
