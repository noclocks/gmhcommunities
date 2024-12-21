
#  ------------------------------------------------------------------------
#
# Title : Dashboard Shiny Module
#    By : Jimmy Briggs
#  Date : 2024-11-27
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' Dashboard Shiny Module
#'
#' @name mod_dashboard
#'
#' @description
#' A Shiny module that displays a dashboard with key metrics and visualizations.
#'
#' - `mod_dashboard_ui()`: User interface
#' - `mod_dashboard_server()`: Server logic
#'
#' @param id Module's namespace ID.
#'
#' @return
#' - `mod_dashboard_ui()`: UI HTML Output.
#' - `mod_dashboard_server()`: Reactive values returned from server logic.
#'
#' @examples
#' if (interactive()) {
#'   mod_dashboard_demo()
#' }
NULL

# UI ----------------------------------------------------------------------

#' @rdname mod_dashboard
#' @export
#' @importFrom bsicons bs_icon
#' @importFrom bslib layout_columns value_box card card_header card_body layout_column_wrap
#' @importFrom htmltools tagList tags
#' @importFrom shiny NS textOutput plotOutput
mod_dashboard_ui <- function(id) {

  ns <- shiny::NS(id)

  bslib::navset_card_underline(
    id = ns("dashboard"),
    bslib::nav_panel(
      title = icon_text("dashboard", "Overview"),
      mod_dashboard_overview_ui(ns("overview"))
    ),
    bslib::nav_panel(
      title = icon_text("info-circle", "Details"),
      mod_dashboard_details_ui(ns("details"))
    )
  )

}


# server ------------------------------------------------------------------

#' @rdname mod_dashboard
#' @export
#' @importFrom cli cat_rule
#' @importFrom dplyr mutate case_when
#' @importFrom ggplot2 ggplot aes geom_col labs theme_minimal
#' @importFrom qs qread
#' @importFrom shiny moduleServer renderText reactive req renderPlot
mod_dashboard_server <- function(
    id,
    pool = NULL,
    selected_properties = NULL,
    report_date = NULL
) {

  # database connection
  if (is.null(pool)) { pool <- shiny::getDefaultReactiveDomain()$userData$pool }
  if (is.null(pool)) { pool <- db_connect() }
  check_db_conn(pool)

  # validate reactives
  if (is.null(selected_properties)) selected_properties <- shiny::reactive({ mem_get_entrata_property_ids() })
  if (is.null(report_date)) report_date <- shiny::reactive({ Sys.Date() })
  stopifnot(shiny::is.reactive(selected_properties), shiny::is.reactive(report_date))

  # module
  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns
    cli::cli_rule("[Module]: Dashboard")

    # get database data
    summary_data <- shiny::reactive({
      shiny::req(selected_properties())
      props <- selected_properties()

      db_get_latest_pre_lease_summary(pool) |>
        dplyr::filter(
          property_id %in% props
        ) |>
        dplyr::collect()
    })

    mod_dashboard_overview_server("overview", summary_data)
    mod_dashboard_details_server("details", summary_data)

    return(list(summary_data = summary_data))

  })
}

# demo --------------------------------------------------------------------

#' @rdname mod_dashboard
#' @export
#' @importFrom pkgload load_all
#' @importFrom bslib page_fluid bs_theme
#' @importFrom shiny shinyApp
mod_dashboard_demo <- function() {

  pkgload::load_all()

  ui <- bslib::page_fluid(
    title = "Demo",
    theme = app_theme(),
    lang = "en",
    mod_dashboard_ui("demo")
  )

  server <- function(input, output, session) {

    pool <- db_connect()

    report_date <- shiny::reactive({ Sys.Date() })
    selected_properties <- shiny::reactive({ mem_get_entrata_property_ids() })

    dashboard_data <- mod_dashboard_server("demo", pool, selected_properties, report_date)

    shiny::observe({
      print(
        list(
          "summary_data" = dashboard_data$summary_data()
        )
      )
    })
  }

  shiny::shinyApp(ui, server)
}


# overview ----------------------------------------------------------------


mod_dashboard_overview_ui <- function(id) {

  ns <- shiny::NS(id)

  htmltools::tagList(
    bslib::layout_columns(
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          icon_text("buildings", "Property Summary Metrics", .function = bsicons::bs_icon),
          class = "bg-primary text-white"
        ),
        reactable::reactableOutput(ns("property_summary_table")) |>
          with_loader()
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          icon_text("house-check-fill", "Occupancy by Property", .function = bsicons::bs_icon),
          class = "bg-primary text-white"
        ),
        plotly::plotlyOutput(ns("occupancy_plot")) |>
          with_loader()
      )
    ),
    bslib::layout_columns(
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          # icon for velocity
          icon_text("speedometer", "Leasing Velocity"),
          class = "bg-primary text-white"
        ),
        plotly::plotlyOutput(ns("velocity_plot")) |>
          with_loader()
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          # weekly performance
          icon_text("calendar-week", "Weekly Performance"),
          class = "bg-primary text-white"
        ),
        plotly::plotlyOutput(ns("weekly_plot")) |>
          with_loader()
      )
    )
  )

}

mod_dashboard_overview_server <- function(id, summary_data) {

  # validate reactives
  stopifnot(shiny::is.reactive(summary_data))

  # module
  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns
    cli::cli_rule("[Module]: Dashboard - Overview")

    # property summary table
    output$property_summary_table <- reactable::renderReactable({
      shiny::req(summary_data())

      data <- summary_data() |>
        dplyr::select(
          "property_name",
          "total_beds",
          "current_occupied",
          "current_preleased_percent",
          "weekly_total",
          "beds_left"
        ) |>
        dplyr::arrange(dplyr::desc(.data$total_beds))

      reactable::reactable(
        data,
        compact = TRUE,
        searchable = TRUE,
        striped = TRUE,
        defaultPageSize = 5,
        columns = list(
          property_name = reactable::colDef(name = "Property Name"),
          total_beds = reactable::colDef(name = "Total Beds"),
          current_occupied = reactable::colDef(name = "Occupied Beds"),
          current_preleased_percent = reactable::colDef(
            name = "Pre-leased %",
            format = reactable::colFormat(percent = TRUE, digits = 1)
          ),
          weekly_total = reactable::colDef(name = "Weekly Leases"),
          beds_left = reactable::colDef(name = "Beds Available")
        )
      )
    })

    # occupancy plot
    output$occupancy_plot <- plotly::renderPlotly({
      shiny::req(summary_data())
      data <- summary_data()

      plotly::plot_ly(
        data,
        x = ~property_name,
        y = ~current_occupancy,
        type = "bar",
        name = "Current Occupancy (%)"
      ) |>
        plotly::layout(
          title = "Occupancy Rate by Property",
          xaxis = list(title = "", tickangle = 45),
          yaxis = list(title = "Occupancy Rate (%)", tickformat = ",.1%"),
          margin = list(b = 120)
        )
    })

    # velocity plot
    output$velocity_plot <- plotly::renderPlotly({
      shiny::req(summary_data())
      data <- summary_data()

      plotly::plot_ly(data) |>
        plotly::add_trace(
          x = ~property_name,
          y = ~vel_90,
          name = "90% Velocity",
          type = "bar"
        ) |>
        plotly::add_trace(
          x = ~property_name,
          y = ~vel_95,
          name = "95% Velocity",
          type = "bar"
        ) |>
        plotly::add_trace(
          x = ~property_name,
          y = ~vel_100,
          name = "100% Velocity",
          type = "bar"
        ) |>
        plotly::layout(
          title = "Velocity by Property",
          xaxis = list(title = "", tickangle = 45),
          yaxis = list(title = "Velocity % (Beds/Week Needed)"),
          margin = list(b = 120)
        )

    })

    # weekly plot
    output$weekly_plot <- plotly::renderPlotly({
      shiny::req(summary_data())
      data <- summary_data()

      plotly::plot_ly(data) |>
        plotly::add_trace(
          x = ~property_name,
          y = ~weekly_new,
          name = "New Leases",
          type = "bar"
        ) |>
        plotly::add_trace(
          x = ~property_name,
          y = ~weekly_renewal,
          name = "Renewals",
          type = "bar"
        ) |>
        plotly::layout(
          barmode = "stack",
          yaxis = list(title = "Weekly Leases"),
          xaxis = list(title = "", tickangle = 45),
          margin = list(b = 120)
        )

    })

  })
}


# details -----------------------------------------------------------------

# Details Module UI
mod_dashboard_details_ui <- function(id) {

  ns <- shiny::NS(id)

  htmltools::tagList(
    bslib::card(
      bslib::card_header("Property Metrics"),
      bslib::layout_columns(
        bslib::value_box(
          title = "Total Beds",
          value = shiny::textOutput(ns("total_beds")),
          showcase = bsicons::bs_icon("building")
        ),
        bslib::value_box(
          title = "Current Occupancy",
          value = shiny::textOutput(ns("occupancy_rate")),
          showcase = bsicons::bs_icon("percent")
        ),
        bslib::value_box(
          title = "Weekly Leases",
          value = shiny::textOutput(ns("weekly_leases")),
          showcase = bsicons::bs_icon("graph-up")
        )
      ),
      bslib::layout_columns(
        bslib::card(
          bslib::card_header("Detailed Metrics"),
          reactable::reactableOutput(ns("details_table"))
        )
      )
    )
  )

}

# Details Module Server
mod_dashboard_details_server <- function(id, property_data) {

  # validate reactives
  stopifnot(shiny::is.reactive(property_data))

  shiny::moduleServer(id, function(input, output, session) {

    output$total_beds <- shiny::renderText({
      shiny::req(property_data())
      sum(property_data()$total_beds) |>
        format_number()
    })

    output$occupancy_rate <- shiny::renderText({
      shiny::req(property_data())
      scales::percent(mean(property_data()$current_occupancy), accuracy = 0.1)
    })

    output$weekly_leases <- shiny::renderText({
      shiny::req(property_data())
      sum(property_data()$weekly_total) |>
        format_currency()
    })

    output$details_table <- reactable::renderReactable({
      shiny::req(property_data())
      data <- property_data() |>
        dplyr::select(
          property_name,
          current_total_new,
          current_total_renewals,
          current_total_leases,
          prior_total_new,
          prior_total_renewals,
          prior_total_leases,
          yoy_variance_count,
          yoy_variance_percent
        )

      reactable::reactable(
        data,
        columns = list(
          property_name = reactable::colDef(name = "Property Name"),
          current_total_new = reactable::colDef(name = "Current New"),
          current_total_renewals = reactable::colDef(name = "Current Renewals"),
          current_total_leases = reactable::colDef(name = "Current Total"),
          prior_total_new = reactable::colDef(name = "Prior New"),
          prior_total_renewals = reactable::colDef(name = "Prior Renewals"),
          prior_total_leases = reactable::colDef(name = "Prior Total"),
          yoy_variance_count = reactable::colDef(name = "YOY Variance Count"),
          yoy_variance_percent = reactable::colDef(
            name = "YOY Variance %",
            format = reactable::colFormat(percent = TRUE, digits = 1)
          )
        ),
        compact = TRUE,
        filterable = TRUE
      )
    })
  })
}


db_get_latest_pre_lease_summary <- function(pool) {

  check_db_conn(pool)

  dplyr::tbl(pool, I("gmh.pre_lease_summary")) |>
    dplyr::filter(
      report_date == max(.data$report_date, na.rm = TRUE)
    )

}


# size vs rent plot
# output$size_rent_plot <- plotly::renderPlotly({
#   shiny::req(summary_data())
#
#   data <- summary_data() |> dplyr::collect()
#
#   plotly::plot_ly(
#     data,
#     x = ~avg_sqft,
#     y = ~avg_scheduled_rent,
#     text = ~property_name,
#     mode = "markers+text",
#     type = "scatter",
#     textposition = "top center"
#   ) |>
#     plotly::layout(
#       title = "Property Size vs. Average Scheduled Rent",
#       xaxis = list(title = "Average Square Footage"),
#       yaxis = list(title = "Average Scheduled Rent ($)")
#     )
# })

# col_widths = c(4, 4, 4),
# fill = FALSE,
# bslib::value_box(
#   title = "Occupancy Rate",
#   value = shiny::uiOutput(ns("occupancy")),
#   showcase = bsicons::bs_icon("house-check-fill"),
#   theme = "primary",
#   full_screen = TRUE,
#
# ),
# bslib::value_box(
#   title = "New Leases",
#   value = shiny::textOutput("leases", inline = TRUE),
#   showcase = bsicons::bs_icon("file-earmark-text-fill"),
#   theme_color = "success",
#   htmltools::tags$p("5 more than last month", class = "small text-muted"),
#   full_screen = TRUE
# ),
# bslib::value_box(
#   title = "Revenue",
#   value = shiny::textOutput("revenue", inline = TRUE),
#   showcase = bsicons::bs_icon("currency-dollar"),
#   theme_color = "info",
#   htmltools::tags$p("15% increase YoY", class = "small text-muted"),
#   full_screen = TRUE
# )
# ),
# bslib::card(
#   bslib::card_header(bsicons::bs_icon("graph-up"), "Key Metrics"),
#   bslib::card_body(
#     "This is where you would display additional metrics or charts.",
#     bslib::layout_column_wrap(
#       width = 1/2,
#       bslib::card(
#         shiny::plotOutput(ns("plot_occupancy"))
#       ),
#       bslib::card(
#         shiny::plotOutput(ns("plot_revenue"))
#       )
#     )
#   )
# ),
# bslib::layout_columns(
#   col_widths = c(6, 6),
#   bslib::card(
#     bslib::card_header(bs_icon("clock-history"), "Recent Activity"),
#     bslib::card_body("This section could show recent updates or activities.")
#   ),
#   bslib::card(
#     bslib::card_header(bs_icon("chat-dots"), "Messages"),
#     bslib::card_body(
#       "This section could show recent messages or notifications."
#     )
#   )
# )
# )




# output$occupancy <- shiny::renderUI({
#   pct <- sum(pre_lease_summary_data$occupied_count) / sum(pre_lease_summary_data$available_count)
#   format_percent_increase_decrease(pct)
# })
#
# output$leases <- shiny::renderText("23")
# output$revenue <- shiny::renderText("$127,500")
#
# summary_data <- shiny::reactive({
#   pre_lease_summary_data
# })
#
# summary_data_prepped <- shiny::reactive({
#   shiny::req(summary_data())
#   summary_data() |>
#     dplyr::mutate(
#       `Investment Partner` = dplyr::case_when(
#         `Investment Partner` == "GMH" ~ "GMH Capital Partners",
#         `Investment Partner` == "GMH Capital Partners" ~ "GMH Capital Partners",
#         TRUE ~ `Investment Partner`
#       )
#     )
# })
#
# output$plot_occupancy <- shiny::renderPlot({
#   ggplot2::ggplot(summary_data_prepped(), ggplot2::aes(x = `Investment Partner`, y = `Occupancy Rate`)) +
#     ggplot2::geom_col(fill = "#007bff") +
#     ggplot2::labs(
#       title = "Occupancy Rate by Investment Partner",
#       x = "Investment Partner",
#       y = "Occupancy Rate"
#     ) +
#     ggplot2::theme_minimal()
# })
#
# output$plot_revenue <- shiny::renderPlot({
#   ggplot2::ggplot(summary_data_prepped(), ggplot2::aes(x = `Investment Partner`, y = `Revenue`)) +
#     ggplot2::geom_col(fill = "#28a745") +
#     ggplot2::labs(
#       title = "Revenue by Investment Partner",
#       x = "Investment Partner",
#       y = "Revenue"
#     ) +
#     ggplot2::theme_minimal()
# })
