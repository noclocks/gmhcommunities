
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

  htmltools::tagList(
    bslib::layout_columns(
      col_widths = c(4, 4, 4),
      fill = FALSE,
      bslib::value_box(
        title = "Occupancy Rate",
        value = shiny::textOutput("occupancy", inline = TRUE),
        showcase = bsicons::bs_icon("house-check-fill"),
        theme = "primary",
        full_screen = TRUE,
        htmltools::tags$p("2% increase from last month", class = "small text-muted")
      ),
      bslib::value_box(
        title = "New Leases",
        value = shiny::textOutput("leases", inline = TRUE),
        showcase = bsicons::bs_icon("file-earmark-text-fill"),
        theme_color = "success",
        htmltools::tags$p("5 more than last month", class = "small text-muted"),
        full_screen = TRUE
      ),
      bslib::value_box(
        title = "Revenue",
        value = shiny::textOutput("revenue", inline = TRUE),
        showcase = bsicons::bs_icon("currency-dollar"),
        theme_color = "info",
        htmltools::tags$p("15% increase YoY", class = "small text-muted"),
        full_screen = TRUE
      )
    ),
    bslib::card(
      bslib::card_header(bsicons::bs_icon("graph-up"), "Key Metrics"),
      bslib::card_body(
        "This is where you would display additional metrics or charts.",
        bslib::layout_column_wrap(
          width = 1/2,
          bslib::card(
            shiny::plotOutput(ns("plot_occupancy"))
          ),
          bslib::card(
            shiny::plotOutput(ns("plot_revenue"))
          )
        )
      )
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header(bs_icon("clock-history"), "Recent Activity"),
        bslib::card_body("This section could show recent updates or activities.")
      ),
      bslib::card(
        bslib::card_header(bs_icon("chat-dots"), "Messages"),
        bslib::card_body(
          "This section could show recent messages or notifications."
        )
      )
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
mod_dashboard_server <- function(id) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      cli::cat_rule("mod_dashboard_server")

      pre_lease_summary_data <- qs::qread(pkg_sys("extdata/data/pre_lease_summary_data.qs"))

      output$occupancy <- shiny::renderText("95%")
      output$leases <- shiny::renderText("23")
      output$revenue <- shiny::renderText("$127,500")

      summary_data <- shiny::reactive({
        pre_lease_summary_data
      })

      summary_data_prepped <- shiny::reactive({
        shiny::req(summary_data())
        summary_data() |>
          dplyr::mutate(
            `Investment Partner` = dplyr::case_when(
              `Investment Partner` == "GMH" ~ "GMH Capital Partners",
              `Investment Partner` == "GMH Capital Partners" ~ "GMH Capital Partners",
              TRUE ~ `Investment Partner`
            )
          )
      })

      output$plot_occupancy <- shiny::renderPlot({
        ggplot2::ggplot(summary_data_prepped(), ggplot2::aes(x = `Investment Partner`, y = `Occupancy Rate`)) +
          ggplot2::geom_col(fill = "#007bff") +
          ggplot2::labs(
            title = "Occupancy Rate by Investment Partner",
            x = "Investment Partner",
            y = "Occupancy Rate"
          ) +
          ggplot2::theme_minimal()
      })

      output$plot_revenue <- shiny::renderPlot({
        ggplot2::ggplot(summary_data_prepped(), ggplot2::aes(x = `Investment Partner`, y = `Revenue`)) +
          ggplot2::geom_col(fill = "#28a745") +
          ggplot2::labs(
            title = "Revenue by Investment Partner",
            x = "Investment Partner",
            y = "Revenue"
          ) +
          ggplot2::theme_minimal()
      })

      return(
        list(

        )
      )

    }
  )
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
    theme = bslib::bs_theme(version = 5),
    lang = "en",
    mod_dashboard_ui("demo")
  )

  server <- function(input, output, session) {
    mod_dashboard_server("demo")
  }

  shiny::shinyApp(ui, server)
}
