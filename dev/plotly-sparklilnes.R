library(plotly)

set.seed(123)

generate_random_walk <- function(num_steps = 90) {
  start_date <- as.POSIXct(as.integer(Sys.time()) * runif(1), origin = "1970-01-01")

  increments <- rnorm(num_steps)
  cumulative_sum <- cumsum(increments)
  time_series <- c(0, cumulative_sum) + rnorm(1, 0, 50) + 25

  dates <- seq(start_date, length.out = num_steps + 1, by = "day")

  data.frame(date = dates, value = time_series)
}

as_plotly_sparkline <- function(plot, color = "white") {
  plot |>
    layout(
      xaxis = list(visible = FALSE, showgrid = FALSE, title = ""),
      yaxis = list(visible = FALSE, showgrid = FALSE, title = ""),
      hovermode = "x",
      margin = list(t = 0, r = 0, l = 0, b = 0),
      font = list(color = color),
      paper_bgcolor = "transparent",
      plot_bgcolor = "transparent"
    ) |>
    config(displayModeBar = FALSE) |>
    htmlwidgets::onRender(
      "function(el) {
        var ro = new ResizeObserver(function() {
          var visible = el.offsetHeight > 200;
          Plotly.relayout(el, {'xaxis.visible': visible});
        });
        ro.observe(el);
      }"
    )
}


random_plotly_plot <- function(type = NULL, color = "white") {
  if (is.null(type)) {
    type <-  sample(c("bar", "box", "line"), 1)
  }

  plot <- switch(
    type,
    bar = random_plotly_bar(color, 50),
    box = random_plotly_box(color, 50),
    line = random_plotly_line(color, 50),
    stop("Not a valid random plot type: ", type)
  )

  as_plotly_sparkline(plot, color)
}

random_plotly_bar <- function(color, n = 50) {
  plot_ly(
    x = ~ runif(n),
    type = "histogram",
    histnorm = "probability",
    nbinsx = 10,
    color = I(color),
    stroke = I(color),
    alpha_stroke = 1,
    alpha = 0.6
  )
}

random_plotly_box <- function(color, n = 50) {
  plot_ly(x = ~rnorm(n), type = "box", color = I(color))
}

random_plotly_line <- function(color, n) {
  add_lines(
    plot_ly(generate_random_walk(n)),
    x = ~ date,
    y = ~ value,
    color = I(color),
    fill = "tozeroy",
    span = I(1),
    alpha = 0.2
  )
}

# bslib::layout_column_wrap(
#   width = 1/3,
#   equal = "row",
#   max_height = "200px",
#   bslib::value_box(
#     title = "Total Managed Properties",
#     value = shiny::textOutput(ns("total_properties"), inline = TRUE),
#     # theme = "bg-gradient-blue-indigo",
#     showcase = bsicons::bs_icon("buildings"),
#     showcase_layout = "left center",
#     full_screen = TRUE,
#     fill = TRUE
#   ),
#   bslib::value_box(
#     title = "Occupancy (%)",
#     value = shiny::uiOutput(ns("occupancy")),
#     # theme = "bg-gradient-blue-indigo",
#     showcase = sparkline_plot(),
#     showcase_layout = "bottom",
#     full_screen = TRUE,
#     fill = TRUE,
#     shiny::textOutput(ns("total_properties_info"))
#   ),
#   bslib::value_box(
#     title = "New Leases",
#     value = shiny::textOutput(ns("new_leases"), inline = TRUE),
#     # theme = "bg-gradient-blue-indigo",
#     showcase = bsicons::bs_icon("file-earmark-plus"),
#     showcase_layout = "left center",
#     full_screen = TRUE,
#     fill = TRUE
#   )
# ),

mod_home_ui <- function(id) {

  ns <- shiny::NS(id)

  htmltools::tagList(
    htmltools::tags$head(
      fontawesome::fa_html_dependency()
    ),
    # value boxes -------------------------------------------------------------
    bslib::layout_column_wrap(
      width = 1/3,
      fill = FALSE,
      bslib::value_box(
        title = "Total Managed Properties",
        value = shiny::textOutput(ns("total_properties"), inline = TRUE),
        theme = "dark",
        showcase = random_plotly_plot(type = "line", color = "white"),
        showcase_layout = "bottom",
        full_screen = TRUE
      ),
      bslib::value_box(
        title = "Total Occupancy (%)",
        value = shiny::uiOutput(ns("occupancy")),
        theme = "dark",
        showcase = random_plotly_plot(type = "line", color = "white"),
        showcase_layout = "bottom",
        full_screen = TRUE
      ),
      bslib::value_box(
        title = "Average Scheduled Rent ($)",
        value = shiny::textOutput(ns("avg_scheduled_rent"), inline = TRUE),
        theme = "dark",
        showcase = random_plotly_plot(type = "line", color = "white"),
        showcase_layout = "bottom",
        full_screen = TRUE,
        fill = TRUE
      )
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header(
          class = "bg-dark text-white",
          bsicons::bs_icon("graph-up"),
          "Data Visualization"
        ),
        bslib::card_body(
          # TODO: Add Charts
          "Coming soon..."
        )
      ),
      bslib::card(
        bslib::card_header(
          class = "bg-dark text-white",
          bsicons::bs_icon("clock-history"),
          "Recent Activity"
        ),
        bslib::card_body(
          # TODO: Add Recent Activity
          "This section could show recent updates or activities."
        )
      )
    )
  )
}

mod_home_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    pre_lease_summary_data <- qs::qread(pkg_sys("extdata/data/pre_lease_summary_data.qs"))

    output$total_properties <- shiny::renderText({
      num_props <- nrow(pre_lease_summary_data) |>
        format_integer()
      paste0(num_props, " Properties")
    })

    output$occupancy <- shiny::renderUI({
      val <- (sum(pre_lease_summary_data$occupied_count) / sum(pre_lease_summary_data$units))
      format_percent_incr_decr(val) |>
        HTML()
    })

    output$avg_scheduled_rent <- shiny::renderText({
      val <- mean(pre_lease_summary_data$avg_scheduled_rent)
      format_currency(val)
    })

  })
}


# demo --------------------------------------------------------------------

#' @rdname mod_home
#' @export
#' @importFrom pkgload load_all
#' @importFrom bslib page_fluid bs_theme
#' @importFrom shiny shinyApp
mod_home_demo <- function() {

  pkgload::load_all()

  ui <- bslib::page_fluid(
    title = "Demo",
    theme = bslib::bs_theme(version = 5),
    lang = "en",
    mod_home_ui("demo")
  )

  server <- function(input, output, session) {
    mod_home_server("demo")
  }

  shiny::shinyApp(ui, server)
}

mod_home_demo()
