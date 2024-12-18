mod_summary_tbl_dataviz_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(

    bslib::layout_columns(
      bslib::value_box(
        title = "Total Properties",
        value = shiny::textOutput(ns("total_properties")),
        showcase = bsicons::bs_icon("building")
      ),
      value_box(
        title = "Total Beds",
        value = shiny::textOutput(ns("total_beds")),
        showcase = bsicons::bs_icon("houses")
      ),
      value_box(
        title = "Average Occupancy",
        value = shiny::textOutput(ns("avg_occupancy")),
        showcase = bsicons::bs_icon("percent")
      )
    ),

    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        shiny::selectInput(
          ns("plot_type"),
          "Select Visualization",
          choices = c(
            "Occupancy Rates" = "occupancy",
            "Current vs Prior Leases" = "leases",
            "New vs Renewal Distribution" = "distribution",
            "YOY Variance" = "variance"
          )
        ),
        shiny::checkboxGroupInput(
          ns("properties"),
          "Filter Properties",
          choices = NULL,
          selected = NULL
        )
      ),

      bslib::card(
        height = "500px",
        bslib::card_header("Visualization"),
        apexcharter::apexchartOutput(ns("main_plot")),
        shiny::textOutput(ns("chart_explanation"))
      ),

      bslib::card(
        full_screen = TRUE,
        card_header("Property Details"),
        reactable::reactableOutput(ns("property_table"))
      )
    )
  )

}

mod_summary_tbl_dataviz_server <- function(id, summary_data = NULL) {

  if (!is.null(summary_data)) {
    stopifnot(shiny::is.reactive(summary_data))
  }

  shiny::moduleServer(id, function(input, output, session) {

    # filter data
    filtered_data <- shiny::reactive({

      shiny::req(summary_data(), input$properties)

      summary_data() |>
        dplyr::filter(
          "property_name" %in% input$properties
        )

    }) |>
      shiny::bindEvent(input$properties, ignoreNULL = TRUE, ignoreInit = TRUE)

    # update property filter
    shiny::observe({
      shiny::updateCheckboxGroupInput(
        session,
        "properties",
        choices = unique(summary_data()$property_name),
        selected = unique(summary_data()$property_name)
      )
    }) |>
      shiny::bindEvent(summary_data())

    # valboxes
    output$total_properties <- shiny::renderText({
      shiny::req(summary_data())
      nrow(summary_data())
    })

    output$total_beds <- shiny::renderText({
      shiny::req(summary_data())
      sum(summary_data()$total_beds)
    })

    output$avg_occupancy <- shiny::renderText({
      shiny::req(summary_data())
      scales::percent(mean(summary_data()$current_occupancy), accuracy = 0.1)
    })

    # leases plot
    output$leases_plot <- apexcharter::renderApexchart({

      df <- filtered_data() |>
        dplyr::select(property_name, total_leases, prior_total_leases) |>
        dplyr::rename("Current" = total_leases,
                      "Prior" = prior_total_leases) |>
        tidyr::pivot_longer(-property_name, names_to = "Period", values_to = "Leases")

      apexcharter::apex(data = df,
                        type = "bar",
                        mapping = apexcharter::aes(x = property_name, y = Leases, fill = Period)) |>
        apexcharter::ax_yaxis(title = list(text = "Number of Leases")) |>
        apexcharter::ax_xaxis(title = list(text = "Property")) |>
        apexcharter::ax_tooltip(shared = TRUE, intersect = TRUE)
    })

    # distribution plot
    output$distribution_plot <- apexcharter::renderApexchart({
      df <- filtered_data() |>
        dplyr::select(property_name, total_new, total_renewals) |>
        dplyr::rename("New" = total_new,
                      "Renewals" = total_renewals) |>
        tidyr::pivot_longer(-property_name, names_to = "Type", values_to = "Count")

      apexcharter::apex(data = df,
                        type = "bar",
                        mapping = apexcharter::aes(x = property_name, y = Count, fill = Type)) |>
        apexcharter::ax_plotOptions(bar = list(stacked = TRUE)) |>
        apexcharter::ax_yaxis(title = list(text = "Number of Leases")) |>
        apexcharter::ax_xaxis(title = list(text = "Property")) |>
        apexcharter::ax_tooltip(shared = TRUE, followCursor = TRUE)
    })

    # variance plot
    output$variance_plot <- apexcharter::renderApexchart({
      df <- filtered_data() |>
        dplyr::arrangearrange(yoy_variance_pct) |>
        dplyr::mutate(variance_pct = round(yoy_variance_pct * 100, 1),
                      color = ifelse(yoy_variance_pct >= 0, "#00E396", "#FF4560"))

      apexcharter::apex(data = df,
                        type = "bar",
                        mapping = apexcharter::aes(x = property_name, y = variance_pct)) |>
        apexcharter::ax_colors(unique(df$color)) |>
        apexcharter::ax_yaxis(title = list(text = "YOY Variance (%)"),
                              labels = list(formatter = apexcharter::JS("function(val) { return val + '%' }"))) |>
        apexcharter::ax_xaxis(title = list(text = "Property")) |>
        apexcharter::ax_tooltip(y = list(formatter = apexcharter::JS("function(val) { return val + '%' }")))
    })

    # chart explanations
    explanations <- list(
      occupancy = paste0(
        "This chart shows the current occupancy rate for each property. Higher ",
        "percentages indicate better utilization of available beds."
      ),
      leases = paste0(
        "Compares the number of current leases with prior period ",
        "leases for each property, helping identify trending patterns in lease volumes."
      ),
      distribution = paste0(
        "Breaks down the composition of leases between new tenants and renewals, ",
        "showing the balance of tenant retention vs. new acquisition."
      ),
      variance = paste0(
        "Displays the year-over-year change in lease percentage. Green bars ",
        "indicate improvement, while red bars show decline from the previous year."
      )
    )

    output$occupancy_explanation <- shiny::renderText(explanations[["occupancy"]])
    output$leases_explanation <- shiny::renderText(explanations[["leases"]])
    output$distribution_explanation <- shiny::renderText(explanations[["distribution"]])
    output$variance_explanation <- shiny::renderText(explanations[["variance"]])

    # table output
    output$property_table <- reactable::renderReactable({
      filtered_data() |>
        dplyr::select(
          "property_name",
          "total_beds",
          "current_occupancy",
          "total_leases",
          "prelease_percent",
          "yoy_variance_pct"
        ) |>
        reactable::reactable(
          columns = list(
            property_name = reactable::colDef(name = "Property"),
            total_beds = reactable::colDef(name = "Total Beds"),
            current_occupancy = reactable::colDef(
              name = "Current Occupancy",
              format = reactable::colFormat(percent = TRUE, digits = 1)
            ),
            total_leases = reactable::colDef(name = "Total Leases"),
            prelease_pct = reactable::colDef(
              name = "Pre-lease %",
              format = reactable::colFormat(percent = TRUE, digits = 1)
            ),
            yoy_variance_pct = reactable::colDef(
              name = "YOY Variance",
              format = reactable::colFormat(percent = TRUE, digits = 1),
              style = function(value) {
                color <- if (value >= 0) "#00E396" else "#FF4560"
                list(color = color, fontWeight = "bold")
              }
            )
          ),
          defaultPageSize = 5,
          filterable = TRUE,
          sortable = TRUE,
          striped = TRUE,
          highlight = TRUE
        )
    })

    return(
      list(
      )
    )

  })
}


mod_summary_tbl_dataviz_demo <- function() {

  pkgload::load_all()

  ui <- bslib::page_fillable(
    title = "Demo",
    theme = bslib::bs_theme(version = 5),
    lang = "en",
    shinyjs::useShinyjs(),
    mod_summary_tbl_dataviz_ui("demo")
  )

  server <- function(input, output, session) {

    # session$userData$db_pool <- db_connect()

    demo_data <- readr::read_csv(
      pkg_sys("extdata/data/pre_lease_data.csv")
    )

    mod_summary_tbl_dataviz_server("demo", summary_data = shiny::reactive({demo_data}))
  }

  shiny::shinyApp(ui, server)

}
