

#' @importFrom bsicons bs_icon
#' @importFrom bslib layout_column_wrap value_box layout_columns card card_header card_body card_footer
#' @importFrom fontawesome fa_html_dependency
#' @importFrom htmltools tagList tags
#' @importFrom reactable reactableOutput
#' @importFrom shiny NS textOutput uiOutput
mod_home_ui <- function(id) {

  ns <- shiny::NS(id)

  htmltools::tagList(
    htmltools::tags$head(
      fontawesome::fa_html_dependency()
    ),
    # title -------------------------------------------------------------------
    htmltools::tags$h1(
      icon_text(
        icon = "home",
        text = "Home"
      )
    ),
    htmltools::tags$hr(),

    # value boxes -------------------------------------------------------------
    bslib::layout_column_wrap(
      width = 1/3,
      fill = FALSE,
      bslib::value_box(
        title = icon_text(
          "buildings",
          "Total Managed Properties",
          .function = bsicons::bs_icon
        ),
        value = shiny::textOutput(ns("total_properties"), inline = TRUE),
        theme = "dark",
        showcase = random_plotly_plot(type = "line", color = "white"),
        showcase_layout = "bottom",
        full_screen = TRUE
      ),
      bslib::value_box(
        title = icon_text(
          "percent",
          "Total Occupancy (%)",
          .function = bsicons::bs_icon
        ),
        value = shiny::uiOutput(ns("occupancy")),
        theme = "dark",
        showcase = random_plotly_plot(type = "line", color = "white"),
        showcase_layout = "bottom",
        full_screen = TRUE
      ),
      bslib::value_box(
        title = icon_text(
          "currency-dollar",
          "Average Scheduled Rent ($)",
          .function = bsicons::bs_icon
        ),
        value = shiny::textOutput(ns("avg_scheduled_rent"), inline = TRUE),
        theme = "dark",
        showcase = random_plotly_plot(type = "line", color = "white"),
        showcase_layout = "bottom",
        full_screen = TRUE,
        fill = TRUE
      )
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        bslib::card_header(
          class = "bg-dark text-white",
          bsicons::bs_icon("clock-history"),
          "Recent Activity"
        ),
        bslib::card_body(
          "This section shows recent updates or activities.",
          reactable::reactableOutput(ns("recent_activity"))
        ),
        bslib::card_footer(
          htmltools::tags$small(
            "Last updated: ",
            shiny::textOutput(
              ns("recent_activity_last_updated"),
              inline = TRUE
            )
          )
        )
      ),
      bslib::card(
        style = "padding: 0;",
        bslib::card_header(
          class = "bg-dark text-white",
          bsicons::bs_icon("map"),
          "Properties Map"
        ),
        bslib::card_body(
          gmaps_properties_map_embed_iframe()
        ),
        bslib::card_footer(
          htmltools::tags$small(
            "Last updated: ",
            shiny::textOutput(
              ns("properties_map_last_updated"),
              inline = TRUE
            )
          )
        )
      )
    )
  )
}

#' @importFrom bsicons bs_icon
#' @importFrom dplyr arrange tbl desc pull collect
#' @importFrom htmltools HTML tagList tags
#' @importFrom qs qread
#' @importFrom reactable renderReactable reactable colDef
#' @importFrom shiny moduleServer reactive renderText renderUI req
mod_home_server <- function(id, pool) {
  shiny::moduleServer(id, function(input, output, session) {

    pre_lease_summary_data <- qs::qread(pkg_sys("extdata/data/pre_lease_summary_data.qs"))

    occupancy_data <- shiny::reactive({

      current <- mean(pre_lease_summary_data$occupied_count / pre_lease_summary_data$rentable_unit_count, na.rm = TRUE) * 100
      prior <- mean(pre_lease_summary_data$preleased_count_prior / pre_lease_summary_data$rentable_unit_count, na.rm = TRUE) * 100
      pct_change <- (current - prior) / prior * 100

      list(
        current = current,
        pct_change = round(pct_change, 2),
        icon = if (pct_change >= 0) bsicons::bs_icon("arrow-up") else bsicons::bs_icon("arrow-down"),
        color = if (pct_change >= 0) "lightgreen" else "red"
      )


    })

    output$total_properties <- shiny::renderText({
      num_props <- nrow(pre_lease_summary_data) |>
        format_integer()
      paste0(num_props, " Properties")
    })

    output$occupancy <- shiny::renderUI({
      occ <- occupancy_data()
      htmltools::HTML(
        sprintf(
          '<span style="color: %s;">%s %.2f%% (%.2f%%)</span>',
          occ$color,
          as.character(occ$icon),
          occ$current,
          occ$pct_change
        )
      )
    })

    output$avg_scheduled_rent <- shiny::renderText({
      val <- mean(pre_lease_summary_data$avg_scheduled_rent)
      format_currency(val)
    })

    logs_data <- shiny::reactive({
      dplyr::tbl(pool, I("logs.recent_activity")) |>
        dplyr::arrange(dplyr::desc(created_at))
    })

    output$recent_activity_last_updated <- shiny::renderText({
      shiny::req(logs_data())

      logs_data() |>
        dplyr::pull("created_at") |>
        max(na.rm = TRUE) |>
        format_last_updated_at()
    })

    output$properties_map_last_updated <- shiny::renderText({
      "Not Available"
    })

    output$recent_activity <- reactable::renderReactable({

      data <- logs_data() |> dplyr::collect()

      reactable::reactable(
        data,
        columns = list(
          created_at = reactable::colDef(name = "Timestamp"),
          log_type = reactable::colDef(name = "Log Type"),
          context = reactable::colDef(name = "Context"),
          action = reactable::colDef(name = "Action"),
          user_email = reactable::colDef(name = "User Email"),
          details = reactable::colDef(name = "Details", show = FALSE)
        ),
        searchable = TRUE,
        striped = TRUE,
        highlight = TRUE,
        sortable = TRUE,
        resizable = TRUE,
        filterable = TRUE,
        pagination = TRUE,
        outlined = TRUE,
        selection = "multiple",
        bordered = TRUE,
        compact = TRUE,
        wrap = FALSE,
        defaultPageSize = 10,
        showPageSizeOptions = TRUE,
        pageSizeOptions = c(10, 25, 50, 100),
        showPagination = TRUE,
        showPageInfo = TRUE,
        onClick = "expand",
        height = "auto",
        details = function(index) {
          htmltools::tagList(
            htmltools::tags$h4(
              "Activity Details: "
            ),
            htmltools::tags$pre(
              data$details[index]
            )
          )
        }
      )
    })


  })
}


# demo --------------------------------------------------------------------

#' @rdname mod_home
#' @export
#' @importFrom pkgload load_all
#' @importFrom bslib page_fluid bs_theme
#' @importFrom shiny shinyApp
#' @importFrom bslib page_fluid bs_theme
#' @importFrom pkgload load_all
#' @importFrom shiny shinyApp
mod_home_demo <- function() {

  pkgload::load_all()

  pool <- db_connect()

  ui <- bslib::page_fluid(
    title = "Demo",
    theme = bslib::bs_theme(version = 5),
    lang = "en",
    mod_home_ui("demo")
  )

  server <- function(input, output, session) {
    mod_home_server("demo", pool)
  }

  shiny::shinyApp(ui, server)
}

