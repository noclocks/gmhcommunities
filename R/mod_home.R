

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
      bslib::value_box(
        title = icon_text("buildings", "Total Managed Properties", .function = bsicons::bs_icon),
        value = shiny::textOutput(ns("total_properties"), inline = TRUE),
        showcase = bsicons::bs_icon("building", size = "3rem"),
        full_screen = TRUE,
        theme = "primary"
      ),
      bslib::value_box(
        title = icon_text("percent", "Total Occupancy (%)", .function = bsicons::bs_icon),
        value = shiny::uiOutput(ns("occupancy")),
        showcase = random_plotly_plot(type = "line"),
        showcase_layout = "bottom",
        full_screen = TRUE,
        theme = "dark"
      ),
      bslib::value_box(
        title = icon_text("currency-dollar", "Average Scheduled Rent ($)", .function = bsicons::bs_icon),
        value = shiny::textOutput(ns("avg_scheduled_rent"), inline = TRUE),
        showcase = random_plotly_plot(type = "line"),
        showcase_layout = "bottom",
        full_screen = TRUE,
        theme = "dark"
      )
    ),
    # recent activity & map ----------------------------------------------
    bslib::layout_column_wrap(
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
mod_home_server <- function(
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
    cli::cli_rule("[Module]: Home")

    # get database metrics for value boxes
    summary_metrics <- shiny::reactive({
      shiny::req(report_date(), selected_properties())
      rept_date <- report_date()
      prop_ids <- selected_properties()
      db_get_home_metrics(pool, report_date = rept_date, properties = prop_ids)
    })

    # occupancy value box
    output$occupancy <- shiny::renderUI({
      shiny::req(summary_metrics())
      metrics <- summary_metrics()
      current <- metrics$occupancy_current |> format_percent()
      prior <- metrics$occupancy_prior |> format_percent()
      pct_change <- metrics$occupancy_pct_change
      icon <- if (pct_change >= 0) bsicons::bs_icon("arrow-up") else bsicons::bs_icon("arrow-down") |>
        as.character()
      color <- if (pct_change >= 0) "lightgreen" else "red"
      pct_change <- pct_change |> format_percent()
      htmltools::HTML(
        glue::glue("<span style='color: {color};'>{icon} {current} ({pct_change})</span>")
      )
    })

    # total properties value box
    output$total_properties <- shiny::renderText({
      shiny::req(summary_metrics())
      metrics <- summary_metrics()
      metrics$total_properties |> paste0(" Properties")
    })

    # average scheduled rent value box
    output$avg_scheduled_rent <- shiny::renderUI({
      shiny::req(summary_metrics())
      metrics <- summary_metrics()
      avg <- metrics$scheduled_rent_avg |> format_currency()
      tot <- metrics$scheduled_rent_total |> format_currency()
    })

    # recent activity data
    logs_data <- shiny::reactive({
      db_get_recent_activity_logs(pool)
    })

    # recent activity last updated at
    output$recent_activity_last_updated <- shiny::renderText({
      shiny::req(logs_data())
      logs_data() |>
        dplyr::pull("created_at") |>
        max(na.rm = TRUE) |>
        format_last_updated_at()
    })

    # recent activity table
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

    # properties map last updated at
    output$properties_map_last_updated <- shiny::renderText({
      "Not Available"
    })

    return(
      list(
        summary_metrics = summary_metrics,
        logs_data = logs_data
      )
    )

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
    report_date <- shiny::reactive({ Sys.Date() })
    selected_properties <- shiny::reactive({ mem_get_entrata_property_ids() })
    home_data <- mod_home_server("demo", pool)
    shiny::observe({
      print(
        list(
          "summary_metrics" = home_data$summary_metrics(),
          "logs_data" = dplyr::glimpse(home_data$logs_data())
        )
      )
    })
  }

  shiny::shinyApp(ui, server)
}

welcome_banner_ui <- function() {
  bslib::layout_columns(
    col_widths = c(12),
    bslib::card(
      class = "my-3",
      bslib::card_header(
        class = "bg-primary",
        htmltools::tags$h2(
          "Welcome to GMH Communities Data Hub",
          class = "mb-0"
        )
      ),
      bslib::card_body(
        htmltools::tags$p(
          class = "lead",
          "Your centralized platform for student housing portfolio analytics and insights"
        )
      )
    )
  )
}

db_get_home_metrics <- function(conn, report_date = NULL, properties = NULL) {

  check_db_conn(conn)

  tbl <- dplyr::tbl(conn, I("entrata.pre_lease_summary"))
  tbl_report_date <- max(dplyr::pull(tbl, "report_date"), na.rm = TRUE)

  if (is.null(report_date)) {
    report_date <- tbl_report_date
  }

  tbl_filt <- dplyr::filter(tbl, .data$report_date == .env$report_date)

  total_properties <- length(dplyr::pull(tbl_filt, "property_id"))

  if (total_properties == 0) {
    cli::cli_alert_warning("No data found for the specified report date: {.field {report_date}}.")
    report_date <- tbl_report_date
    tbl_filt <- dplyr::filter(tbl, .data$report_date == .env$report_date)
    total_properties <- length(dplyr::pull(tbl_filt, "property_id"))
    cli::cli_alert_info("Using the latest report date instead: {.field {report_date}}.")
  }

  last_updated_at <- max(dplyr::pull(tbl_filt, "created_at"), na.rm = TRUE)
  occupancy_current <- mean(
    dplyr::pull(tbl_filt, "occupied_count") / dplyr::pull(tbl_filt, "rentable_unit_count"),
    na.rm = TRUE
  )
  occupancy_prior <- mean(
    dplyr::pull(tbl_filt, "preleased_count_prior") / dplyr::pull(tbl_filt, "rentable_unit_count"),
    na.rm = TRUE
  )
  occupancy_pct_change <- occupancy_current - occupancy_prior
  scheduled_rent_total <- sum(
    dplyr::pull(tbl, "scheduled_rent_total"),
    na.rm = TRUE
  )
  scheduled_rent_avg <- mean(
    dplyr::pull(tbl_filt, "avg_scheduled_rent"),
    na.rm = TRUE
  )
  variance_total <- sum(
    dplyr::pull(tbl_filt, "variance"),
    na.rm = TRUE
  )
  variance_avg <- variance_total / total_properties
  prelease_pct_current <- mean(
    dplyr::pull(tbl_filt, "preleased_percent"),
    na.rm = TRUE
  )
  prelease_pct_prior <- mean(
    dplyr::pull(tbl_filt, "preleased_percent_prior"),
    na.rm = TRUE
  )
  prelease_pct_change <- prelease_pct_current - prelease_pct_prior

  list(
    report_date = report_date,
    last_updated_at = last_updated_at,
    total_properties = total_properties,
    occupancy_current = occupancy_current,
    occupancy_prior = occupancy_prior,
    occupancy_pct_change = occupancy_pct_change,
    scheduled_rent_total = scheduled_rent_total,
    scheduled_rent_avg = scheduled_rent_avg,
    variance_total = variance_total,
    variance_avg = variance_avg,
    prelease_pct_current = prelease_pct_current,
    prelease_pct_prior = prelease_pct_prior,
    prelease_pct_change = prelease_pct_change
  )

}

db_get_recent_activity_logs <- function(pool, ...) {

  check_db_conn(pool)

  dplyr::tbl(pool, I("logs.recent_activity")) |>
    dplyr::arrange(dplyr::desc(created_at))

}
