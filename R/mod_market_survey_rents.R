
#  ------------------------------------------------------------------------
#
# Title : market_survey_rents Shiny Module
#    By : Jimmy Briggs
#  Date : 2024-11-27
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' market_survey_rents Shiny Module
#'
#' @name mod_market_survey_rents
#'
#' @description
#' A Shiny Module for ...
#'
#' - `mod_market_survey_rents_ui()`: User interface
#' - `mod_market_survey_rents_server()`: Server logic
#'
#' @param id Module's namespace ID.
#'
#' @return
#' - `mod_market_survey_rents_ui()`: UI HTML Output.
#' - `mod_market_survey_rents_server()`: Reactive values returned from server logic.
#'
#' @examples
#' if (interactive()) {
#'   mod_market_survey_rents_demo()
#' }
NULL


# UI ----------------------------------------------------------------------

#' @rdname mod_market_survey_rents
#' @export
#' @importFrom apexcharter apexchartOutput
#' @importFrom bsicons bs_icon
#' @importFrom bslib navset_card_pill card_header sidebar nav_panel card card_body
#' @importFrom htmltools tagList tags
#' @importFrom rhandsontable rHandsontableOutput
#' @importFrom shiny NS selectInput radioButtons
mod_market_survey_rents_ui <- function(id) {

  ns <- shiny::NS(id)

  htmltools::tagList(
    htmltools::tags$head(
      # shinyjs::useShinyjs(),
      # waiter::use_waiter(),
      # shinyWidgets::useSweetAlert(),
      # waiter::waiterShowOnLoad(waiter::spin_fading_circles())
    ),
    bslib::navset_card_pill(
      placement = "above",
      id = ns("nav"),
      header = bslib::card_header(
        icon_text("table", "Market Survey Rent Tables & Charts"),
        class = "bg-dark text-white"
      ),
      sidebar = bslib::sidebar(
        shiny::selectInput(
          ns("floorplan_type"),
          "Floorplan Type",
          choices = c("Studio", "1 Bed", "2 Bed", "3 Bed", "4 Bed", "5 Bed", "6 Bed"),
          selected = c("Studio", "1 Bed", "2 Bed", "3 Bed", "4 Bed", "5 Bed", "6 Bed"),
          multiple = TRUE
        ),
        shiny::radioButtons(
          ns("per_bed_sf"),
          "Select Metric:",
          choices = c("Per Bed", "Per SF"),
          selected = "Per Bed"
        )
      ),
      bslib::nav_panel(
        title = icon_text("table", "Tables"),
        bslib::card(
          bslib::card_header(bsicons::bs_icon("info-circle"), "Rents by Floorplan"),
          bslib::card_body(
            # rhandsontable::rHandsontableOutput(ns("rents_by_floorplan_table")) |>
            #   with_loader()
          )
        ),
        bslib::card(
          bslib::card_header(bsicons::bs_icon("info-circle"), "Average Rents by Unit Type"),
          bslib::card_body(
            # rhandsontable::rHandsontableOutput(ns("average_rents_by_unit_type_table")) |>
            #   with_loader()
          )
        )
      ),
      bslib::nav_panel(
        title = icon_text("chart-bar", "Charts"),
        bslib::card(
          bslib::card_header(bsicons::bs_icon("info-circle"), "Rents Scatter"),
          bslib::card_body(
            apexcharter::apexchartOutput(ns("scatter_chart")) |>
              with_loader()
          )
        ),
        bslib::card(
          bslib::card_header(bsicons::bs_icon("bar-chart-line"), "Rent Comparison"),
          bslib::card_body(
            apexcharter::apexchartOutput(ns("rent_comparison_chart")) |>
              with_loader()
          )
        ),
        bslib::card(
          bslib::card_header(bsicons::bs_icon("bar-chart-line"), "Average Market Rent by Unit Type"),
          bslib::card_body(
            apexcharter::apexchartOutput(ns("average_market_rent_by_unit_type_chart")) |>
              with_loader()
          )
        )
      )
    )
  )

}


# server ------------------------------------------------------------------

#' @rdname mod_market_survey_rents
#' @export
#' @importFrom apexcharter renderApexchart ax_yaxis ax_xaxis apex aes ax_legend ax_chart
#' @importFrom cli cat_rule cli_alert_info
#' @importFrom dplyr select mutate filter all_of case_when
#' @importFrom lubridate today days
#' @importFrom rhandsontable renderRHandsontable rhandsontable
#' @importFrom shiny reactive is.reactive moduleServer observe renderText req
#' @importFrom snakecase to_title_case
#' @importFrom tidyr pivot_longer
mod_market_survey_rents_server <- function(
    id,
    pool,
    selected_property = NULL,
    selected_leasing_week = NULL
) {

  # default property (commonwealth)
  if (is.null(selected_property)) {
    selected_property <- shiny::reactive({"739085"})
  }

  # default leasing week (current week)
  if (is.null(selected_leasing_week)) {
    selected_leasing_week <- shiny::reactive({
      get_weekly_period_start_date(as_of_date = lubridate::today() - lubridate::days(6))
    })
  }

  # validation
  stopifnot(shiny::is.reactive(selected_property))
  stopifnot(shiny::is.reactive(selected_leasing_week))
  check_db_conn(pool)

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
      cli::cat_rule("[Module]: mod_market_survey_rents_server()")

      # selected property -------------------------------------------------------
      shiny::observe({
        prop <- selected_property()
        week <- selected_leasing_week()
        cli::cli_alert_info(
          c(
            "Selected Property: {.field {prop}}\n",
            "Selected Leasing Week: {.field {week}}\n"
          )
        )
      })

      output$selected_property_title <- shiny::renderText({
        "Selected Property Title"
      })

      output$last_updated_at <- shiny::renderText({
        "Last Updated At"
      })


      rents_by_floorplan_data <- shiny::reactive({

        db_get_mkt_rents_by_floorplan(
          conn = pool,
          property_id = selected_property(),
          leasing_week = selected_leasing_week()
        )

      })

      avg_rents_by_unit_type_data <- shiny::reactive({

        db_get_avg_rents_by_unit_type(
          conn = pool,
          property_id = selected_property(),
          leasing_week = selected_leasing_week()
        )

      })

      output$rents_by_floorplan_tbl <- rhandsontable::renderRHandsontable({
        shiny::req(rents_by_floorplan_data())

        data <- rents_by_floorplan_data() |>
          dplyr::select(
            property_name,
            leasing_week_start,
            floorplan_type:bundled_rent_per_sf
          )

        rhandsontable::rhandsontable(
          data,
          colHeaders = snakecase::to_title_case(colnames(data)),
          rowHeaders = NULL
        )

      })

      output$average_rents_by_unit_type_tbl <- rhandsontable::renderRHandsontable({
        shiny::req(avg_rents_by_unit_type_data())

        data <- avg_rents_by_unit_type_data() |>
          dplyr::select(
            property_name,
            leasing_week_start,
            floorplan_type:property_floorplan_bundled_rent_per_sf
          )

        rhandsontable::rhandsontable(
          data,
          colHeaders = snakecase::to_title_case(colnames(data)),
          rowHeaders = NULL
        )

      })

      output$scatter_chart <- apexcharter::renderApexchart({
        shiny::req(rents_by_floorplan_data())

        apexcharter::apex(
          data = rents_by_floorplan_data(),
          mapping = apexcharter::aes(
            x = sf_per_bed,
            y = market_rent_per_bed,
            fill = property_name,
            shape = floorplan_type
          ),
          type = "scatter"
        ) |>
          apexcharter::ax_xaxis(
            title = list(text = "Square Feet per Bed"),
            tickAmount = 10
          ) |>
          apexcharter::ax_yaxis(
            title = list(text = "Market Rent per Bed ($)"),
            tickAmount = 10
          )
      })

      output$rent_comparison_chart <- apexcharter::renderApexchart({
        shiny::req(rents_by_floorplan_data())

        if (input$per_bed_sf == "Per Bed") {
          y_vars <- c("market_rent_per_bed", "effective_rent_per_bed", "bundled_rent_per_bed")
          y_label <- "Rent per Bed ($)"
        } else {
          y_vars <- c("market_rent_per_sf", "effective_rent_per_sf", "bundled_rent_per_sf")
          y_label <- "Rent per SF ($)"
        }

        data <- rents_by_floorplan_data() |>
          dplyr::select(
            property = property_name,
            dplyr::all_of(y_vars)
          ) |>
          dplyr::group_by(
            property
          ) |>
          dplyr::summarise(
            market = mean(market_rent_per_bed, na.rm = TRUE),
            effective = mean(effective_rent_per_bed, na.rm = TRUE),
            bundled = mean(bundled_rent_per_bed, na.rm = TRUE)
          ) |>
          dplyr::ungroup() |>
          tidyr::pivot_longer(
            cols = -c("property"),
            names_to = "rent_type",
            values_to = "value"
          ) |>
          dplyr::mutate(
            rent_type = dplyr::case_when(
              grepl("market", rent_type) ~ "Market Rent",
              grepl("effective", rent_type) ~ "Effective Rent",
              grepl("bundled", rent_type) ~ "Bundled Rent"
            )
          )

        apexcharter::apex(
          data = data,
          type = "bar",
          mapping = apexcharter::aes(
            x = property,
            y = value,
            fill = rent_type
          )
        ) |>
          apexcharter::ax_chart(stacked = FALSE) |>
          apexcharter::ax_yaxis(title = list(text = y_label)) |>
          apexcharter::ax_xaxis(
            title = list(text = "Property"),
            labels = list(rotate = -45)
          ) |>
          apexcharter::ax_legend(position = "top", horizontalAlign = "center") |>
          apexcharter::ax_title(text = "Rent Comparison by Property")

      })

      output$average_market_rent_by_unit_type_chart <- apexcharter::renderApexchart({
        shiny::req(avg_rents_by_unit_type_data())

        if (input$per_bed_sf == "Per Bed") {
          y_label <- "Average Market Rent per Bed ($)"
        } else {
          y_label <- "Average Market Rent per Square Foot ($)"
        }

        data <- avg_rents_by_unit_type_data() |>
          dplyr::select(
            property_name,
            unit_type = floorplan_type,
            per_bed = property_floorplan_market_rent_per_bed,
            per_sf = property_floorplan_market_rent_per_sf
          ) |>
          dplyr::group_by(
            property_name,
            unit_type
          ) |>
          dplyr::summarise(
            per_bed = mean(per_bed, na.rm = TRUE),
            per_sf = mean(per_sf, na.rm = TRUE)
          ) |>
          dplyr::ungroup() |>
          dplyr::filter(
            per_bed > 0,
            per_sf > 0
          ) |>
          tidyr::complete(
            property_name,
            unit_type
          )

        apexcharter::apex(
          data = data,
          type = "bar",
          mapping = apexcharter::aes(
            x = unit_type,
            y = if (input$per_bed_sf == "Per Bed") per_bed else per_sf,
            fill = property_name
          )
        ) |>
          apexcharter::ax_title(text = "Average Market Rent by Unit Type by Property") |>
          apexcharter::ax_yaxis(title = list(text = "Unit Type")) |>
          apexcharter::ax_xaxis(title = list(text = y_label)) |>
          apexcharter::ax_legend(position = "top", horizontalAlign = "center")

      })

      # shiny::observeEvent(input$rents_by_floorplan_tbl, {
      #   dat <- rhandsontable::hot_to_r(input$rents_by_floorplan_tbl)
      #   str(dat)
      #   cli::cli_alert_info("Rents by Floorplan Table Updated")
      # })
      #
      # shiny::observeEvent(input$average_rents_by_unit_type_tbl, {
      #   dat <- rhandsontable::hot_to_r(input$average_rents_by_unit_type_tbl)
      #   str(dat)
      #   cli::cli_alert_info("Average Rents by Unit Type Table Updated")
      # })

      return(
        list(
          rents_by_floorplan = shiny::reactive({ rents_by_floorplan_data() }),
          avg_rents_by_unit_type = shiny::reactive({ avg_rents_by_unit_type_data() })
        )
      )

    }
  )
}



# demo --------------------------------------------------------------------

#' @rdname mod_market_survey_rents
#' @export
#' @importFrom bslib page_fluid run_with_themer
#' @importFrom lubridate today days
#' @importFrom pkgload load_all
#' @importFrom shiny reactive
mod_market_survey_rents_demo <- function() {

  pkgload::load_all()

  ui <- bslib::page_fluid(
    title = "Demo",
    theme = app_theme(),
    lang = "en",
    mod_market_survey_rents_ui("demo")
  )

  server <- function(input, output, session) {

    session$userData$user <- shiny::reactive({
      user_id <- 1
      user_email <- "jimmy.briggs@noclocks.dev"
      user_role <- "admin"
      list(
        user_id = user_id,
        email = user_email,
        role = user_role
      )
    })

    pool <- db_connect(user_id = 1)

    mod_market_survey_rents_server(
      "demo",
      pool = pool,
      selected_property = shiny::reactive({ "739085" }),
      selected_leasing_week = shiny::reactive({
        get_weekly_period_start_date(
          as_of_date = lubridate::today() - lubridate::days(6)
        )
      })
    )
  }

  bslib::run_with_themer(shinyApp(ui, server))

}

#' @importFrom dplyr pull filter select tbl collect
#' @importFrom pool poolCheckout poolReturn
db_get_mkt_rents_by_floorplan <- function(conn, property_id, leasing_week_start_date) {

  check_db_conn(conn)

  if (inherits(conn, "Pool")) {
    conn <- pool::poolCheckout(conn)
    on.exit(pool::poolReturn(conn), add = TRUE)
  }

  property_id <- as.character(property_id)

  competitor_property_ids <- dplyr::tbl(conn, I("mkt.competitors")) |>
    dplyr::select(
      competitor_id,
      associated_property_id
    ) |>
    dplyr::filter(
      .data$associated_property_id == .env$property_id
    ) |>
    dplyr::pull("competitor_id")

  prop_ids <- c(property_id, competitor_property_ids)
  # leasing_week_id <- db_get_weekly_period_id(conn, leasing_week)

  dplyr::tbl(conn, I("mkt.rents_by_floorplan")) |>
    dplyr::filter(
      property_id %in% prop_ids,
      leasing_week_start == leasing_week_start_date
    ) |>
    dplyr::collect()
}

#' @importFrom dplyr pull filter select tbl collect
#' @importFrom pool poolCheckout poolReturn
db_get_avg_rents_by_unit_type <- function(conn, property_id, leasing_week_start_date) {

  check_db_conn(conn)

  if (inherits(conn, "Pool")) {
    conn <- pool::poolCheckout(conn)
    on.exit(pool::poolReturn(conn), add = TRUE)
  }

  property_id <- as.character(property_id)

  competitor_property_ids <- dplyr::tbl(conn, I("mkt.competitors")) |>
    dplyr::select(
      competitor_id,
      associated_property_id
    ) |>
    dplyr::filter(
      .data$associated_property_id == .env$property_id
    ) |>
    dplyr::pull("competitor_id")

  prop_ids <- c(property_id, competitor_property_ids)
  # leasing_week_id <- db_get_weekly_period_id(conn, leasing_week)

  dplyr::tbl(conn, I("mkt.avg_rents_by_unit_type")) |>
    dplyr::filter(
      property_id %in% prop_ids,
      leasing_week_start == leasing_week_start_date
    ) |>
    dplyr::collect()
}
