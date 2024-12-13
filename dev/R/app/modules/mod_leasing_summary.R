
#  ------------------------------------------------------------------------
#
# Title : leasing_summary Shiny Module
#    By : Jimmy Briggs
#  Date : 2024-11-27
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' leasing_summary Shiny Module
#'
#' @name mod_leasing_summary
#'
#' @description
#' A Shiny Module for ...
#'
#' - `mod_leasing_summary_ui()`: User interface
#' - `mod_leasing_summary_server()`: Server logic
#'
#' @param id Module's namespace ID.
#'
#' @return
#' - `mod_leasing_summary_ui()`: UI HTML Output.
#' - `mod_leasing_summary_server()`: Reactive values returned from server logic.
#'
#' @examples
#' if (interactive()) {
#'   mod_leasing_summary_demo()
#' }
NULL


# UI ----------------------------------------------------------------------

#' @rdname mod_leasing_summary
#' @export
#' @importFrom shiny NS
#' @importFrom htmltools tagList tags
mod_leasing_summary_ui <- function(id) {

  ns <- shiny::NS(id)

  htmltools::tagList(
    bslib::card(
      bslib::card_header(
        htmltools::tags$span(bsicons::bs_icon("file-earmark-text"), "Leasing Summary"),
        class = "bg-light"
      ),
      bslib::card_body(
        class = "p-2",
        htmltools::tags$div(
          class = "container-fluid p-0",
          style = "font-size: 0.9rem;",
          htmltools::tags$div(
            class = "row g-2",
            shiny::selectInput(ns("reporting_cycle"), label = htmltools::tags$span(bsicons::bs_icon("calendar-week"), "Reporting Cycle"),
                        choices = c("Saturday-Friday", "Sunday-Saturday"), selected = "Saturday-Friday"),
            shiny::dateInput(ns("lease_launch_date"), label = htmltools::tags$span(bsicons::bs_icon("calendar-check"), "Lease Launch Date"),
                      value = "2023-11-27"),
            shiny::dateInput(ns("renewal_launch_date"), label = htmltools::tags$span(bsicons::bs_icon("calendar-plus"), "Renewal Launch Date"),
                      value = "2023-10-23"),
            shiny::numericInput(ns("current_occupancy"), label = htmltools::tags$span(bsicons::bs_icon("percent"), "Current Occupancy (%)"),
                         value = 99.5),
            shiny::numericInput(ns("last_year_occupancy"), label = htmltools::tags$span(bsicons::bs_icon("clock-history"), "Last Year Occupancy (%)"),
                         value = 0),
            shiny::numericInput(ns("current_prelease"), label = htmltools::tags$span(bsicons::bs_icon("graph-up"), "Current Pre-Lease (%)"),
                         value = 38.8),
            shiny::numericInput(ns("last_year_prelease"), label = htmltools::tags$span(bsicons::bs_icon("clock-history"), "Last Year Pre-Lease (%)"),
                         value = NA),
            shiny::numericInput(ns("total_renewals"), label = htmltools::tags$span(bsicons::bs_icon("arrow-repeat"), "Total Renewals"),
                         value = 51),
            shiny::numericInput(ns("total_new_leases"), label = htmltools::tags$span(bsicons::bs_icon("file-earmark-plus"), "Total New Leases"),
                         value = 20),
            shiny::numericInput(ns("weekly_traffic"), label = htmltools::tags$span(bsicons::bs_icon("people"), "Total Weekly Traffic"),
                         value = 58),
            shiny::selectInput(ns("current_incentive"), label = htmltools::tags$span(bsicons::bs_icon("gift"), "Current Incentive"),
                        choices = c("None", "Gift Card", "Monthly Concession", "One-Time Concession"), selected = "None"),
            shiny::numericInput(ns("incentive_amount"), label = htmltools::tags$span(bsicons::bs_icon("cash-stack"), "Incentive Amount ($)"),
                         value = 0),
            shiny::dateInput(ns("data_last_updated"), label = htmltools::tags$span(bsicons::bs_icon("clock"), "Data Last Updated"),
                      value = "2024-02-22")
          )
        )
      )
    )
  )
}


# server ------------------------------------------------------------------

#' @rdname mod_leasing_summary
#' @export
#' @importFrom shiny moduleServer
#' @importFrom cli cat_rule
mod_leasing_summary_server <- function(id) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      cli::cat_rule("mod_leasing_summary_server")

      inputs <- shiny::reactive({
        list(
          reporting_cycle = input$reporting_cycle,
          lease_launch_date = input$lease_launch_date,
          renewal_launch_date = input$renewal_launch_date,
          current_occupancy = input$current_occupancy,
          last_year_occupancy = input$last_year_occupancy,
          current_prelease = input$current_prelease,
          last_year_prelease = input$last_year_prelease,
          total_renewals = input$total_renewals,
          total_new_leases = input$total_new_leases,
          weekly_traffic = input$weekly_traffic,
          current_incentive = input$current_incentive,
          incentive_amount = input$incentive_amount,
          data_last_updated = input$data_last_updated
        )
      })

      return(inputs)

    }
  )
}


# demo --------------------------------------------------------------------

#' @rdname mod_leasing_summary
#' @export
#' @importFrom pkgload load_all
#' @importFrom bslib page_fluid bs_theme
#' @importFrom shiny shinyApp
mod_leasing_summary_demo <- function() {

  pkgload::load_all()

  ui <- bslib::page_fluid(
    title = "Demo",
    theme = bslib::bs_theme(version = 5),
    lang = "en",
    mod_leasing_summary_ui("demo")
  )

  server <- function(input, output, session) {
    mod_leasing_summary_server("demo")
  }

  shiny::shinyApp(ui, server)
}
