library(shiny)
library(bslib)
library(shinyWidgets)
library(bsicons)

db_config <- config::get("db")

ui <- bslib::page_navbar(
  title = "Market Survey",
  theme = bslib::bs_theme(version = 5),
  sidebar = bslib::sidebar(
    title = "Filters",
    shiny::selectInput("selected_portfolio", "Portfolio", choices = NULL),
    shiny::selectInput("selected_property", "Property", choices = NULL),
    shiny::selectInput("selected_competitor", "Competitor", choices = NULL),
    shinyWidgets::airDatepickerInput("selected_week", "Leasing Week", value = Sys.Date())
  ),
  nav_panel(
    title = "Property Summary",
    icon = bsicons::bs_icon("building"),
    mod_property_summary_ui("property_summary")
  ),
  nav_panel(
    title = "Leasing Summary",
    icon = bsicons::bs_icon("calendar"),
    mod_leasing_summary_ui("leasing_summary")
  )
)

server <- function(input, output, session) {
  # Database pool (global connection)
  pool <- db_connect(user_id = 1)
  check_db_conn(pool)

  # Populate global filters
  observe({
    portfolios <- DBI::dbGetQuery(pool, "SELECT portfolio_id, name FROM gmh.portfolios")
    updateSelectInput(session, "selected_portfolio", choices = setNames(portfolios$portfolio_id, portfolios$name))
  })

  observeEvent(input$selected_portfolio, {
    properties <- DBI::dbGetQuery(pool, glue::glue("
      SELECT property_id, name FROM gmh.properties
      WHERE portfolio_id = {input$selected_portfolio}
    "))
    updateSelectInput(session, "selected_property", choices = setNames(properties$property_id, properties$name))
  })

  observeEvent(input$selected_property, {
    competitors <- DBI::dbGetQuery(pool, glue::glue("
      SELECT competitor_property_id, name FROM mkt.competitors
      JOIN gmh.properties ON competitor_property_id = property_id
      WHERE related_to_property_id = {input$selected_property}
    "))
    updateSelectInput(session, "selected_competitor", choices = setNames(competitors$competitor_property_id, competitors$name))
  })

  # Pass global filters and database connection to modules
  callModule(mod_property_summary_server, "property_summary", pool = pool, filters = reactive(input))
  callModule(mod_leasing_summary_server, "leasing_summary", pool = pool, filters = reactive(input))

  # Clean up pool on session end
  session$onSessionEnded(function() {
    pool::poolClose(pool)
  })
}

