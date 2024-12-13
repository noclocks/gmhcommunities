library(shiny)
library(bslib)
library(bsicons)

ui <- page_navbar(
  title = htmltools::tags$div(
    class = "navbar-brand",
    htmltools::tags$a(
      href = "#",
      htmltools::tags$img(
        src = "https://cdn.brandfetch.io/gmhcommunities.com/logo",
        height = 50,
        width = "auto",
        alt = "GMH Logo"
      )
    )
  ),
  window_title = "GMH Leasing Dashboard",
  theme = bs_theme(version = 5),
  sidebar = sidebar(
    title = "Controls",
    selectInput("property", "Select Property", choices = c("Property A", "Property B", "Property C")),
    dateRangeInput("daterange", "Date Range"),
    bslib::input_task_button("entrata_refresh", label = "Refresh Data", icon = shiny::icon("refresh"))
  ),
  nav_spacer(),
  nav_panel("Dashboard", icon = bs_icon("speedometer2"),
            layout_columns(
              value_box(
                title = "Occupancy Rate",
                value = "95%",
                showcase = bs_icon("house-check"),
                theme = "primary"
              ),
              value_box(
                title = "New Leases",
                value = "23",
                showcase = bs_icon("file-earmark-text"),
                theme = "success"
              ),
              value_box(
                title = "Revenue",
                value = "$127,500",
                showcase = bs_icon("currency-dollar"),
                theme = "info"
              )
            ),
            layout_columns(
              card(
                card_header(bs_icon("graph-up"), "Key Metrics"),
                card_body(
                  "This is where you would display additional metrics or charts."
                )
              ),
              card(
                card_header(bs_icon("clock-history"), "Recent Activity"),
                card_body(
                  "This section could show recent updates or activities."
                )
              )
            )
  ),
  nav_panel("Summary", icon = bs_icon("bar-chart-line"),
            card(
              card_header(bs_icon("clipboard-data"), "Data Summary"),
              card_body(
                "Here you can display summary statistics or visualizations."
              )
            )
  ),
  nav_panel("Reports", icon = bs_icon("file-earmark-text"),
            card(
              card_header(bs_icon("folder"), "Available Reports"),
              card_body(
                "List of available reports or report generation options would go here."
              )
            )
  ),
  nav_spacer(),
  nav_menu(
    title = "Contact",
    icon = bs_icon("envelope"),
    nav_item("Email Support", icon = bs_icon("envelope"), href = "mailto:support@example.com"),
    nav_item("Phone Support", icon = bs_icon("telephone"), href = "tel:+1234567890"),
    nav_item("Live Chat", icon = bs_icon("chat-dots"), href = "#")
  ),
  nav_menu(
    title = "User",
    icon = bs_icon("person-circle"),
    align = "right",
    nav_item("Profile", icon = bs_icon("person"), href = "#"),
    nav_item("Settings", icon = bs_icon("gear"), href = "#"),
    nav_item("Logout", icon = bs_icon("box-arrow-right"), href = "#")
  )
)

server <- function(input, output, session) {
  # Server logic can be added here if needed
}

shinyApp(ui, server)
