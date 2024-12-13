# in UI:

current_week_start <- get_weekly_period_start_date()
current_week_end <- get_weekly_period_end_date()
initial_value <- c(current_week_start, current_week_end)

shinyWidgets::airDatepickerInput(
  ns("leasing_week"),
  label = icon_text("calendar-alt", "Leasing Week", .function = shiny::icon),
  value = initial_value,
  range = TRUE,
  firstDay = 1,
  clearButton = FALSE,
  autoClose = TRUE,
  inline = FALSE
)

# in Server:

# update leasing week input with selected date's week period (i.e. Monday-Sunday)
shiny::observe({
  shiny::req(input$leasing_week)

  start_date <- input$leasing_week[1]
  end_date <- input$leasing_week[2]

  leasing_period_start_date <- get_weekly_period_start_date(start_date)
  leasing_period_end_date <- get_weekly_period_end_date(start_date)

  if (start_date != leasing_period_start_date || end_date != leasing_period_end_date) {

    shinyWidgets::updateAirDateInput(
      session = session,
      inputId = "leasing_week",
      value = c(leasing_period_start_date, leasing_period_end_date)
    )

    cli::cli_alert_info(
      c(
        "Selected week has been adjusted to the leasing week period of the selected date range.",
        "Selected week: {.field {start_date}} - {.field {end_date}}",
        "Adjusted week: {.field {leasing_period_start_date}} - {.field {leasing_period_end_date}}"
      )
    )
  }

}) |>
  shiny::bindEvent(input$leasing_week)
