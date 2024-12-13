db_get_gmh_portfolios <- function(conn) {

  check_db_conn(conn)

  hold <- dplyr::tbl(conn, I("gmh.portfolios")) |>
    dplyr::select(
      portfolio_id,
      portfolio_name
    )

  ids <- dplyr::pull(hold, "portfolio_id")
  names(ids) <- dplyr::pull(hold, "portfolio_name")
  return(ids)

}

update_portfolio_input <- function(session, id, choices = db_get_gmh_portfolios(session$pool)) {

  label <- htmltools::span(
    shiny::icon("building"),
    " Select a Portfolio:"
  )

  shinyWidgets::pickerInput(
    inputId = id,
    label = label,
    multiple = FALSE,
    width = "100%",
    choices = choices,
    selected = choices[[1]],
    choicesOpt = list(
      icon = shiny::icon("building")
    ),
    options = list(
      `actions-box` = TRUE,
      `live-search` = TRUE,
      `selected-text-format` = "count > 3"
    )
  )

}
