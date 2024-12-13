library(shiny)
library(bslib)
library(rhandsontable)

process_table_data <- function(data) {
  # Convert ENUM fields to character
  for (col in names(data)) {
    if (inherits(data[[col]], "pq_enum")) {
      data[[col]] <- as.character(data[[col]])
    }
  }

  # Format Date/Time fields
  for (col in names(data)) {
    if (inherits(data[[col]], "Date") || inherits(data[[col]], "POSIXt")) {
      data[[col]] <- format(as.Date(data[[col]], origin = "1970-01-01"), "%Y-%m-%d")
    }
  }

  return(data)
}


ui <- page_fillable(
  theme = bs_theme(version = 5),

  navset_card_pill(
    # Property Summary Tab
    nav_panel(
      "Property Summary",
      layout_column_wrap(
        width = 1,
        card(
          card_header(h3("Property Summary", align = "center")),
          card_body(rhandsontable::rHandsontableOutput("property_summary_table")),
          card_footer(
            actionButton("submit_property_summary", "Submit Changes", class = "btn-primary")
          )
        )
      )
    ),

    # Leasing Summary Tab
    nav_panel(
      "Leasing Summary",
      layout_column_wrap(
        width = 1,
        card(
          card_header(h3("Leasing Summary", align = "center")),
          card_body(rhandsontable::rHandsontableOutput("leasing_summary_table")),
          card_footer(
            actionButton("submit_leasing_summary", "Submit Changes", class = "btn-primary")
          )
        )
      )
    ),

    # Short Term Leases Tab
    nav_panel(
      "Short Term Leases",
      layout_column_wrap(
        width = 1,
        card(
          card_header(h3("Short Term Leases", align = "center")),
          card_body(rhandsontable::rHandsontableOutput("short_term_leases_table")),
          card_footer(
            actionButton("submit_short_term_leases", "Submit Changes", class = "btn-primary")
          )
        )
      )
    ),

    # Utilities Summary Tab
    nav_panel(
      "Utilities Summary",
        card(
          card_title(h3("Utilities Summary", align = "center")),
          htmltools::HTML("<center>"),
          actionButton("submit_utilities_summary", "Submit Changes", class = "btn-primary", width = "33%", style = "align:center"),
          card_body(
            rhandsontable::rHandsontableOutput("utilities_summary_table")
          )
        )
    ),

    # Rents by Floorplan Tab
    nav_panel(
      "Rents by Floorplan",
      layout_column_wrap(
        width = 1,
        card(
          card_header(h3("Rents by Floorplan", align = "center")),
          card_body(rhandsontable::rHandsontableOutput("rents_by_floorplan_table")),
          card_footer(
            actionButton("submit_rents_by_floorplan", "Submit Changes", class = "btn-primary")
          )
        )
      )
    ),

    # Average Rents by Unit Type Tab
    nav_panel(
      "Average Rents by Unit Type",
      layout_column_wrap(
        width = 1,
        card(
          card_header(h3("Average Rents by Unit Type", align = "center")),
          card_body(rhandsontable::rHandsontableOutput("average_rents_by_unit_type_table")),
          card_footer(
            actionButton("submit_average_rents_by_unit_type", "Submit Changes", class = "btn-primary")
          )
        )
      )
    ),

    # Notes Tab
    nav_panel(
      "Notes",
      layout_column_wrap(
        width = 1,
        card(
          card_header(h3("Notes and Office Hours", align = "center")),
          card_body(rhandsontable::rHandsontableOutput("notes_table")),
          card_footer(
            actionButton("submit_notes", "Submit Changes", class = "btn-primary")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  library(DBI)
  library(RPostgres)

  # Database Connection
  con <- DBI::dbConnect(
    drv = RPostgres::Postgres(),
    dbname = db_config$dbname,
    host = db_config$host,
    port = db_config$port,
    user = db_config$user,
    password = db_config$password
  )

  onStop(function() {
    dbDisconnect(con)
  })

  prop_summary_tbl <- shiny::reactive({

    tbl <- dplyr::tbl(con, I("mkt.properties")) |>
      dplyr::filter(property_id == 1) |>
      dplyr::select(
        property_id,
        property_name,
        website,
        address,
        city,
        state,
        zip_code,
        phone_number,
        developer,
        manager,
        owner,
        property_type,
        property_status,
        product_type,
        property_rating,
        comp_status,
        year_built,
        year_renovated,
        most_recent_sale,
        distance_from_campus,
        created_at,
        updated_at
      ) |>
      dplyr::collect() |>
      t() |>
      as.data.frame() |>
      tibble::rownames_to_column("attribute") |>
      setNames(c("attribute", "value")) |>
      tibble::as_tibble()

  })

  # Render Property Summary
  output$property_summary_table <- rhandsontable::renderRHandsontable({
    data <- prop_summary_tbl()
    rhandsontable::rhandsontable(
      data,
      colHeaders = c("Attribute", "Value"),
      stretchH = "all",
      search = TRUE
    ) |>
      rhandsontable::hot_table(
        contextMennu = TRUE,
        stretchH = "all",
        highlightCol = TRUE,
        highlightRow = TRUE,
        enableComments = TRUE,
        overflow = "auto"
      ) |>
      rhandsontable::hot_col(
        col = "Attribute",
        readOnly = TRUE
      ) |>
      rhandsontable::hot_col(
        col = "Value",
        readOnly = FALSE
      )

  })

  observeEvent(input$submit_property_summary, {
    updated_data <- rhandsontable::hot_to_r(input$property_summary_table)
    dbWriteTable(con, "mkt.properties", updated_data, overwrite = TRUE, row.names = FALSE)
    showModal(modalDialog(title = "Success", "Property Summary updated successfully!", easyClose = TRUE))
  })

  # Render Leasing Summary
  output$leasing_summary_table <- renderRHandsontable({
    data <- dbGetQuery(con, "SELECT * FROM mkt.leasing_summary WHERE property_id = 1")
    data <- process_table_data(data)
    rhandsontable::rhandsontable(data, stretchH = "all")
  })


  observeEvent(input$submit_leasing_summary, {
    updated_data <- rhandsontable::hot_to_r(input$leasing_summary_table)
    dbWriteTable(con, "mkt.leasing_summary", updated_data, overwrite = TRUE, row.names = FALSE)
    showModal(modalDialog(title = "Success", "Leasing Summary updated successfully!", easyClose = TRUE))
  })

  # Render Utilities Summary
  output$utilities_summary_table <- renderRHandsontable({
    data <- dbGetQuery(con, "SELECT * FROM mkt.utilities WHERE property_id = 1")
    data <- process_table_data(data) |> dplyr::select(-c("utility_id", "property_id", "created_at"))

    # browser()

    rhandsontable::rhandsontable(
      data,
      width = "100%",
      colHeaders = c("Utility", "Included?", "Cost ($)", "Cost Frequency")
    ) |>
      rhandsontable::hot_table(
        contextMennu = TRUE,
        stretchH = "all",
        highlightCol = TRUE,
        highlightRow = TRUE,
        enableComments = TRUE,
        overflow = "auto"
      ) |>
      rhandsontable::hot_col(
        col = "Utility",
        type = "dropdown",
        source = c("Electricity", "Water", "Gas", "Trash", "Sewer", "Internet", "Cable/Satellite", "Other"),
        allowInvalid = TRUE,
        copyable = TRUE
      ) |>
      rhandsontable::hot_col(
        col = "Included?",
        type = "checkbox"
      ) |>
      rhandsontable::hot_col(
        col = "Cost ($)",
        type = "numeric",
        format = "$0,0.00"
      ) |>
      rhandsontable::hot_col(
        col = "Cost Frequency",
        type = "dropdown",
        source = c("Monthly", "Quarterly", "Annually")
      )
  })

  observeEvent(input$submit_utilities_summary, {
    updated_data <- rhandsontable::hot_to_r(input$utilities_summary_table)
    dbWriteTable(con, "mkt.utilities", updated_data, overwrite = TRUE, row.names = FALSE)
    showModal(modalDialog(title = "Success", "Utilities Summary updated successfully!", easyClose = TRUE))
  })

  # Render Rents by Floorplan
  output$rents_by_floorplan_table <- renderRHandsontable({
    data <- dbGetQuery(con, "SELECT * FROM mkt.rents_by_floorplan WHERE property_id = 1")
    data <- process_table_data(data)
    rhandsontable::rhandsontable(data, stretchH = "all")
  })

  observeEvent(input$submit_rents_by_floorplan, {
    updated_data <- rhandsontable::hot_to_r(input$rents_by_floorplan_table)
    dbWriteTable(con, "mkt.rents_by_floorplan", updated_data, overwrite = TRUE, row.names = FALSE)
    showModal(modalDialog(title = "Success", "Rents by Floorplan updated successfully!", easyClose = TRUE))
  })

  # Render Average Rents by Unit Type
  output$average_rents_by_unit_type_table <- renderRHandsontable({
    data <- dbGetQuery(con, "SELECT * FROM mkt.average_rents_by_unit_type WHERE property_id = 1")
    data <- process_table_data(data)
    rhandsontable::rhandsontable(data, stretchH = "all")
  })

  observeEvent(input$submit_average_rents_by_unit_type, {
    updated_data <- rhandsontable::hot_to_r(input$average_rents_by_unit_type_table)
    dbWriteTable(con, "mkt.average_rents_by_unit_type", updated_data, overwrite = TRUE, row.names = FALSE)
    showModal(modalDialog(title = "Success", "Average Rents by Unit Type updated successfully!", easyClose = TRUE))
  })

  # Render Notes
  output$notes_table <- renderRHandsontable({
    data <- dbGetQuery(con, "SELECT * FROM mkt.notes WHERE property_id = 1")
    data <- process_table_data(data)
    rhandsontable::rhandsontable(data, stretchH = "all")
  })

  observeEvent(input$submit_notes, {
    updated_data <- rhandsontable::hot_to_r(input$notes_table)
    dbWriteTable(con, "mkt.notes", updated_data, overwrite = TRUE, row.names = FALSE)
    showModal(modalDialog(title = "Success", "Notes updated successfully!", easyClose = TRUE))
  })

}

shinyApp(ui, server)


# qry <- 'SELECT
#   property_id,
#   property_name,
#   website,
#   address,
#   city,
#   state,
#   zip_code,
#   phone_number,
#   developer,
#   manager,
#   owner,
#   CAST(property_type AS TEXT) AS property_type,
#   CAST(property_status AS TEXT) AS property_status,
#   CAST(product_type AS TEXT) AS product_type,
#   CAST(property_rating AS TEXT) AS property_rating,
#   CAST(comp_status AS TEXT) AS comp_status,
#   year_built,
#   year_renovated,
#   most_recent_sale,
#   distance_from_campus,
#   CAST(created_at AS TEXT) AS created_at,
#   CAST(updated_at AS TEXT) AS updated_at
# FROM mkt.properties
# WHERE property_id = 1;'
