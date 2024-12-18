library(shiny)
library(bslib)
library(reactable)
library(rhandsontable)
library(shinyvalidate)
library(fontawesome)
library(DT)
library(shinyWidgets)

# Simulate database data
db_data <- data.frame(
  property_id = c("739085", "boylston", "bower", "vanness"),
  property_name = c("1047 Commonwealth Avenue", "1330 Boylston", "Bower", "Van Ness"),
  leasing_week_start = as.Date(rep("2024-12-01", 4)),
  website = c("http://www.1047commonwealth.com/", NA, NA, NA),
  address = c("1047 Commonwealth Ave, Boston, MA 02215",
              "1330 Boylston St, Boston, MA 02215",
              "771 Beacon St Apartment 775, Boston, MA 02215",
              "1335 Boylston St, Boston, MA, 02215"),
  phone_number = c("617-500-6481", "(617) 267-1330", "(617) 341-9700", "617-424-1335"),
  developer = c("BPDA", "Samuels and Associates", "The Green Cities Company", "Samuels And Associates"),
  manager = c("GMH Communities", "Samuels and Associates", "Greystar", "Samuels And Associates"),
  owner = c("AGC + GMH Communities", "Samuels and Associates", "Greystar", "Samuels And Associates"),
  property_type = c("Student", "Conventional", "Conventional", "Conventional"),
  property_status = c("Operational", "Operational", "Operational", "Operational"),
  product_type = c("Mid-Rise", "High-Rise", "High-Rise", "High-Rise"),
  property_rating = c(2, 5, 5, 4),
  comp_status = c("Subject Property", "Tier 2", "Tier 2", "Tier 2"),
  year_built = c(2017, 2008, 2020, 2015),
  most_recent_sale = as.Date(c("2019-01-01", NA, NA, NA)),
  distance_from_campus = c(0.1, 1, 0.2, 1)
)

ui <- page_fluid(
  theme = bs_theme(version = 5),
  navset_card_tab(
    id = "id",
    title = "GMH Communities Market Survey",
    nav_panel(
      "Property Summary",
      layout_sidebar(
        sidebar = sidebar(
          title = "Filters",
          selectInput("property", "Select Property",
                      choices = unique(db_data$property_name)),
          dateRangeInput("leasing_week", "Leasing Week",
                         start = Sys.Date(), end = Sys.Date() + 7)
        ),

        card(
          card_header("Progress"),
          progressBar(id = "section_progress", value = 0,
                      title = "Section Progress"),
          progressBar(id = "overall_progress", value = 0,
                      title = "Overall Progress")
        ),

        layout_column_wrap(
          width = 1/2,
          card(
            card_header("Property Details"),
            textInput("property_name", "Property Name", value = ""),
            textInput("property_website", "Website URL"),
            textInput("property_address", "Address"),
            textInput("property_phone", "Phone Number"),
            fileInput("property_image", "Property Image"),
            textInput("property_developer", "Developer"),
            textInput("property_manager", "Property Manager"),
            textInput("property_owner", "Property Owner")
          ),

          card(
            card_header("Property Classification"),
            radioButtons("property_type", "Property Type",
                         choices = c("Student", "Conventional", "Affordable", "Innovative")),
            radioButtons("property_status", "Property Status",
                         choices = c("Operational", "New Construction", "Undergoing Renovations")),
            selectInput("product_type", "Product Type",
                        choices = c("High-Rise", "Mid-Rise", "Wrap", "Garden", "Cottage", "SFR")),
            sliderInput("property_rating", "Property Rating",
                        min = 0.5, max = 5, value = 3, step = 0.5),
            radioButtons("comp_status", "Competition Status",
                         choices = c("Subject Property", "Tier 1", "Tier 2")),
            dateInput("year_built", "Year Built"),
            dateInput("most_recent_sale", "Most Recent Sale"),
            numericInput("distance_from_campus", "Distance from Campus (miles)",
                         value = 0, min = 0, step = 0.1)
          )
        ),

        card(
          card_header("Current vs. Historical Data"),
          reactableOutput("comparison_table")
        ),

        card(
          card_header("Bulk Edit"),
          rHandsontableOutput("bulk_edit")
        ),

        layout_column_wrap(
          width = 1/3,
          input_task_button("save", "Save Changes"),
          input_task_button("refresh", "Refresh Data"),
          actionButton("launch_survey", "Launch Survey Form",
                       class = "btn-primary w-100")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Initialize input validator
  iv <- InputValidator$new()
  iv$add_rule("property_name", sv_required())
  iv$add_rule("property_website", sv_url())
  iv$add_rule("property_phone", sv_regex("^[0-9-()]+$"))
  iv$enable()

  # Reactive values for storing current data
  values <- reactiveValues(
    current_data = NULL,
    section_progress = 0
  )

  # Update progress bars
  observe({
    updateProgressBar(session, "section_progress", value = values$section_progress)
    updateProgressBar(session, "overall_progress", value = values$section_progress/3)
  })

  # Comparison table
  output$comparison_table <- renderReactable({
    reactable(db_data[1,], bordered = TRUE, striped = TRUE)
  })

  # Bulk edit table
  output$bulk_edit <- renderRHandsontable({
    rhandsontable(db_data[1,], stretchH = "all")
  })

  # Image preview
  output$property_image_preview <- renderImage({
    if (is.null(input$property_image)) return(NULL)
    list(src = input$property_image$datapath,
         contentType = input$property_image$type,
         width = 400)
  }, deleteFile = TRUE)
}

shinyApp(ui, server)
