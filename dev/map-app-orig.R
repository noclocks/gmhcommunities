library(shiny)
library(leaflet)
library(DT)
library(bslib)
library(tidyverse)

# Create the data frame
properties_data <- tribble(
  ~property_id, ~property_name, ~address, ~lat, ~lon, ~is_competitor, ~website, ~phone_number, ~property_image, ~developer, ~manager, ~owner, ~property_type, ~property_status, ~product_type, ~property_rating, ~comp_status, ~year_built, ~most_recent_sale, ~distance_from_campus,
  "739085", "1047 Commonwealth Avenue", "1047 Commonwealth Ave, Boston, MA, 02215", 42.3522312, -71.1226248, FALSE, "https://www.1047commonwealth.com/", "(617) 500-6481", "https://images1.forrent.com/i2/0ksd7Uwm-74efUhB-UiU1cEyVv3NVOiCCVuF4oQ7cZI/117/image.jpg", "BPDA", "GMH Communities", "AGC + GMH Communities", "Student", "Operational", "Mid-Rise", 2, "Subject Property", 2017, "2019-01-01", 0.1,
  "boylston", "1330 Boylston", "1330 Boylston St, Boston, MA, 02215", 42.3441492, -71.0983916, TRUE, "https://1330boylston.com/", "(617) 267-1330", "", "Samuels and Associates", "Samuels and Associates", "Samuels and Associates", "Conventional", "Operational", "High-Rise", 5, "Tier 2", 2008, NA, 1,
  "vanness", "Van Ness", "1335 Boylston St, Boston, MA, 02215", 42.3442548, -71.0996707, TRUE, "https://www.thevanness.com/", "(617) 424-1335", "", "Samuels And Associates", "Samuels And Associates", "Samuels And Associates", "Conventional", "Operational", "High-Rise", 4, "Tier 2", 2015, NA, 1,
  "bower", "Bower", "771 Beacon St Apartment 775, Boston, MA, 02215", 42.347404, -71.1010124, TRUE, "https://bowerboston.com/", "(617) 341-9700", "", "The Green Cities Company", "Greystar", "Greystar", "Conventional", "Operational", "High-Rise", 5, "Tier 2", 2020, NA, 0.2
)

ui <- page_sidebar(
  title = "Boston Properties",
  sidebar = sidebar(
    title = "Filters",
    checkboxGroupInput("property_type", "Property Type",
                       choices = unique(properties_data$property_type),
                       selected = unique(properties_data$property_type)),
    checkboxGroupInput("product_type", "Product Type",
                       choices = unique(properties_data$product_type),
                       selected = unique(properties_data$product_type))
  ),

  layout_columns(
    fill = FALSE,
    value_box(
      title = "Total Properties",
      value = textOutput("total_properties"),
      showcase = bsicons::bs_icon("building")
    ),
    value_box(
      title = "Average Rating",
      value = textOutput("avg_rating"),
      showcase = bsicons::bs_icon("star")
    )
  ),

  card(
    padding = 0,
    full_screen = TRUE,
    card_header("Property Locations"),
    leafletOutput("map", height = "1500px")
  ),

  card(
    full_screen = TRUE,
    card_header("Property Details"),
    DTOutput("table")
  )
)

server <- function(input, output, session) {

  # Filtered data reactive
  filtered_data <- shiny::reactive({
    db_get_mkt_map_locations(conn = conn, property_id = "739085")
  })

  # Value boxes
  output$total_properties <- renderText({
    nrow(filtered_data())
  })

  output$avg_rating <- renderText({
    round(mean(filtered_data()$gmaps_rating), 1)
  })

  # Map output
  output$map <- renderLeaflet({
    # Create custom icons
    icons <- leaflet::awesomeIcons(
      icon = filtered_data()$map_marker_icon,
      iconColor = 'white',
      library = 'fa',
      markerColor = filtered_data()$map_marker_color
    )

    leaflet(filtered_data()) %>%
      addTiles() %>%
      addAwesomeMarkers(
        ~longitude, ~latitude,,
        icon = icons,
        popup = filtered_data()$map_popup_html
      ) %>%
      addLegend(
        position = "bottomright",
        colors = c("blue", "red"),
        labels = c("Subject Property", "Competitor"),
        title = "Property Type"
      ) %>%
      setView(lng = mean(filtered_data()$longitude),
              lat = mean(filtered_data()$latitude),
              zoom = 14)
  })

  # Table output
  output$table <- renderDT({
    filtered_data() %>%
      select(Property = property_name, Address = address, Phone = phone_number, Website = website) %>%
      datatable(
        options = list(
          pageLength = 10,
          scrollX = TRUE
        )
      )
  })
}

shinyApp(ui, server)
