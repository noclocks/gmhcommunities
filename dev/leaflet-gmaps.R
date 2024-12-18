library(leaflet)
library(googleway)
library(shiny)
library(dplyr)

# Initialize API key
google_key <- get_gmaps_config("api_key")
set_key(key = google_key)

# Create the base leaflet map
create_property_map <- function(properties_df) {
  leaflet() |>
    addTiles() |>
    setView(lng = mean(properties_df$longitude),
            lat = mean(properties_df$latitude),
            zoom = 10) |>
    addMarkers(
      data = properties_df,
      lng = ~longitude,
      lat = ~latitude,
      popup = ~paste("<b>", property_name, "</b><br>",
                     "Type: ", property_type, "<br>",
                     "Units: ", units),
      clusterOptions = markerClusterOptions()
    )
}[1][7]

# Add custom marker colors for different portfolio groups
add_portfolio_markers <- function(map, properties_df) {
  pal <- colorFactor(palette = "viridis",
                     domain = properties_df$portfolio_group)

  map %>%
    addCircleMarkers(
      data = properties_df,
      lng = ~longitude,
      lat = ~latitude,
      color = ~pal(portfolio_group),
      radius = 8,
      popup = ~paste("<b>", property_name, "</b><br>",
                     "Portfolio: ", portfolio_group),
      label = ~property_name
    ) %>%
    addLegend(
      position = "bottomright",
      pal = pal,
      values = ~portfolio_group,
      title = "Portfolio Groups"
    )
}[2][5]

# Add competitor properties with different styling
add_competitors <- function(map, competitors_df) {
  map %>%
    addCircleMarkers(
      data = competitors_df,
      lng = ~longitude,
      lat = ~latitude,
      color = "red",
      radius = 6,
      popup = ~paste("<b>", name, "</b><br>",
                     "Distance: ", distance_to_property, " miles<br>",
                     "Avg Rent: $", avg_rent),
      group = "Competitors"
    )
}[1][7]

# Shiny server logic for dynamic updates
server <- function(input, output, session) {
  output$property_map <- renderLeaflet({
    base_map <- create_property_map(properties_df)
    base_map %>%
      add_portfolio_markers(properties_df) %>%
      add_competitors(competitors_df) %>%
      add_layer_controls()
  })

  # Handle map click events
  observeEvent(input$property_map_marker_click, {
    click <- input$property_map_marker_click
    updateSelectInput(session, "selected_property",
                      selected = click$id)
  })
}[7][8]


