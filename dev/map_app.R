library(shiny)
library(leaflet)
library(bslib)

# Create the property data with all fields
properties <- data.frame(
  property_id = c("739085", "boylston", "vanness", "bower"),
  property_name = c("1047 Commonwealth Avenue", "1330 Boylston", "Van Ness", "Bower"),
  address = c(
    "1047 Commonwealth Ave, Boston, MA, 02215",
    "1330 Boylston St, Boston, MA, 02215",
    "1335 Boylston St, Boston, MA, 02215",
    "771 Beacon St Apartment 775, Boston, MA, 02215"
  ),
  lat = c(42.3522312, 42.3441492, 42.3442548, 42.347404),
  lon = c(-71.1226248, -71.0983916, -71.0996707, -71.1010124),
  is_competitor = c(FALSE, TRUE, TRUE, TRUE),
  website = c(
    "https://www.1047commonwealth.com/",
    "https://1330boylston.com/",
    "https://www.thevanness.com/",
    "https://bowerboston.com/"
  ),
  phone_number = c("(617) 500-6481", "(617) 267-1330", "(617) 424-1335", "(617) 341-9700"),
  property_image = c(
    "https://images1.forrent.com/i2/0ksd7Uwm-74efUhB-UiU1cEyVv3NVOiCCVuF4oQ7cZI/117/image.jpg",
    "", "", ""
  ),
  developer = c("BPDA", "Samuels and Associates", "Samuels And Associates", "The Green Cities Company"),
  manager = c("GMH Communities", "Samuels and Associates", "Samuels And Associates", "Greystar"),
  owner = c("AGC + GMH Communities", "Samuels and Associates", "Samuels And Associates", "Greystar"),
  property_type = c("Student", "Conventional", "Conventional", "Conventional"),
  property_status = c("Operational", "Operational", "Operational", "Operational"),
  product_type = c("Mid-Rise", "High-Rise", "High-Rise", "High-Rise"),
  property_rating = c(2, 5, 4, 5),
  comp_status = c("Subject Property", "Tier 2", "Tier 2", "Tier 2"),
  year_built = c(2017, 2008, 2015, 2020),
  most_recent_sale = c("2019-01-01", "", "", ""),
  distance_from_campus = c(0.1, 1, 1, 0.2)
)

ui <- page_fluid(
  theme = bs_theme(bootswatch = "flatly"),
  card(
    card_header("Boston Properties Map"),
    leafletOutput("map", height = "600px")
  )
)

server <- function(input, output, session) {

  db_data <- shiny::reactive({
    db_query_mkt_locations()
  })


  output$map <- renderLeaflet({
    # Create custom icons based on property type
    icons <- ifelse(properties$is_competitor,
                    "info-sign",  # competitor icon
                    "home"        # subject property icon
    )

    colors <- ifelse(properties$is_competitor,
                     "blue",   # competitor color
                     "red"     # subject property color
    )

    iconSet <- awesomeIconList(
      home = makeAwesomeIcon(icon = "home", markerColor = "red", library = "glyphicon"),
      info = makeAwesomeIcon(icon = "info-sign", markerColor = "blue", library = "glyphicon")
    )

    leaflet(properties) %>%
      addTiles() %>%
      addAwesomeMarkers(
        ~lon, ~lat,
        icon = ~ifelse(is_competitor, iconSet$info, iconSet$home),
        popup = ~paste0(
          "<div style='min-width: 200px;'>",
          "<h4>", property_name, "</h4>",
          "<p><strong>Property Details:</strong><br>",
          "Type: ", property_type, " (", product_type, ")<br>",
          "Status: ", property_status, "<br>",
          "Year Built: ", year_built, "<br>",
          "Rating: ", property_rating, " stars<br>",
          "Distance from Campus: ", distance_from_campus, " miles</p>",

          "<p><strong>Contact:</strong><br>",
          "Phone: ", phone_number, "<br>",
          "<a href='", website, "' target='_blank'>Visit Website</a></p>",

          "<p><strong>Management:</strong><br>",
          "Developer: ", developer, "<br>",
          "Manager: ", manager, "<br>",
          "Owner: ", owner, "</p>",

          ifelse(property_image != "",
                 paste0("<img src='", property_image,
                        "' style='max-width: 200px; margin-top: 10px;'>"),
                 ""),
          "</div>"
        ),
        label = ~property_name
      ) %>%
      setView(
        lng = mean(properties$lon),
        lat = mean(properties$lat),
        zoom = 15
      )
  })
}

shinyApp(ui, server)
