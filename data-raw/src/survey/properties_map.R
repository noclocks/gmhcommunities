
library(leaflet)

db_config <- get_db_config()
pool <- db_connect()
conn <- pool::poolCheckout(pool)
connections::connection_view(conn)

# location table structure:

# CREATE TABLE IF NOT EXISTS mkt.locations (
#   location_id               INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
#   property_id               TEXT REFERENCES mkt.properties(property_id) ON UPDATE CASCADE ON DELETE SET NULL,
#   property_name             TEXT NOT NULL,
#   is_competitor             BOOLEAN NOT NULL,
#   address                   TEXT NOT NULL,
#   city                      TEXT,
#   state                     TEXT,
#   postal_code               TEXT,
#   country                   TEXT NOT NULL DEFAULT 'USA',
#   region                    TEXT,
#   phone_number              TEXT,
#   email                     TEXT,
#   property_image            TEXT,
#   latitude                  NUMERIC(9,6) NOT NULL CHECK (latitude BETWEEN -90 AND 90),
#   longitude                 NUMERIC(9,6) NOT NULL CHECK (longitude BETWEEN -180 AND 180),
#   location_geometry         GEOGRAPHY(POINT, 4326) NOT NULL,
#   gmaps_address             TEXT,
#   gmaps_name                TEXT,
#   gmaps_place_id            TEXT,
#   gmaps_rating              NUMERIC(2,1) CHECK (gmaps_rating >= 0 AND gmaps_rating <= 5),
#   gmaps_num_of_reviews      INT CHECK (gmaps_num_of_reviews >= 0),
#   gmaps_place_types         TEXT,
#   gmaps_icon                TEXT,
#   gmaps_website             TEXT,
#   gmaps_url                 TEXT,
#   map_layer                 TEXT,
#   map_marker_icon           TEXT,
#   map_marker_icon_color     TEXT NOT NULL DEFAULT 'white',
#   map_marker_color          TEXT,
#   map_popup_html            TEXT,
#   created_at                TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
#   updated_at                TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP
# );


property_data <- dplyr::tbl(conn, dbplyr::in_schema("mkt", "properties")) |>
  dplyr::left_join(
    dplyr::tbl(
      conn,
      dbplyr::in_schema("mkt", "property_summary")
    ) |>
      dplyr::select(
        property_id,
        property_name,
        website:distance_from_campus
      ),
    by = c("property_id", "property_name")
  ) |>
  dplyr::collect()

# commonwealth place id: "ChIJcYT25PZ544kRK42i2BUEzZ8"
#
# enriched_property_data$gmaps_formatted_address[[1]] |> parse_address()

enriched_property_data <- enrich_property_data(property_data) |>
  dplyr::transmute(
    property_id = property_id,
    property_name = property_name,
    is_competitor = is_competitor,
    address = address,
    city = parse_address(.data$gmaps_formatted_address)$city,
    state = parse_address(.data$gmaps_formatted_address)$state_province,
    postal_code = parse_address(.data$gmaps_formatted_address)$postal_code,
    country = parse_address(.data$gmaps_formatted_address)$country,
    region = NA_character_,
    phone_number = phone_number,
    email = NA_character_,
    property_image = property_image,
    latitude = gmaps_latitude,
    longitude = gmaps_longitude,
    gmaps_address = gmaps_formatted_address,
    gmaps_name = gmaps_name,
    gmaps_place_id = gmaps_place_id,
    gmaps_rating = gmaps_rating,
    gmaps_num_of_reviews = gmaps_user_ratings_total,
    gmaps_place_types = gmaps_types,
    gmaps_icon = gmaps_icon,
    gmaps_website = gmaps_website,
    gmaps_url = gmaps_url,
    map_layer = ifelse(is_competitor, "competitors", "properties"),
    map_marker_icon = ifelse(is_competitor, "building", "home"),
    map_marker_icon_color = "white",
    map_marker_color = ifelse(is_competitor, "red", "blue"),
    map_popup_html = paste0(
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
    )
  )


dplyr::glimpse(enriched_property_data)

DBI::dbWriteTable(
  conn,
  DBI::SQL("mkt.locations"),
  value = enriched_property_data,
  append = TRUE
)

# create map

db_data <- db_get_mkt_map_locations(conn = conn, property_id = "739085")





###########################


# enriched_data |>
#   dplyr::rowwise() |>
#   dplyr::mutate(
#     parsed = list(parse_address(address))
#   ) |>
#   dplyr::ungroup() |>
#   # Unnest parsed address components
#   dplyr::mutate(
#     address_line_1 = parsed$address_line_1,
#     city = parsed$city,
#     state_province = parsed$state_province,
#     postal_code = parsed$postal_code,
#     country = parsed$country
#   ) |>
#   # Define defaults for map display fields; adapt as needed
#   dplyr::mutate(
#     image_url = NA_character_,
#     map_marker_icon = ifelse(is_competitor, "map-marker", "home"),
#     map_marker_color = "blue",
#     map_marker_icons_color = "white",
#     map_popup_html = paste0(
#       "<div style='min-width: 200px;'>",
#       "<h4>", property_name, "</h4>",
#       "<p><strong>Property Details:</strong><br>",
#       "Type: ", property_type, " (", product_type, ")<br>",
#       "Status: ", property_status, "<br>",
#       "Year Built: ", year_built, "<br>",
#       "Rating: ", property_rating, " stars<br>",
#       "Distance from Campus: ", distance_from_campus, " miles</p>",
#
#       "<p><strong>Contact:</strong><br>",
#       "Phone: ", phone_number, "<br>",
#       "<a href='", website, "' target='_blank'>Visit Website</a></p>",
#
#       "<p><strong>Management:</strong><br>",
#       "Developer: ", developer, "<br>",
#       "Manager: ", manager, "<br>",
#       "Owner: ", owner, "</p>",
#
#       ifelse(property_image != "",
#              paste0("<img src='", property_image,
#                     "' style='max-width: 200px; margin-top: 10px;'>"),
#              ""),
#       "</div>"
#     ),
#     map_layer = dplyr::if_else(is_competitor, "competitors", "properties"),
#     region = NA_character_,
#     property_rating = gmaps_rating,
#     num_reviews = gmaps_user_ratings_total
#   ) |>
#   # Select and rename columns to match mkt.property_locations schema
#   # Adjust based on your actual schema fields if necessary
#   dplyr::select(
#     property_id,
#     entrata_property_id,
#     property_name,
#     is_competitor,
#     website,
#     address,
#     phone_number,
#     gmaps_phone_number,
#     property_type,
#     property_status,
#     product_type,
#     street,
#     city,
#     state,
#     postal_code,
#     country,
#     latitude = gmaps_latitude,
#     longitude = gmaps_longitude,
#     gmaps_name,
#     gmaps_address = gmaps_formatted_address,
#     gmaps_place_id,
#     gmaps_url,
#     gmaps_rating = property_rating,
#     gmaps_num_reviews = num_reviews,
#     image_url,
#     map_marker_icon,
#     gmaps_icon,
#     map_marker_color,
#     map_marker_icons_color,
#     map_popup_html,
#     map_layer,
#     distance_from_campus,
#
#   )
#
# map_data_enriched <- enriched_property_data |>
#   dplyr::mutate(
#     map_popup_html = paste0(
#       "<div style='min-width: 200px;'>",
#       "<h4>", property_name, "</h4>",
#       "<p><strong>Property Details:</strong><br>",
#       "Type: ", property_type, " (", product_type, ")<br>",
#       "Status: ", property_status, "<br>",
#       "Year Built: ", year_built, "<br>",
#       "Rating: ", property_rating, " stars<br>",
#       "Distance from Campus: ", distance_from_campus, " miles</p>",
#
#       "<p><strong>Contact:</strong><br>",
#       "Phone: ", phone_number, "<br>",
#       "<a href='", website, "' target='_blank'>Visit Website</a></p>",
#
#       "<p><strong>Management:</strong><br>",
#       "Developer: ", developer, "<br>",
#       "Manager: ", manager, "<br>",
#       "Owner: ", owner, "</p>",
#
#       ifelse(property_image != "",
#              paste0("<img src='", property_image,
#                     "' style='max-width: 200px; margin-top: 10px;'>"),
#              ""),
#       "</div>"
#     )
#   )
#
#
#   dplyr::select(
#     property_id,
#     property_name,
#     address,
#     address_type = type,
#     location_type,
#     lat,
#     lng,
#     coordinates,
#     gmaps_place_id,
#     is_competitor,
#     website,
#     phone_number,
#     property_image,
#     developer:distance_from_campus
#
#   )
#
# map_data <- dplyr::left_join(
#   property_data,
#   google_maps_data,
#   by = c("address" = "original_address")
# ) |>
#   dplyr::select(
#     property_id,
#     property_name,
#     address,
#     address_type = type,
#     location_type,
#     lat,
#     lng,
#     coordinates,
#     gmaps_place_id,
#     is_competitor,
#     website,
#     phone_number,
#     property_image,
#     developer:distance_from_campus
#   ) |>
#   dplyr::mutate(
#     map_marker_icon = "home",
#     map_marker_library = "fa",
#     map_marker_color = ifelse(is_competitor, "red", "blue")
#   )
#
# # fix commonwealth place id
# map_data$gmaps_place_id[map_data$property_name == "1047 Commonwealth Avenue"] <- "ChIJcYT25PZ544kRK42i2BUEzZ8"


















icons <- leaflet::awesomeIcons(
  icon = 'home',
  iconColor = 'white',
  library = 'fa',
  markerColor = ifelse(map_data$is_competitor, 'red', 'blue')
)

leaflet::leaflet(map_data) |>
  leaflet::addTiles() |>
  leaflet::addAwesomeMarkers(
    ~lon, ~lat,
    icon = icons,
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
  ) |>
  leaflet::setView(
    lng = mean(map_data$lon),
    lat = mean(map_data$lat),
    zoom = 15
  )

properties <- map_data

ui <- bslib::page_fluid(
  title = "Property Map",
  theme = bslib::bs_theme(
    version = 5
  ),
  bslib::card(
    bslib::card_header("Property Map"),
    bslib::card_body(
      leaflet::leafletOutput("property_map", height = "1000px")
    )
  )
)

server <- function(input, output, session) {

  output$property_map <- leaflet::renderLeaflet({

    icons <- lapply(seq_len(nrow(map_data)), function(i) {
      if (map_data$is_competitor[i]) {
        makeAwesomeIcon(
          icon = "info-sign",
          markerColor = "red",
          library = "glyphicon",
          iconColor = "white"
        )
      } else {
        makeAwesomeIcon(
          icon = "home",
          markerColor = "blue",
          library = "glyphicon",
          iconColor = "white"
        )
      }
    })

    leaflet::leaflet(map_data) |>
      leaflet::addProviderTiles(providers$CartoDB.Positron) |>
      # leaflet::addTiles() |>
      leaflet::addAwesomeMarkers(
        ~lon, ~lat,
        icon = icons,
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
      ) |>
      leaflet::setView(
        lng = mean(map_data$lon),
        lat = mean(map_data$lat),
        zoom = 15
      )
  })

}

server <- function(input, output, session) {
  output$property_map <- renderLeaflet({
    # Create icons based on competitor status
    icons <- lapply(seq_len(nrow(properties)), function(i) {
      if (properties$is_competitor[i]) {
        makeAwesomeIcon(
          icon = "info-sign",
          markerColor = "red",
          library = "glyphicon",
          iconColor = "white"
        )
      } else {
        makeAwesomeIcon(
          icon = "home",
          markerColor = "blue",
          library = "glyphicon",
          iconColor = "white"
        )
      }
    })

    leaflet(properties) %>%
      addTiles() %>%
      addAwesomeMarkers(
        ~lon, ~lat,
        icon = icons,
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

server <- function(input, output, session) {
  output$property_map <- renderLeaflet({
    # Create a color palette based on competitor status
    pal <- colorFactor(c("blue", "red"), domain = c(FALSE, TRUE))

    # Create icons based on competitor status
    icons <- lapply(seq_len(nrow(properties)), function(i) {
      makeAwesomeIcon(
        icon = ifelse(properties$is_competitor[i], "info-sign", "home"),
        markerColor = pal(properties$is_competitor[i]),
        library = "glyphicon",
        iconColor = "white"
      )
    })

    leaflet(properties) |>
      clearShapes() |>
      addTiles() |>
      addMarkers(
        color = ~pal(is_competitor),
        lng = ~lon,
        lat = ~lat,
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
      )
  })
}


shiny::shinyApp(ui = ui, server = server)


pool::poolReturn(conn)
