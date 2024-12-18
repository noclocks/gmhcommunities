address_input_autocomplete <- function(
    id,
    value = "",
    label = "Address:",
    icon = bsicons::bs_icon("search"),
    placeholder = "Search for an address...",
    width = "100%",
    api_key = get_gmaps_config("api_key"),
    ...
) {

  shiny::addResourcePath(
    prefix = "www",
    directoryPath = system.file("www", package = "gmhcommunities")
  )

  input_label <- icon_text("search", label)

  htmltools::tagList(
    htmltools::tags$head(
      htmltools::tags$script(
        src = paste0(
          "https://maps.googleapis.com/maps/api/js?key=",
          api_key,
          "&libraries=places&callback=initAutocomplete"
        ),
        async = NA,
        defer = NA
      ),
      htmltools::tags$script(
        src = "www/scripts/js/address_autocomplete_input_binding.js"
      )
    ),
    shinyWidgets::textInputIcon(
      inputId = id,
      label = input_label,
      value = value,
      icon = icon,
      placeholder = placeholder,
      width = width,
      ...
    ) |>
      htmltools::tagAppendAttributes(
        class = "address-autocomplete",
      )
  )
}


gmaps_address_input_ui <- function(
    id,
    value = "",
    label = "Address:",
    icon = bsicons::bs_icon("search"),
    placeholder = "Search for an address...",
    width = "100%",
    gmaps_api_key = get_gmaps_config("api_key"),
    ...
) {

  ns <- shiny::NS(id)

  button <- ns("button")
  jsValue <- ns("jsValue")
  jsValueAddressNumber <- ns("jsValueAddressNumber")
  jsValuePretty <- ns("jsValuePretty")
  jsValueCoords <- ns("jsValueCoords")

  js_script <- stringr::str_c("
    <script>
        function initAutocomplete() {

        var autocomplete =   new google.maps.places.Autocomplete(document.getElementById('${button}'),{types: ['geocode']});
        autocomplete.setFields(['address_components', 'formatted_address',  'geometry', 'icon', 'name']);
        autocomplete.addListener('place_changed', function() {
        var place = autocomplete.getPlace();
        if (!place.geometry) {
            return;
        }

        var addressPretty = place.formatted_address;
        var address = '';
        if (place.address_components) {
        address = [
        (place.address_components[0] && place.address_components[0].short_name || ''),
        (place.address_components[1] && place.address_components[1].short_name || ''),
        (place.address_components[2] && place.address_components[2].short_name || ''),
        (place.address_components[3] && place.address_components[3].short_name || ''),
        (place.address_components[4] && place.address_components[4].short_name || ''),
        (place.address_components[5] && place.address_components[5].short_name || ''),
        (place.address_components[6] && place.address_components[6].short_name || ''),
        (place.address_components[7] && place.address_components[7].short_name || '')
        ].join(' ');
        }
        var address_number =''
        address_number = [(place.address_components[0] && place.address_components[0].short_name || '')]
        var coords = place.geometry.location;
        //console.log(address);
        Shiny.onInputChange('${jsValue}', address);
        Shiny.onInputChange('${jsValueAddressNumber}', address_number);
        Shiny.onInputChange('${jsValuePretty}', addressPretty);
        Shiny.onInputChange('${jsValueCoords}', coords);});}
    </script>
    <script src='https://maps.googleapis.com/maps/api/js?key=${gmaps_api_key}&libraries=places&callback=initAutocomplete' async defer></script>"
  ) |>
    stringr::str_interp()

  js_script <- stringr::str_replace_all(js_script, "\\s+", "")

  htmltools::tagList(
    htmltools::tags$div(
      id = stringr::str_c(button, "-layout"),
      shiny::textInput(
        button,
        label = label,
        value = value,
        placeholder = placeholder,
        width = width
      ),
      htmltools::HTML(js_script),
      ...
    )
  )
}

gmaps_address_input_server <- function(id, gmaps_api_key = get_gmaps_config("api_key")) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      address <- shiny::reactive({
        if (!is.null(input$jsValueAddressNumber)) {
          if (length(grep(
            pattern = input$jsValueAddressNumber,
            x = input$jsValuePretty
          )) == 0) {
            final_address <- c(
              input$jsValueAddressNumber,
              input$jsValuePretty
            )
          } else {
            final_address <- input$jsValuePretty
          }
          return(final_address)
        }
      })

      address <- reactive({
        shiny::req(address())
        result <- googleway::google_geocode(
          address = address(),
          key = key
        )
        return(result)
      })

      address
    }
  )
}

library(shiny)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  gmaps_address_input_ui(
    "test",
    width = "100%"
  ),
  leaflet::leafletOutput(
    "map",
    width = "auto",
    height = "auto"
  )
)

server <- function(input, output, session) {
  address <- gmaps_address_input_server("test")

  output$map <- leaflet::renderLeaflet({
    shiny::req(address())

    shinyjs::runjs('$("#map").width(500).height(500);')

    results <- address()[["results"]]

    lng <- results[["geometry"]][["location"]][["lng"]]
    lat <- results[["geometry"]][["location"]][["lat"]]

    leaflet::leaflet() |>
      leaflet::addTiles() |>
      leaflet::setView(
        lng = lng,
        lat = lat,
        zoom = 13
      ) |>
      leaflet::addMarkers(
        lng = lng,
        lat = lat,
        popup = "Well done, noble warrior!"
      )

  })

}

shinyApp(ui, server)
