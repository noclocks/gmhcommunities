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
