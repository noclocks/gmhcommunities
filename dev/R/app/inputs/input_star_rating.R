star_rating_input <- function(id, label, value = 0, stars = 5) {
  dep <- htmltools::htmlDependency(
    name = "star-rating",
    version = "1.0.0",
    src = pkg_sys("www/scripts/js/star-rating"),
    script = "star-rating-binding.js",
    stylesheet = "star-rating.css"
  )

  div_tag <- tags$div(
    class = "star-rating-input",
    id = id,
    `data-stars` = stars,
    `data-value` = value,
    tags$label(label),
    lapply(1:stars, function(i) {
      tags$i(class = "star-icon", `data-value` = i)
    })
  )

  htmltools::attachDependencies(div_tag, dep)
}


address_input <- function(id, label, value = "", api_key = get_gmaps_config("api_key")) {
  dep <- htmltools::htmlDependency(
    name = "google-maps-autocomplete",
    version = "1.0.0",
    src = pkg_sys("www/scripts/js/gmaps-autocomplete"),
    script = c(
      sprintf("https://maps.googleapis.com/maps/api/js?key=%s&libraries=places", api_key),
      "address-binding.js"
    )
  )

  div_tag <- tags$div(
    class = "address-input-container",
    tags$label(label),
    tags$input(
      id = id,
      class = "address-input",
      type = "text",
      value = value
    )
  )

  htmltools::attachDependencies(div_tag, dep)
}

phone_input <- function(id, label, value = "") {
  dep <- htmltools::htmlDependency(
    name = "phone-input",
    version = "1.0.0",
    src = pkg_sys("www/scripts/js/phone-input"),
    script = "phone-binding.js",
    stylesheet = "phone.css"
  )

  div_tag <- tags$div(
    class = "phone-input-container",
    tags$label(label),
    tags$input(
      id = id,
      class = "phone-input",
      type = "tel",
      pattern = "[0-9]{3}-[0-9]{3}-[0-9]{4}",
      placeholder = "123-456-7890",
      value = value
    )
  )

  htmltools::attachDependencies(div_tag, dep)
}

email_input <- function(id, label, value = "") {
  dep <- htmltools::htmlDependency(
    name = "email-input",
    version = "1.0.0",
    src = pkg_sys("www/scripts/js/email-input"),
    script = "email-binding.js"
  )

  div_tag <- tags$div(
    class = "email-input-container",
    tags$label(label),
    tags$input(
      id = id,
      class = "email-input",
      type = "email",
      value = value
    )
  )

  htmltools::attachDependencies(div_tag, dep)
}

url_input <- function(id, label, value = "") {
  dep <- htmltools::htmlDependency(
    name = "url-input",
    version = "1.0.0",
    src = pkg_sys("www/scripts/js/url-input"),
    script = "url-binding.js"
  )

  div_tag <- tags$div(
    class = "url-input-container",
    tags$label(label),
    tags$input(
      id = id,
      class = "url-input",
      type = "url",
      pattern = "https?://.+",
      placeholder = "https://example.com",
      value = value
    )
  )

  htmltools::attachDependencies(div_tag, dep)
}


custom_input_deps <- function() {
  list(
    htmltools::htmlDependency(
      name = "star-rating",
      version = "1.0.0",
      src = pkg_sys("www/scripts/js/star-rating"),
      script = "star-rating-binding.js",
      stylesheet = "star-rating.css"
    ),
    htmltools::htmlDependency(
      name = "google-maps-autocomplete",
      version = "1.0.0",
      src = pkg_sys("www/scripts/js/gmaps-autocomplete"),
      script = c(
        sprintf("https://maps.googleapis.com/maps/api/js?key=%s&libraries=places", get_gmaps_config("api_key")),
        "address-binding.js"
      )
    ),
    htmltools::htmlDependency(
      name = "phone-input",
      version = "1.0.0",
      src = pkg_sys("www/scripts/js/phone-input"),
      script = "phone-binding.js",
      stylesheet = "phone.css"
    ),
    htmltools::htmlDependency(
      name = "email-input",
      version = "1.0.0",
      src = pkg_sys("www/scripts/js/email-input"),
      script = "email-binding.js"
    ),
    htmltools::htmlDependency(
      name = "url-input",
      version = "1.0.0",
      src = pkg_sys("www/scripts/js/url-input"),
      script = "url-binding.js"
    )
  )
}

use_custom_inputs <- function() {
  shiny::addResourcePath("www", pkg_sys("www"))
  htmltools::htmlDependencies(
    custom_input_deps()
  )
}

# showcase inputs

ui <- fluidPage(
  use_custom_inputs(),
  titlePanel("Custom Inputs"),
  sidebarLayout(
    sidebarPanel(
      star_rating_input("star-rating", "Star Rating", value = 3),
      address_input("address", "Address", value = "123 Main St, Anytown, USA"),
      phone_input("phone", "Phone Number", value = "123-456-7890"),
      email_input("email", "Email Address", value = "john.doe@example.com"),
      url_input("url", "Website URL", value = "https://example.com")
    ),
    mainPanel(
      verbatimTextOutput("star-rating"),
      verbatimTextOutput("address"),
      verbatimTextOutput("phone"),
      verbatimTextOutput("email"),
      verbatimTextOutput("url")
    )
  )
)

server <- function(input, output, session) {
  output$star_rating <- renderPrint(input$star_rating)
  output$address <- renderPrint(input$address)
  output$phone <- renderPrint(input$phone)
  output$email <- renderPrint(input$email)
  output$url <- renderPrint(input$url)
}

shinyApp(ui, server)
