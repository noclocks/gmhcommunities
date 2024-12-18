phone_number_input <- function(id, label = "Phone", country = "US") {
  tagList(
    tags$link(href = "https://cdnjs.cloudflare.com/ajax/libs/intl-tel-input/17.0.12/css/intlTelInput.css", rel = "stylesheet"),
    tags$div(
      class = "form-group",
      tags$label(label),
      tags$input(
        id = id,
        type = "tel",
        class = "form-control phone-input"
      )
    ),
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/intl-tel-input/17.0.12/js/intlTelInput.min.js"),
    tags$script(sprintf(
      "const phoneInput = document.querySelector('#%s');
      const iti = window.intlTelInput(phoneInput, {
        initialCountry: '%s',
        utilsScript: 'https://cdnjs.cloudflare.com/ajax/libs/intl-tel-input/17.0.12/js/utils.js'
      });
      phoneInput.addEventListener('change', function() {
        Shiny.setInputValue('%s', {
          number: iti.getNumber(),
          isValid: iti.isValidNumber()
        });
      });", id, country, id
    ))
  )
}

ui <- fluidPage(
  phone_number_input("phone")
)

server <- function(input, output, session) {
  observeEvent(input$phone, {
    print(input$phone)
  })
}

shinyApp(ui, server)
