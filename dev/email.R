email_input <- function(inputId, label = "Email") {
  tagList(
    tags$div(
      class = "form-group",
      tags$label(label),
      tags$input(
        id = inputId,
        type = "email",
        class = "form-control email-input",
        pattern = "[a-z0-9._%+-]+@[a-z0-9.-]+\\.[a-z]{2,}$"
      ),
      tags$div(
        class = "invalid-feedback",
        "Please enter a valid email address"
      )
    ),
    tags$script(sprintf(
      "const emailInput = document.querySelector('#%s');
      emailInput.addEventListener('input', function() {
        const isValid = this.checkValidity();
        Shiny.setInputValue('%s', {
          value: this.value,
          isValid: isValid
        });
        this.classList.toggle('is-invalid', !isValid);
      });", inputId, inputId
    ))
  )
}
