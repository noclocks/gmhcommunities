openai_chat <- function(prompt, model = "gpt-4o") {

  elmer::chat_openai(
    model = model,
    system_prompt = prompt
  )

}

chat <- openai_chat(prompt = "You are an expert with R Shiny and Web Design.")

ai_build_form <- function(requirements, ...) {

  prompt <- paste0(
    "Build an R Shiny input form using bslib and shinyWidgets for the following specifications/requirements:\n\n",
    paste0(
      "- ", requirements, collapse = "\n- "
    )
  )

  chat$chat(
    prompt
  )

}

ai_build_form(
  requirements = c(
    "The form is for a leasing market survey",
    "The form should have a text input widget for the property name",
    "The form should have a date input widget for the survey date",
    "The form should have a numeric input widget for the number of units"
  )
)
