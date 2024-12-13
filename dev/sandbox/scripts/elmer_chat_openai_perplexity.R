library(elmer)

chat <- chat_openai(
  model = "gpt-4o-mini",
  system_prompt = "You are an expert software engineer with experience using R, R Shiny, R Plumber, API OpenAPI Specification, Docker, Google Cloud Services, Web Design, Web Development, JavaScript, Typescript, etc.",
  echo = TRUE
)

live_console(chat)

chat$chat("What is the best way to deploy a R Plumber API on Google Cloud Services?")

chat_perplex <- chat_perplexity(
  system_prompt = "You are an expert software engineer with experience using R, R Shiny, R Plumber, API OpenAPI Specification, Docker, Google Cloud Services, Web Design, Web Development, JavaScript, Typescript, etc.",
  api_key =
)
