parse_apispec <- function(spec) {
  tibblify::parse_openapi_spec(spec)
}

read_apispec <- function(path) {
  switch(
    fs::path_ext(path),
    "yml" = yaml::read_yaml(path),
    "yaml" = yaml::read_yaml(path),
    "json" = jsonlite::read_json(path)
  )
}

# apispec <- read_apispec(apispec_file)

curate_openapi_components <- function(spec_file) {

  spec <- yaml::read_yaml(spec_file)

  sys_prompt <- paste0(
    "As an expert in OpenAPI specifications, review the provided OpenAPI ",
    "specification containing various paths and operations. Your task is to: ",
    "1. Extract necessary component schemas, headers, requestBodies, responses, ",
    "examples, securitySchemes, etc., from the provided paths and operations. ",
    "2. Create these components in the `components` section of the specification. ",
    "3. Update the paths and operations to use `$ref` references to these newly ",
    "created components. After completing your review and updates, provide the ",
    "revised OpenAPI specification."
  )

  spec_yml <- yaml::as.yaml(spec)

  prompt <- paste0(
    "Here is the openapi specification to review:\n\n",
    "```yaml\n",
    spec_yml,
    "\n```"
  )

  chat <- elmer::chat_openai(
    system_prompt = sys_prompt,
    model = "gpt-4o",
    echo = "none"
  )

  resp <- chat$chat(prompt)

  cat(resp, sep = "\n")

}
