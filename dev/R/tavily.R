
get_tavily_api_key <- function() { "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" }

tavily_search <- function(query, api_key = get_tavily_api_key()) {

  req <- httr2::request("https://api.tavily.com/search") |>
    httr2::req_headers(
      "Content-Type" = "application/json"
    ) |>
    httr2::req_body_json(
      list(
        api_key = api_key,
        query = query
      )
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_json() |>
    purrr::pluck("results")

}

register_tavily_seach_tool <- function(
  agent,
  instructions = "Search the internet for..."
) {

  agent$register_function(
    "tavily_seach",
    tavily_search,
    description = instructions
  )

  return(agent)

}
