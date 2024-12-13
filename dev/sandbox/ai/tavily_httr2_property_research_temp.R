library(elmer)
library(rvest)

# Sys.getenv("OPENAI_API_KEY")

google_api_key <- ""
google_search_engine_id <- ""

tavily_api_key <- "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"
# "api_key": "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",

prompt_qry_template <- "Retrieve property leasing and competitor info for '{{qry}}'.
Include location, specs, year built, available units (types, sizes, prices), amenities (e.g., gym, pool, parking), lease terms (e.g., duration, specials), nearby competitors (names, addresses, prices), neighborhood info (attractions, transit), and owner/manager details."

qry <- "1330 Boylston"

agent_role <- "
You are an expert real estate analyst AI assistant. Your main task is to gather detailed information on property leasing, including location, specifications, available units, amenities, lease terms, nearby competitors, neighborhood attractions, transit options, and owner/manager details.
"

prompt <- "Retrieve property leasing and competitor info for '1330 Boylston.' Include location, specs, year built, available units (types, sizes, prices), amenities (e.g., gym, pool, parking), lease terms (e.g., duration, specials), nearby competitors (names, addresses, prices), neighborhood info (attractions, transit), and owner/manager details."


ai_property_search_report(qry, api_key) {

  req <- httr2::request("https://api.tavily.com/search") |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      "Content-Type" = "application/json"
    ) |>
    httr2::req_user_agent("Mozilla/5.0") |>
    httr2::req_body_json(
      list(
        api_key = tavily_api_key,
        include_answer = FALSE,
        include_images = TRUE,
        include_raw_content = FALSE,
        max_results = 5,
        query = glue::glue(
          "Retrieve property leasing and competitor info for '{{qry}}'. Include location, specs, year built, available units (types, sizes, prices), amenities (e.g., gym, pool, parking), lease terms (e.g., duration, specials), nearby competitors (names, addresses, prices), neighborhood info (attractions, transit), and owner/manager details."
        ),
        search_depth = "advanced"
      )
    )

  resp <- httr2::req_perform(req)

  resp_content <- httr2::resp_body_json(resp)

  context <- resp_content$results

  req_agent <- httr2::request(
    "https://cehewlkelxpx6ut2x2crpewo3y0gmemy.lambda-url.us-east-1.on.aws/researchAPI"
  ) |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      "Content-Type" = "application/json"
    ) |>

}

ai_search_property_info <- function(qry, api_key) {

  req_website <- httr2::request("https://api.tavily.com/search") |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      "Content-Type" = "application/json"
    ) |>
    httr2::req_user_agent("Mozilla/5.0") |>
    httr2::req_body_json(
      list(
        api_key = tavily_api_key,
        query = paste0("what is the official website URL for: ", qry, "?"),
        include_answer = TRUE,
        search_depth = "basic", # "advanced",
        max_results = 5,
        topic = "general"
      )
    )

  resp <- httr2::req_perform(req_website)

  result <- httr2::resp_body_json(resp)
  answer <- result$answer
  results <- result$results

  urls <- results |>
    purrr::map_chr(~purrr::pluck(.x, "url"))

  url_titles <- results |>
    purrr::map_chr(~purrr::pluck(.x, "title"))

  url_content <- results |>
    purrr::map_chr(~purrr::pluck(.x, "content"))

  website <- urls[[1]]
  website_title <- url_titles[[1]]
  website_description <- url_content[[1]]

  cli::cli_bullets(
    c(
      "Found the following information from initial query: {.field {qry}}\n",
      "Website: {.url {website}}\n",
      "Title: {.field {website_title}}\n",
      "Description: {.field {website_description}}"
    )
  )

  cli::cli_bullets(
    c(
      "Additionally, found these other URLs:\n",
      "{.field {purrr::map_chr(urls, ~.x)}}"
    )
  )

}

ai_search_for_property_website <- function(query) {

  search_url <- glue::glue(
    "https://www.google.com/search?q={URLencode(query)}+property+website"
  )

  resp <- httr2::request(search_url) |>
    httr2::req_method("GET") |>
    httr2::req_user_agent("Mozilla/5.0") |>
    httr2::req_perform()

  resp_body <- httr2::resp_body_html(resp)

  first_result <- resp_body$doc |>
    rvest::html_nodes("div.yuRUbf > a") |>
    rvest::html_attr("href") |>
    purrr::pluck(1)

  resp_content <- rvest::read_html(resp$body)

}

ai_get_property_website <- function(property_name, property_address, ...) {

  elmer::chat_

}
