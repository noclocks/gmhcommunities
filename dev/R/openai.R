openai_chat <- function(prompt, model = "gpt-4o-mini") {

  elmer::chat_openai(
    model = model,
    system_prompt = prompt
  )

}


#' Gets the current time in the given time zone.
#'
#' @param tz The time zone to get the current time in.
#' @returns The current time in the given time zone.
get_current_time <- function(tz = "UTC") {
  format(Sys.time(), tz = tz, usetz = TRUE)
}

chat <- openai_chat(prompt = "You are a helpful assistant.")

chat$register_tool(
  tool_def = elmer::tool(
    get_current_time,
    "Gets the current time in the given time zone.",
    tz = elmer::type_string(
      "The time zone to get the current time in. Defaults to `\"UTC\"`.",
      required = FALSE
    )
  )
)

chat$chat("What time is it?")
chat$chat("What time is it in New York?", "America/New_York")
