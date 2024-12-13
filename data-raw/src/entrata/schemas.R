
#  ------------------------------------------------------------------------
#
# Title : Entrata Schemas
#    By : Jimmy Briggs
#  Date : 2024-11-27
#
#  ------------------------------------------------------------------------

source("data-raw/src/entrata.R")

root_path <- "inst/extdata/schemas"

get_response_schema <- function(endpoint, method) {

  fs::path(root_path, endpoint, paste0(method, ".response.schema.json")) |>
    as.character()

}

entrata_schemas <- entrata_methods_tbl |>
  dplyr::mutate(
    response_schema = purrr::map2(.data$endpoint, .data$method, get_response_schema)
  )
