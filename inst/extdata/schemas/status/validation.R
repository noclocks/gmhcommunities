require(jsonvalidate)
require(jsonlite)
require(rjsoncons)
require(tidyjson)

json_schema <- "inst/extdata/schemas/status/status.getStatus.request.schema.json"
json_example <- "inst/extdata/examples/status/status.getStatus.request.example.json"

validator <- jsonvalidate::json_validator(
  schema = json_schema,
  engine = "ajv"
)

json <- readr::read_file(json_example)

validator(json, verbose = TRUE)
#> [1] TRUE


status_resp_json <- readr::read_file(json_example)
cat(status_resp_json)

status_resp_json_schema <- readr::read_file(json_schema)
cat(status_resp_json_schema)

rjsoncons::j_schema_is_valid(status_resp_json, status_resp_json_schema)
rjsoncons::j_schema_validate(status_resp_json, status_resp_json_schema, as = "tibble")

status_resp_flat <- rjsoncons::j_flatten(status_resp_json, as = "R")
status_resp_flat

status_resp_json |> rjsoncons::as_r()
