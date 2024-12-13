entrata_config <- config::get("entrata")

# base request
entrata_req <- httr2::request(entrata_config$base_url) |>
  httr2::req_method("POST") |>
  httr2::req_auth_basic(
    entrata_config$username,
    entrata_config$password
  ) |>
  httr2::req_headers(
    `Content-Type` = "application/json; charset=utf-8",
    `Accept` = "application/json"
  )

prop_ids <- mem_get_entrata_property_ids()

# function to derive request body
# TODO

prop_id <- prop_ids[names(prop_ids) == "1047 Commonwealth Avenue"][[1]]

entrata_floor_plans_req <- entrata_req |>
  httr2::req_url_path_append("properties") |>
  httr2::req_body_json(
    list(
      auth = list(
        type = "basic"
      ),
      requestId = 999,
      method = list(
        name = "getFloorPlans",
        version = "r1",
        params = list(
          "propertyId" = prop_id, #"641240",
          "usePropertyPreferences" = "0",
          "includeDisabledFloorplans" = "1"
        )
      )
    )
  )

httptest2::capture_requests({
  entrata_floor_plans_resp <- entrata_floor_plans_req |>
    httr2::req_perform()
})

resp_json <- entrata_floor_plans_resp |>
  httr2::resp_body_json()

resp_floor_plans <- purrr::pluck(resp_json, "response", "result", "FloorPlans", "FloorPlan")

property_ids <- purrr::map_int(resp_floor_plans, ~ purrr::pluck(.x, "PropertyId"))
floor_plan_ids <- purrr::map_int(resp_floor_plans, ~ purrr::pluck(.x, "Identification", "IDValue"))
floor_plan_names <- purrr::map_chr(resp_floor_plans, ~ purrr::pluck(.x, "Name"))
floor_plan_total_unit_counts <- purrr::map_int(resp_floor_plans, ~ purrr::pluck(.x, "UnitCount") |> as.integer())
floor_plan_available_unit_counts <- purrr::map_int(resp_floor_plans, ~ purrr::pluck(.x, "UnitsAvailable") |> as.integer())
floor_plan_unit_type_ids <- purrr::map_int(
  resp_floor_plans,
  ~ purrr::map_int(
    purrr::pluck(.x, "UnitTypes", "UnitType"),
    ~ purrr::pluck(.x, "@attributes", "Id") |> as.integer()
  )
)
floor_plan_unit_type_names <- purrr::map_chr(
  resp_floor_plans,
  ~ purrr::map_chr(
    purrr::pluck(.x, "UnitTypes", "UnitType"),
    ~ purrr::pluck(.x, "@value")
  )
)
floor_plan_is_disableds <- purrr::map_lgl(resp_floor_plans, ~ purrr::pluck(.x, "IsDisabled"))



floor_plan_number_of_bedrooms <- purrr::map_int(
  resp_floor_plans,
  # need to get the value of count for the bedroom room type which will
  # have nested @attribute.RoomType of "Bedroom"
  ~ purrr::pluck(
    purrr::pluck(.x, "Room"),
    ~ purrr::pluck(.x, "@attributes", "RoomType") == "Bedroom",
    "Count"
  ) |> as.integer()
)

floor_plans_tbl <- purrr::map_dfr(
  resp_floor_plans,
  function(x) {
    tibble::tibble(
      floorplan_id = purrr::pluck(x, "Identification", "IDValue"),
      floorplan_name = purrr::pluck(x, "Name"),
      property_id = purrr::pluck(x, "PropertyId"),
      available_units_count = purrr::pluck(x, "UnitsAvailable") |> as.integer(),
      is_disabled = purrr::pluck(x, "IsDisabled")
    )
  }
)

unit_types_tbl <- purrr::map_dfr(
  resp_floor_plans,
  ~ {
    purrr::map_dfr(
      purrr::pluck(.x, "UnitTypes", "UnitType"),
      ~ {
        tibble::tibble(
          floorplan_id = purrr::pluck(.x, "Identification", "IDValue"),
          unit_type_id = purrr::pluck(.x, "@attributes", "Id") |> as.integer(),
          unit_type_name = purrr::pluck(.x, "@value")
        )
      }
    )
  }
)

rooms_tbl <- purrr::map_dfr(
  resp_floor_plans,
  ~ {
    purrr::map_dfr(
      purrr::pluck(.x, "Room"),
      ~ {
        tibble::tibble(
          floorplan_id = purrr::pluck(.x, "Identification", "IDValue"),
          room_type = purrr::pluck(.x, "@attributes", "RoomType"),
          room_count = purrr::pluck(.x, "Count") |> as.integer()
        )
      }
    )
  }
)

square_feet_tbl <- purrr::map_dfr(
  resp_floor_plans,
  ~ {
    tibble::tibble(
      floorplan_id = purrr::pluck(.x, "Identification", "IDValue"),
      min_square_feet = purrr::pluck(.x, "SquareFeet", "@attributes", "Min") |> as.integer(),
      max_square_feet = purrr::pluck(.x, "SquareFeet", "@attributes", "Max") |> as.integer()
    )
  }
)


market_rent_tbl <- purrr::map_dfr(
  resp_floor_plans,
  ~ {
    tibble::tibble(
      floorplan_id = purrr::pluck(.x, "Identification", "IDValue"),
      min_market_rent = purrr::pluck(.x, "MarketRent", "@attributes", "Min") |> as.integer(),
      max_market_rent = purrr::pluck(.x, "MarketRent", "@attributes", "Max") |> as.integer()
    )
  }
)

files_tbl <- purrr::map_dfr(
  resp_floor_plans,
  ~ {
    purrr::map_dfr(
      purrr::pluck(.x, "File"),
      ~ {
        tibble::tibble(
          floorplan_id = purrr::pluck(.x, "Identification", "IDValue"),
          file_type = purrr::pluck(.x, "FileType"),
          file_id = purrr::pluck(.x, "@attributes", "FileID") |> as.integer(),
          description = purrr::pluck(.x, "Description"),
          name = purrr::pluck(.x, "Name"),
          caption = purrr::pluck(.x, "Caption"),
          format = purrr::pluck(.x, "Format"),
          width = purrr::pluck(.x, "Width") |> as.integer(),
          height = purrr::pluck(.x, "Height") |> as.integer(),
          source = purrr::pluck(.x, "Src"),
          is_default = purrr::pluck(.x, "IsDefault")
        )
      }
    )
  }
)


entrata_floor_plans_resp_body_result <- entrata_floor_plans_resp_body |>
  purrr::pluck("response", "result", "FloorPlans", "FloorPlan")

floorplans <- entrata_floor_plans_resp_body_result








floorplan_tibble <- tibble(
  FloorplanID = map_int(floorplans, ~ .x$Identification$IDValue),
  Name = map_chr(floorplans, ~ .x$Name),
  PropertyID = map_int(floorplans, ~ .x$PropertyId),
  Comment = map_chr(floorplans, ~ .x$Comment),
  UnitCount = map_int(floorplans, ~ as.integer(.x$UnitCount)),
  UnitsAvailable = map_int(floorplans, ~ as.integer(.x$UnitsAvailable)),
  IsDisabled = map_lgl(floorplans, ~ .x$IsDisabled),
  MinSquareFeet = map_dbl(floorplans, ~ .x$SquareFeet$`@attributes`$Min),
  MaxSquareFeet = map_dbl(floorplans, ~ .x$SquareFeet$`@attributes`$Max),
  MinMarketRent = map_dbl(floorplans, ~ .x$MarketRent$`@attributes`$Min),
  MaxMarketRent = map_dbl(floorplans, ~ .x$MarketRent$`@attributes`$Max)
)

rooms_tibble <- map_dfr(floorplans, function(fp) {
  tibble(
    FloorplanID = fp$Identification$IDValue,
    RoomType = map_chr(fp$Room, ~ .x$`@attributes`$RoomType),
    Count = map_dbl(fp$Room, ~ .x$Count)
  )
})

files_tibble <- map_dfr(floorplans, function(fp) {
  tibble(
    FloorplanID = fp$Identification$IDValue,
    FileID = map_int(fp$File, ~ .x$`@attributes`$FileID),
    Description = map_chr(fp$File, ~ .x$Description),
    Name = map_chr(fp$File, ~ .x$Name),
    Src = map_chr(fp$File, ~ .x$Src),
    Format = map_chr(fp$File, ~ .x$Format),
    Width = map_int(fp$File, ~ as.integer(.x$Width)),
    Height = map_int(fp$File, ~ as.integer(.x$Height)),
    IsDefault = map_lgl(fp$File, ~ .x$IsDefault)
  )
})

# Extract FloorplanGroup details into a separate tibble
floorplan_groups_tibble <- map_dfr(floorplans, function(fp) {
  groups <- fp$FloorplanGroups$FloorplanGroup
  if (!is.null(groups)) {
    tibble(
      FloorplanID = fp$Identification$IDValue,
      GroupID = map_int(groups, ~ .x$`@attributes`$id),
      GroupName = map_chr(groups, ~ .x$`@attributes`$name)
    )
  } else {
    NULL
  }
})

# Extract UnitType details into a separate tibble
unit_types_tibble <- map_dfr(floorplans, function(fp) {
  unit_types <- fp$UnitTypes$UnitType
  tibble(
    FloorplanID = fp$Identification$IDValue,
    UnitTypeID = map_int(unit_types, ~ .x$`@attributes`$Id),
    UnitTypeName = map_chr(unit_types, ~ .x$`@value`)
  )
})

print("Main Floorplan Tibble:")
print(floorplan_tibble)

print("Rooms Tibble:")
print(rooms_tibble)

print("Files Tibble:")
print(files_tibble)

print("Floorplan Groups Tibble:")
print(floorplan_groups_tibble)

print("Unit Types Tibble:")
print(unit_types_tibble)

entrata_floorplans <- function(property_id) {

  # validate property id
  check_property_id(property_id)

  # build method parameters
  method_params <- list(
    propertyId = property_id
  ) |>
    purrr::compact()

  # build request -----------------------------------------------------------
  req <- entrata_request() |>
    entrata_req_endpoint("floorplans") |>
    entrata_req_method("getFloorplans") |>
    entrata_req_method_params(method_params) |>
    entrata_req_id(request_id)

  # perform request into response -------------------------------------------
  resp <- entrata_req_perform(req, ...)

  # parse response ----------------------------------------------------------
  entrata_resp_parse_floorplans(resp)

}

parse_floorplan_to_tibble <- function(x) {
  tibble::tibble(
    floorplan_id = x$FloorplanID,
    floorplan_name = x$Name,
    floorplan_bedrooms = x$Bedrooms,
    floorplan_bathrooms = x$Bathrooms,
    floorplan_square_feet = x$SquareFeet
  )
}
