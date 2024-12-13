leases_params_tbl <- entrata_params_tbl |>
  dplyr::filter(
    .data$endpoint == "leases",
    .data$method == "getLeases"
  ) |>
  dplyr::select(
    "parameter",
    "type",
    "required",
    "multiple",
    "description"
  ) |>
  dplyr::mutate(
    default = dplyr::case_when(
      .data$parameter == "propertyId" ~ integer(),
      .data$parameter == "applicationId" ~ integer(),
      .data$parameter == "buildingName" ~ character(),
      .data$parameter == "customerId" ~ integer(),

    )
  )
