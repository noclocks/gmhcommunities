
#  ------------------------------------------------------------------------
#
# Title : Entrata Data Preparation
#    By : Jimmy Briggs
#  Date : 2024-11-24
#
#  ------------------------------------------------------------------------

source("data-raw/src/entrata/endpoints.R")
source("data-raw/src/entrata/methods.R")
source("data-raw/src/entrata/versions.R")
source("data-raw/src/entrata/params.R")
source("data-raw/src/entrata/defaults.R")

# tibble ------------------------------------------------------------------

entrata_params_tbl <- entrata_method_params |>
  tibble::enframe(name = "endpoint", value = "methods") |>
  tidyr::unnest_longer(methods) |>
  dplyr::mutate(
    method = names(methods)
  ) |>
  tidyr::unnest_longer(methods, values_to = "parameters") |>
  dplyr::mutate(
    parameter = names(parameters)
  ) |>
  tidyr::unnest_wider(parameters, names_sep = "_") |>
  dplyr::mutate(
    multiple = dplyr::coalesce(parameters_multiple, FALSE)
  ) |>
  dplyr::select(
    endpoint,
    method,
    parameter,
    type = parameters_type,
    required = parameters_required,
    multiple,
    description = parameters_description
  ) |>
  dplyr::arrange(
    endpoint,
    method,
    dplyr::desc(required),
    parameter
  )


# cleanup -----------------------------------------------------------------

rm(r2_methods, r3_methods, assign_versions)


# inform ------------------------------------------------------------------

cli::cli_alert_success(
  c(
    "Entrata data preparation completed successfully and generated the following objects:\n",
    "{.field {ls()}}"
  )
)

