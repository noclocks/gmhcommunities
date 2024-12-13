
source("data-raw/R/entrata.R")
source("data-raw/src/entrata/properties.R")

entrata_config <- config::get("entrata")

property_leases <- purrr::map(properties$property_id, mem_entrata_leases) |>
  setNames(properties$property_name)

dplyr::glimpse(property_leases[[1]])

dplyr::select_if(property_leases[[1]], is.data.frame) |> names()


parse_property_leases <- function(df) {

  df |>
    tidyr::unnest(
      cols = c(
        "customers",
        "unitSpaces",
        "leaseIntervals",
        "scheduledCharges",
        "pets"
      ),
      names_repair = "minimal",
      names_sep = "_"
    )

}


