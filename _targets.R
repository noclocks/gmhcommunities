library(targets)
library(tarchetypes)

pkgload::load_all()

targets::tar_option_set(
  packages = c(
    "dplyr",
    "purrr",
    "tibble",
    "httr2",
    "jsonlite",
    "tibblify"
  ),
  format = "qs"
)


# Run the R scripts in the R/ folder with your custom functions:
targets::tar_source()

# Replace the target list below with your own:
list(
  targets::tar_target(
    name = status,
    command = entrata_status()
  ),
  targets::tar_target(
    name = properties_lst,
    command = entrata_properties()
  ),
  targets::tar_target(
    name = charge_codes,
    command = entrata_arcodes()
  ),
  targets::tar_target(
    name = properties,
    command = purrr::pluck(properties_lst, "property_tbl_base")
  ),
  targets::tar_target(
    name = property_addresses,
    command = purrr::pluck(properties_lst, "property_addresses")
  ),
  targets::tar_target(
    name = property_hours,
    command = purrr::pluck(properties_lst, "property_hours")
  ),
  targets::tar_target(
    name = property_space_options,
    command = purrr::pluck(properties_lst, "property_space_options")
  ),
  targets::tar_target(
    name = floorplans_by_property,
    command = purrr::map_dfr(properties$property_id, entrata_properties_getFloorPlans)
  )
)
