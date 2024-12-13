library(targets)
library(tarchetypes)

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
targets::tar_source("R/pipeline.R")

# Replace the target list below with your own:
list(
  targets::tar_target(
    name = initial_properties_data,
    command = entrata_properties(),
    format = "qs"
  ),
  targets::tar_target(

  )
)
