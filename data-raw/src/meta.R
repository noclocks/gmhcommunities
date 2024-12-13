
#  ------------------------------------------------------------------------
#
# Title : Metadata Preparation
#    By : Jimmy Briggs
#  Date : 2024-11-26
#
#  ------------------------------------------------------------------------

# sources -----------------------------------------------------------------

source("data-raw/src/meta/app_choices.R")
source("data-raw/src/assets.R")

# app_choices -------------------------------------------------------------

app_choices <- list(
  portfolios = portfolio_choices,
  properties = property_choices
)

# app_info ----------------------------------------------------------------

app_logo_path <- asset_registry |>
  dplyr::filter(
    file == "app-logo.svg"
  ) |>
  dplyr::pull(path) |>
  stringr::str_replace_all(
    pattern = "inst/",
    replacement = ""
  ) |>
  purrr::pluck(1)

app_symbol_path <- asset_registry |>
  dplyr::filter(
    file == "app-icon.webp"
  ) |>
  dplyr::pull(path) |>
  stringr::str_replace_all(
    pattern = "inst/",
    replacement = ""
  ) |>
  purrr::pluck(1)

app_info <- list(
  name = "GMH Data Hub",
  version = "1.0",
  logo = app_logo_path,
  symbol = app_symbol_path,
  repo_url = "https://github.com/noclocks/gmhdatahub",
  docs_url = "https://docs.noclocks.dev/gmhdatahub"
)

rm(app_logo_path, app_symbol_path)

# client_info -------------------------------------------------------------

client_logo_path <- asset_registry |>
  dplyr::filter(
    file == "gmh-logo.svg"
  ) |>
  dplyr::pull(path) |>
  stringr::str_replace_all(
    pattern = "inst/",
    replacement = ""
  ) |>
  purrr::pluck(1)

client_symbol_path <- asset_registry |>
  dplyr::filter(
    file == "gmh-icon.png"
  ) |>
  dplyr::pull(path) |>
  stringr::str_replace_all(
    pattern = "inst/",
    replacement = ""
  ) |>
  purrr::pluck(1)

client_info <- list(
  name = "GMH Communities",
  url = "https://gmhcommunities.com",
  logo = client_logo_path,
  symbol = client_symbol_path
)

rm(client_logo_path, client_symbol_path)

# developer_info ----------------------------------------------------------

noclocks_logo_path <- asset_registry |>
  dplyr::filter(
    file == "noclocks-logo-black.svg"
  ) |>
  dplyr::pull(path) |>
  stringr::str_replace_all(
    pattern = "inst/",
    replacement = ""
  ) |>
  purrr::pluck(1)

noclocks_symbol_path <- asset_registry |>
  dplyr::filter(
    file == "noclocks-icon-circular.png"
  ) |>
  dplyr::pull(path) |>
  stringr::str_replace_all(
    pattern = "inst/",
    replacement = ""
  ) |>
  purrr::pluck(1)

developer_info <- list(
  name = "No Clocks, LLC",
  url = "https://noclocks.dev",
  logo = noclocks_logo_path,
  symbol = noclocks_symbol_path
)

rm(noclocks_logo_path, noclocks_symbol_path)


# data_provider_info ------------------------------------------------------

entrata_logo_path <- asset_registry |>
  dplyr::filter(
    file == "entrata-logo-light.png"
  ) |>
  dplyr::pull(path) |>
  stringr::str_replace_all(
    pattern = "inst/",
    replacement = ""
  ) |>
  purrr::pluck(1)

entrata_info <- list(
  name = "Entrata",
  url = "https://gmhcommunities.entrata.com/api/v1/documentation",
  logo = entrata_logo_path,
  symbol = NULL
)

rm(entrata_logo_path)


# done --------------------------------------------------------------------

cli::cli_alert_success(
  "Metadata preparation complete."
)

