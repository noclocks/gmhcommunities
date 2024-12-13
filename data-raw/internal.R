#  ------------------------------------------------------------------------
#
# Title : Internal Data
#  File : internal.R
#    By : Jimmy Briggs <jimmy.briggs@noclocks.dev>
#  Date : 2024-11-23
#
#  ------------------------------------------------------------------------


# source ------------------------------------------------------------------

source("data-raw/src/entrata.R")
source("data-raw/src/meta.R")
source("data-raw/src/assets.R")
# source("data-raw/src/excel.R")

# source("data-raw/src/brand.R")

# data --------------------------------------------------------------------

usethis::use_data(
  internal = TRUE,
  overwrite = TRUE,
  # meta
  app_info,
  app_choices,
  client_info,
  developer_info,
  entrata_info,
  # assets
  asset_registry,
  # entrata
  entrata_default_methods,
  entrata_method_params,
  entrata_method_versions,
  entrata_method_versions_tbl,
  entrata_methods,
  entrata_methods_tbl,
  entrata_params_tbl,
  entrata_endpoints
)
