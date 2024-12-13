
#  ------------------------------------------------------------------------
#
# Title : Static Assets Registry Data Preparation
#    By : Jimmy Briggs
#  Date : 2024-11-09
#
#  ------------------------------------------------------------------------

require(tibble)
require(fs)
require(stringr)
require(usethis)

root_dir <- "inst/www"

assets <- fs::dir_ls(root_dir, recurse = TRUE, type = c("file", "directory")) |>
  as.character()

asset_registry <- tibble::tibble(
  path = assets,
  file = basename(assets),
  type = as.character(ifelse(fs::is_dir(assets), "directory", "file")),
  parent = as.character(fs::path_dir(assets)),
  category = as.character(stringr::str_extract(fs::path_rel(assets, start = root_dir), "^[^/]+")),
  file_type = as.character(ifelse(type == "file", fs::path_ext(fs::path(assets)), NA_character_)),
  size = as.numeric(ifelse(type == "file", fs::file_size(assets), NA_real_)),
  description = as.character(dplyr::case_when(
    stringr::str_detect(path, "favicons") ~ "Browser icons and shortcuts",
    stringr::str_detect(path, "logos") ~ "Logos for branding",
    stringr::str_detect(path, "illustrations") ~ "Illustrative graphics",
    TRUE ~ NA_character_
  ))
)


# cleanup -----------------------------------------------------------------

rm(root_dir, assets)
