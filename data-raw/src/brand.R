
#  ------------------------------------------------------------------------
#
# Title : Brand Data Preparation
#    By : Jimmy Briggs
#  Date : 2024-06-11
#
#  ------------------------------------------------------------------------

source("data-raw/R/brandfetch.R")

# setup brand directory --------------------------------------------------

brand_dirs <- c(
  "data-raw/brand",
  "data-raw/brand/fonts",
  "data-raw/brand/images",
  "data-raw/brand/logos",
  "data-raw/brand/styles",
  "data-raw/brand/colors"
)

purrr::walk(brand_dirs, fs::dir_create, recurse = TRUE)

# setup cache ------------------------------------------------------------

brand_cache <- memoise::cache_filesystem("data-raw/cache/brand/", compress = TRUE)
fetch_brand_mem <- memoise::memoise(fetch_brand, cache = brand_cache)


# gmhcommunities brand ---------------------------------------------------

gmh_brand <- fetch_brand_mem("gmhcommunities.com")
noclocks_brand <- fetch_brand_mem("noclocks.dev")
entrata_brand <- fetch_brand_mem("entrata.com")

brands <- dplyr::bind_rows(
  gmh_brand,
  noclocks_brand,
  entrata_brand
)

download_brand_logos(noclocks_brand)


# write brand data --------------------------------------------------------
