pkgload::load_all()

# get property IDs
prop_ids <- mem_get_entrata_property_ids()

# get property datasets

# for each property ID, get its associated floorplans
property_floorplans <- purrr::map_dfr(
  prop_ids,
  function(x) {
    httptest2::capture_requests(
      entrata_properties_getFloorPlans(x)
    )
  }
)

readr::write_csv(property_floorplans, "inst/extdata/property_floorplans.csv")
fs::file_copy("inst/extdata/property_floorplans.csv", "data-raw/data/working/entrata_property_floorplans.csv", overwrite = TRUE)
