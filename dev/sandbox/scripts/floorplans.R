prop_ids <- c(
  "739076",
  "739085",
  "739079",
  "739080",
  "739084",
  "641240",
  "676055",
  "1115679",
  "1161867",
  "518044",
  "952515",
  "577897",
  "518041",
  "518042",
  "1197886",
  "833617",
  "1197887",
  "518046",
  "1143679",
  "1311849"
)

property_floorplans <- purrr::map(
  prop_ids,
  entrata_floorplans
) |>
  setNames(prop_ids)

property_floorplans[[1]][["floorplans"]] |>
  dplyr::glimpse()


