market_survey_property_summary_data <- tibble::tibble(
  property_id = c(
    "739085",
    "boylston",
    "vanness",
    "bower"
  ),
  property_name = c(
    "1047 Commonwealth Avenue",
    "1330 Boylston",
    "Van Ness",
    "Bower"
  ),
  website = c(
    "https://www.1047commonwealth.com/",
    "https://1330boylston.com/",
    "https://www.thevanness.com/",
    "https://bowerboston.com/"
  ),
  address = c(
    "1047 Commonwealth Ave, Boston, MA, 02215",
    "1330 Boylston St, Boston, MA, 02215",
    "1335 Boylston St, Boston, MA, 02215",
    "771 Beacon St Apartment 775, Boston, MA, 02215"
  ),
  phone_number = c(
    "(617) 500-6481",
    "(617) 267-1330",
    "(617) 424-1335",
    "(617) 341-9700"
  ),
  property_image = c(
    "https://images1.forrent.com/i2/0ksd7Uwm-74efUhB-UiU1cEyVv3NVOiCCVuF4oQ7cZI/117/image.jpg",
    NA_character_,
    NA_character_,
    NA_character_
  ),
  developer = c(
    "BPDA",
    "Samuels and Associates",
    "Samuels And Associates",
    "The Green Cities Company"
  ),
  manager = c(
    "GMH Communities",
    "Samuels and Associates",
    "Samuels And Associates",
    "Greystar"
  ),
  owner = c(
    "AGC + GMH Communities",
    "Samuels and Associates",
    "Samuels And Associates",
    "Greystar"
  ),
  property_type = c(
    "Student",
    "Conventional",
    "Conventional",
    "Conventional"
  ),
  property_status = c("Operational", "Operational", "Operational", "Operational"),
  product_type = c("Mid-Rise", "High-Rise", "High-Rise", "High-Rise"),
  property_rating = c(2L, 5L, 4L, 5L),
  comp_status = c("Subject Property", "Tier 2", "Tier 2", "Tier 2"),
  year_built_or_renovated = c(2017L, 2008L, 2015L, 2020L),
  date_of_most_recent_sale = as.Date(c("2019-01-01", NA_character_, NA_character_, NA_character_)),
  distance_from_campus = c(0.1, 1, 1, 0.2)
)
