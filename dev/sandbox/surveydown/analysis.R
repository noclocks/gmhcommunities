survey_response_data <- readr::read_csv("dev/sandbox/surveydown/preview_data.csv") |>
  dplyr::filter(!is.na(property_name))
