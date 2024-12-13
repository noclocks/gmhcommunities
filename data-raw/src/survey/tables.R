
# commonwealth_merged_survey_data <- dplyr::bind_cols(
#   commonwealth_market_survey_data$property_summary$property_summary_wide |>
#     janitor::clean_names(),
#   commonwealth_market_survey_data$leasing_summary$leasing_summary_wide |>
#     janitor::clean_names() |>
#     dplyr::select(
#       -c(
#         property_id,
#         property_name,
#         leasing_week
#       )
#     ),
#   commonwealth_market_survey_data$short_term_leases$short_term_leases_wide |>
#     janitor::clean_names() |>
#     dplyr::select(
#       -c(
#         property_id,
#         property_name,
#         leasing_week
#       )
#     ),
#   commonwealth_market_survey_data$fees$fees_wide |>
#     janitor::clean_names() |>
#     dplyr::select(
#       -c(
#         property_id,
#         property_name,
#         leasing_week
#       )
#     ),
#   commonwealth_market_survey_data$amenities$unit_amenities_wide |>
#     janitor::clean_names() |>
#     dplyr::select(
#       -c(
#         property_id,
#         property_name,
#         leasing_week
#       )
#     ),
#   commonwealth_market_survey_data$amenities$property_amenitites_wide |>
#     janitor::clean_names() |>
#     dplyr::select(
#       -c(
#         property_id,
#         property_name,
#         leasing_week
#       )
#     ),
#   commonwealth_market_survey_data$notes$notes_leasing_special |>
#     janitor::clean_names() |>
#     dplyr::select(
#       -c(
#         property_id,
#         property_name,
#         leasing_week
#       )
#     )
#   # commonwealth_market_survey_data$utilities$utilities_summary_1
#   # commonwealth_market_survey_data$utilities$utilities_summary_2
# )
