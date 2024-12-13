

transform_pre_lease_summary_report_data <- function(summary_data) {

  data |>
    dplyr::mutate(
      "occupancy_rate" = .data$occupied_count / .data$total_beds,
      "pre_lease_pct" = .data$preleased_count / .data$total_beds,
      "yoy_variance" = .data$preleased_percent - .data$preleased_percent_prior
    ) |>
    dplyr::select(
      property_id,
      property_name,


    )

}



empty_leasing_summary_table <- function() {

  tibble::tibble(
    property_id = integer(),
    property_name = character(),
    total_leases = integer(),
    total_beds = integer(),
    model_beds = integer(),
    current_occupied = integer(),
    current_occupency = double(),
    total_new = integer(),
    total_renewals = integer(),
    total_leases = integer(),
    prelease_percent = double(),
    prior_total_new = integer(),
    prior_total_renewals = integer(),
    prior_total_leases = integer(),
    prior_prelease_percent = double(),
    yoy_variance_1 = integer(),
    yoy_variance_2 = double(),
    weekly_new_leases = integer(),
    weekly_new_renewals = integer(),
    weekly_new_total = integer(),
    weekly_new_pct_gained = double(),
    beds_left_to_lease = integer(),
    beds_leased_this_week = integer(),
    weekly_velocity_needed_90 = double(),
    weekly_velocity_needed_95 = double(),
    weekly_velocity_needed_100 = double()
  )

}
