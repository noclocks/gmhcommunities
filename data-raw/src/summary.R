
#  ------------------------------------------------------------------------
#
# Title : Pre-Lease Summary Table
#    By : Jimmy Briggs
#  Date : 2024-12-04
#
#  ------------------------------------------------------------------------

empty_pre_lease_summary_table <- tibble::tibble(
  property_id = integer(),
  property_name = character(),
  investment_partner = character(),
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

db_pre_lease_summary_table <- dplyr::tbl(main_conn, I("public.summary_report")) |>
  dplyr::collect() |>
  setNames(
    c(
      "property_name",
      "total_beds",
      "model_beds",
      "current_occupency",
      "total_new",
      "total_renewals",
      "total_leases",
      "prelease_percent",
      "prior_total_new",
      "prior_total_renewals",
      "prior_total_leases",
      "prior_prelease_percent",
      "yoy_variance_1",
      "yoy_variance_2",
      "weekly_new_leases",
      "weekly_new_renewals",
      "weekly_new_total",
      "weekly_new_pct_gained",
      "beds_left_to_lease",
      "weekly_velocity_needed_90",
      "weekly_velocity_needed_95",
      "weekly_velocity_needed_100"
    )
  )

