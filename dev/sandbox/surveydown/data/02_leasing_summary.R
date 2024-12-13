leasing_summary_inputs <- tibble::tibble(
  order = c(1:14),
  name = c(
    "Reporting Cycle",
    "Lease Launch Date",
    "Renewal Launch Date",
    "Current Occupancy",
    "Prior Year Occupancy",
    "Current Pre-Lease",
    "Prior Year Pre-Lease",
    "Total Renewals",
    "Total New Leases",
    "Total Weekly Leases",
    "Total Weekly Traffic",
    "Current Incentive",
    "Incentive Amount",
    "Data Last Updated"
  ),
  type = c(
    "mc",      # Reporting Cycle
    "date",    # Lease Launch Date
    "date",    # Renewal Launch Date
    "numeric", # Current Occupancy
    "numeric", # Prior Year Occupancy
    "numeric", # Current Pre-Lease
    "numeric", # Prior Year Pre-Lease
    "numeric", # Total Renewals
    "numeric", # Total New Leases
    "numeric", # Total Weekly Leases
    "numeric", # Total Weekly Traffic
    "mc",      # Current Incentive
    "numeric", # Incentive Amount
    "date"     # Data Last Updated
  ),

  id = c(
    "reporting_cycle",
    "lease_launch_date",
    "renewal_launch_date",
    "current_occupancy",
    "prior_year_occupancy",
    "current_pre_lease",
    "prior_year_pre_lease",
    "total_renewals",
    "total_new_leases",
    "total_weekly_leases",
    "total_weekly_traffic",
    "current_incentive",
    "incentive_amount",
    "data_last_updated"
  ),
  label = c(
    "Reporting Cycle",
    "Lease Launch Date",
    "Renewal Launch Date",
    "Current Occupancy",
    "Prior Year Occupancy",
    "Current Pre-Lease",
    "Prior Year Pre-Lease",
    "Total Renewals",
    "Total New Leases",
    "Total Weekly Leases",
    "Total Weekly Traffic",
    "Current Incentive",
    "Incentive Amount",
    "Data Last Updated"
  ),
  required = c(
    TRUE,  # Reporting Cycle
    TRUE,  # Lease Launch Date
    TRUE,  # Renewal Launch Date
    TRUE,  # Current Occupancy
    TRUE,  # Prior Year Occupancy
    TRUE,  # Current Pre-Lease
    TRUE,  # Prior Year Pre-Lease
    TRUE,  # Total Renewals
    TRUE,  # Total New Leases
    TRUE,  # Total Weekly Leases
    TRUE,  # Total Weekly Traffic
    TRUE,  # Current Incentive
    TRUE,  # Incentive Amount
    TRUE   # Data Last Updated
  ),
  options = NULL
)

leasing_summary_inputs <- dplyr::mutate(
  leasing_summary_inputs,
  options = dplyr::case_when(
    name == "Reporting Cycle" ~ list(c(
      "Saturday-Friday",
      "Sunday-Monday",
      "Monday-Sunday"
    )),
    name == "Current Incentive" ~ list(c(
      "Gift Card",
      "Monthly Concession",
      "One-Time Concession",
      "None"
    )),
    TRUE ~ list(NULL)
  )
)

leasing_summary_questions <- purrr::map(
  1:nrow(leasing_summary_inputs),
  function(i) {
    args <- leasing_summary_inputs[i, ]
    surveydown::sd_question(
      id = as.character(args$id),
      type = args$type,
      label = args$label,
      option = unlist(args$options)
    )
  }
)
