params <- list(
  propertyId = as.integer(property_id),
  applicationId = as.integer(application_id),
  customerId = as.integer(customer_id),
  leaseStatusTypeIds = lease_status_type_ids,
  leaseIds = lease_ids,
  scheduledArCodeIds = scheduled_ar_code_ids,
  unitNumber = unit_number,
  buildingName = building_name,
  moveInDateFrom = move_in_date_from,
  moveInDateTo = move_in_date_to,
  leaseExpiringDateFrom = lease_expiring_date_from,
  leaseExpiringDateTo = lease_expiring_date_to,
  moveOutDateFrom = move_out_date_from,
  moveOutDateTo = move_out_date_to,
  includeOtherIncomeLeases = include_other_income_leases,
  residentFriendlyMode = resident_friendly_mode,
  includeLeaseHistory = include_lease_history,
  includeArTransactions = include_ar_transactions
) |>
  purrr::compact()
