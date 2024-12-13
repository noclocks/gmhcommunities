entrata_methods_tbl_constructed <- tibble::tibble(
  endpoint = rep(
    c(
      "status", "applications", "arcodes", "arpayments", "artransactions",
      "communications", "customers", "financial", "leads", "leases",
      "leasingcenter", "maintenance", "pricing", "properties", "propertyunits",
      "queue", "reports", "vendors"
    ),
    c(1L, 8L, 1L, 1L, 5L, 2L, 8L, 15L, 10L, 23L, 2L, 6L, 3L, 14L, 9L, 1L, 4L, 14L)
  ),
  method = c(
    "getStatus", "getCompanyApplications", "sendApplicantGeneralDetails",
    "sendApplication", "sendApplicationAddOns", "sendApplicationEmployers",
    "sendApplicationPets", "sendApplicationVehicles", "updateApplication",
    "getArCodes", "getArPayments", "getArInvoices", "getLeaseArTransactions",
    "getMitsLeaseArTransactions", "sendLeaseArTransactionReversals",
    "sendLeaseArTransactions", "getMarketingPreferencePickList",
    "getMarketingPreferences", "getCustomers", "getCustomerTestimonials",
    "getTestimonialPickLists", "searchCustomers", "sendCustomerTestimonials",
    "updateCustomers", "updateCustomerTestimonials", "updatePropertyResponse",
    "getApCodes", "getBankAccounts", "getBudgetActuals", "getBudgets",
    "getFinancialPickList", "getGlTransactions", "getGlTrees", "getJobCategories",
    "getJobCostBudgets", "getJobs", "getTransactionTagLists",
    "markGlTransactionsExported", "sendBudgets", "sendJournalEntries",
    "updateBudgets", "applyQuote", "generateQuotes", "getLeadEvents",
    "getLeadPickLists", "getLeads", "getMitsLeads", "getQuotes", "sendLeads",
    "sendMitsLeads", "updateLeads", "cancelLease", "getEvictedLeases",
    "getExpiringLeases", "getLeaseDetails", "getLeaseDocuments",
    "getLeaseDocumentsList", "getLeasePickList", "getLeases",
    "getMitsCollections", "getMitsLeases", "getParcelAlerts",
    "getRentersInsurancePolicies", "moveInLease", "moveOutLease", "onNoticeLease",
    "sendLeaseActivities", "sendLeaseDocuments", "sendLeases",
    "sendRentersInsurancePolicies", "sendRoommateGroups", "sendScheduledCharges",
    "updateLease", "updateScheduledCharges", "getCallLogs",
    "getLeasingCenterPickLists", "getInspections", "getInspectionTemplates",
    "getWorkOrderPickLists", "getWorkOrders", "sendWorkOrders",
    "updateWorkOrders", "getPricingPicklists", "insertPricing",
    "sendBudgetedRent", "getAmenityReservations", "getCalendarAvailability",
    "getFloorPlans", "getPetTypes", "getPhoneNumber", "getProperties",
    "getPropertyAddOns", "getPropertyAnnouncements", "getPropertyPickLists",
    "getRentableItems", "getReservableAmenities", "getWebsites", "sendFloorplans",
    "sendRentableItems", "getAmenities", "getMitsPropertyUnits",
    "getPropertyUnits", "getSpecials", "getUnitsAvailabilityAndPricing",
    "getUnitTypes", "sendAmenities", "sendPropertyUnits", "updateAmenities",
    "getResponse", "getDependentFilter", "getReportData", "getReportInfo",
    "getReportList", "getInvoices", "getPoReceivingRecords", "getPurchaseOrders",
    "getTaxFormData", "getVendorLocations", "getVendorPickLists", "getVendors",
    "markInvoicesExported", "sendInvoices", "sendPurchaseOrders", "sendVendors",
    "updateInvoices", "updateVendors", "voidApPayments"
  ),
  used = rep(
    rep(c(TRUE, FALSE), 12),
    c(1L, 8L, 1L, 8L, 1L, 7L, 1L, 1L, 2L, 22L, 3L, 3L, 1L, 28L, 1L, 2L, 3L, 3L, 1L, 3L, 5L, 3L, 5L, 14L)
  ),
  default = rep(
    rep(c(TRUE, FALSE), 15),
    c(
      2L, 7L, 3L, 4L, 1L, 1L, 1L, 7L, 1L, 18L, 1L, 12L, 1L, 15L, 1L, 1L, 1L, 5L, 1L,
      7L, 1L, 10L, 1L, 6L, 1L, 3L, 1L, 6L, 1L, 7L
    )
  ),
  version = rep(
    c("r1", "r2", "r1", "r2", "r1", "r2", "r3", "r2", "r1"),
    c(54L, 1L, 3L, 1L, 50L, 1L, 1L, 1L, 15L)
  ),
)
