
# https://gmhcommunities.entrata.com/api/v1/documentation

# endpoints ---------------------------------------------------------------

entrata_endpoints <- c(
  "status",
  "applications",
  "arcodes",
  "arpayments",
  "artransactions",
  "communications",
  "customers",
  "financial",
  "leads",
  "leases",
  "leasingcenter",
  "maintenance",
  "pricing",
  "properties",
  "pricing",
  "propertyunits",
  "queue",
  "reports",
  "vendors"
)


# methods -----------------------------------------------------------------

entrata_methods <- list(
  "status" = c(
    "getStatus"
  ),
  "applications" = c(
    "getCompanyApplications",
    "sendApplicantGeneralDetails",
    "sendApplication",
    "sendApplicationAddOns",
    "sendApplicationEmployers",
    "sendApplicationPets",
    "sendApplicationVehicles",
    "updateApplication"
  ),
  "arcodes" = c(
    "getArCodes"
  ),
  "arpayments" = c(
    "getArPayments"
  ),
  "artransactions" = c(
    "getArInvoices",
    "getLeaseArTransactions",
    "getMitsLeaseArTransactions",
    "sendLeaseArTransactionReversals",
    "sendLeaseArTransactions"
  ),
  "communications" = c(
    "getMarketingPreferencePickList",
    "getMarketingPreferences"
  ),
  "customers" = c(
    "getCustomers",
    "getCustomerTestimonials",
    "getTestimonialPickLists",
    "searchCustomers",
    "sendCustomerTestimonials",
    "updateCustomers",
    "updateCustomerTestimonials",
    "updatePropertyResponse"
  ),
  "financial" = c(
    "getApCodes",
    "getBankAccounts",
    "getBudgetActuals",
    "getBudgets",
    "getFinancialPickList",
    "getGlTransactions",
    "getGlTrees",
    "getJobCategories",
    "getJobCostBudgets",
    "getJobs",
    "getTransactionTagLists",
    "markGlTransactionsExported",
    "sendBudgets",
    "sendJournalEntries",
    "updateBudgets"
  ),
  "leads" = c(
    "applyQuote",
    "generateQuotes",
    "getLeadEvents",
    "getLeadPickLists",
    "getLeads",
    "getMitsLeads",
    "getQuotes",
    "sendLeads",
    "sendMitsLeads",
    "updateLeads"
  ),
  "leases" = c(
    "cancelLease",
    "getEvictedLeases",
    "getExpiringLeases",
    "getLeaseDetails",
    "getLeaseDocuments",
    "getLeaseDocumentsList",
    "getLeasePickList",
    "getLeases",
    "getMitsCollections",
    "getMitsLeases",
    "getParcelAlerts",
    "getRentersInsurancePolicies",
    "moveInLease",
    "moveOutLease",
    "onNoticeLease",
    "sendLeaseActivities",
    "sendLeaseDocuments",
    "sendLeases",
    "sendRentersInsurancePolicies",
    "sendRoommateGroups",
    "sendScheduledCharges",
    "updateLease",
    "updateScheduledCharges"
  ),
  "leasingcenter" = c(
    "getCallLogs",
    "getLeasingCenterPickLists"
  ),
  "maintenance" = c(
    "getInspections",
    "getInspectionTemplates",
    "getWorkOrderPickLists",
    "getWorkOrders",
    "sendWorkOrders",
    "updateWorkOrders"
  ),
  "pricing" = c(
    "getPricingPicklists",
    "insertPricing",
    "sendBudgetedRent"
  ),
  "properties" = c(
    "getAmenityReservations",
    "getCalendarAvailability",
    "getFloorPlans",
    "getPetTypes",
    "getPhoneNumber",
    "getProperties",
    "getPropertyAddOns",
    "getPropertyAnnouncements",
    "getPropertyPickLists",
    "getRentableItems",
    "getReservableAmenities",
    "getWebsites",
    "sendFloorplans",
    "sendRentableItems"
  ),
  "propertyunits" = c(
    "getAmenities",
    "getMitsPropertyUnits",
    "getPropertyUnits",
    "getSpecials",
    "getUnitsAvailabilityAndPricing",
    "getUnitTypes",
    "sendAmenities",
    "sendPropertyUnits",
    "updateAmenities"
  ),
  "queue" = c("getResponse"),
  "reports" = c(
    "getDependentFilter",
    "getReportData",
    "getReportInfo",
    "getReportList"
  ),
  "vendors" = c(
    "getInvoices",
    "getPoReceivingRecords",
    "getPurchaseOrders",
    "getTaxFormData",
    "getVendorLocations",
    "getVendorPickLists",
    "getVendors",
    "markInvoicesExported",
    "sendInvoices",
    "sendPurchaseOrders",
    "sendVendors",
    "updateInvoices",
    "updateVendors",
    "voidApPayments"
  )
)

# methods table -----------------------------------------------------------
entrata_methods_tbl <- tibble::enframe(
  entrata_methods,
  name = "endpoint",
  value = "method"
) |>
  tidyr::unnest(cols = c("method"))


# parameters --------------------------------------------------------------

entrata_method_params <- list(
  "status" = list(
    "getStatus" = list(NULL)
  ),
  "customers" = list(
    "getCustomers" = list(
      "propertyId" = list(
        "type" = "string",
        "required" = TRUE,
        "multiple" = FALSE,
        "description" = stringr::str_c(
          "This is a required field. This field accepts single value for the ",
          "Property ID."
        )
      ),
      "customerIds" = list(
        type = "string",
        required = FALSE,
        multiple = TRUE,
        description = stringr::str_c(
          "This is an optional field. This field accepts comma separated ",
          "multiple values for the Customer IDs."
        )
      ),
      "leaseStatusTypeIds" = list(
        type = "string",
        required = FALSE,
        multiple = TRUE,
        description = stringr::str_c(
          "This is an optional field. This field accepts comma separated ",
          "multiple values for the Lease Status Type IDs."
        )
      ),
      "isAgreedToTermsOnly" = list(
        type = "boolean_string",
        required = FALSE,
        description = stringr::str_c(
          "This is an optional field. This field accepts single value.",
          "`isAgreedToTermsOnly` returns the date when a customer has agreed to ",
          "the Terms and Conditions of Entrata resident portal."
        )
      ),
      "companyIdentificationTypeIds" = list(
        type = "integer",
        required = FALSE,
        multiple = TRUE,
        description = stringr::str_c(
          "This is an optional field. This field accepts comma seperated ",
          "multiple values. company Identification type IDs."
        )
      )
    )
  ),
  "financial" = list(
    "getBudgetActuals" = list(
      "propertyId" = list(
        type = "string",
        required = TRUE,
        description = stringr::str_c(
          "This is a required field. This field accepts single value ",
          "for `propertyId`."
        )
      ),
      "glTreeId" = list(
        type = "integer",
        required = TRUE,
        description = stringr::str_c(
          "This is a required field. This field accepts a single value for ",
          "GL Tree ID."
        )
      ),
      "budgetId" = list(
        type = "integer",
        required = TRUE,
        description = stringr::str_c(
          "This is a required field. This field accepts a single value for ",
          "Budget ID."
        )
      ),
      "postMonthFrom" = list(
        type = "string",
        required = TRUE,
        description = stringr::str_c(
          "This is a required field. This field accepts a single value in the ",
          "format MM/YYYY for the start month."
        )
      ),
      "postMonthTo" = list(
        type = "string",
        required = TRUE,
        description = stringr::str_c(
          "This is a required field. This field accepts a single value in the ",
          "format MM/YYYY for the end month."
        )
      ),
      glBookTypeIds = list(
        type = "integer",
        required = FALSE,
        multiple = TRUE,
        description = stringr::str_c(
          "This is an optional field. This field accepts comma separated ",
          "multiple values for GL Book Type IDs."
        )
      ),
      budgetStatusTypeId = list(
        type = "integer",
        required = FALSE,
        description = stringr::str_c(
          "This is an optional field. This field accepts a single value for ",
          "Budget Status Type ID."
        )
      ),
      accountingMethod = list(
        type = "string",
        required = TRUE,
        description = stringr::str_c(
          "This is a required field. This field accepts a single value for ",
          "the Accounting Method."
        )
      )
    ),
    "getBudgets" = list(
      propertyId = list(
        type = "string",
        required = TRUE,
        description = stringr::str_c(
          "This is a required field. This field accepts a single value for ",
          "`propertyId`."
        )
      ),
      "budgetIds" = list(
        type = "string",
        required = FALSE,
        multiple = TRUE,
        description = stringr::str_c(
          "This is an optional field. This field accepts comma separated ",
          "multiple values for the Budget IDs."
        )
      ),
      "budgetStatusTypeIds" = list(
        type = "string",
        required = FALSE,
        multiple = TRUE,
        description = stringr::str_c(
          "This is an optional field. This field accepts comma separated ",
          "multiple values for the Budget Status Type IDs."
        )
      ),
      "fiscalYears" = list(
        type = "string",
        required = FALSE,
        multiple = TRUE,
        description = stringr::str_c(
          "This is an optional field. This field accepts comma separated ",
          "multiple values for the Fiscal Years."
        )
      )
    )
  ),
  "leases" = list(
    "getLeases" = list(
      propertyId = list(
        type = "string",
        required = TRUE,
        description = stringr::str_c(
          "This is a required field. This field accepts a single value for ",
          "the Property ID."
        )
      ),
      applicationId = list(
        type = "integer",
        required = FALSE,
        description = stringr::str_c(
          "This is an optional field. This field accepts a single value for ",
          "the Application ID."
        )
      ),
      customerId = list(
        type = "integer",
        required = FALSE,
        description = stringr::str_c(
          "This is an optional field. This field accepts a single value for ",
          "the Customer ID."
        )
      ),
      leaseStatusTypeIds = list(
        type = "integer",
        required = FALSE,
        multiple = TRUE,
        description = stringr::str_c(
          "This is an optional field. This field accepts comma separated ",
          "multiple values for Lease Status Type IDs."
        )
      ),
      leaseIds = list(
        type = "integer",
        required = FALSE,
        multiple = TRUE,
        description = stringr::str_c(
          "This is an optional field. This field accepts comma separated ",
          "multiple values for Lease IDs."
        )
      ),
      scheduledArCodeIds = list(
        type = "integer",
        required = FALSE,
        multiple = TRUE,
        description = stringr::str_c(
          "This is an optional field. This field accepts comma separated ",
          "multiple values for Scheduled AR Code IDs."
        )
      ),
      unitNumber = list(
        type = "string",
        required = FALSE,
        description = stringr::str_c(
          "This is an optional field. This field accepts a single value for ",
          "the Unit Number."
        )
      ),
      buildingName = list(
        type = "string",
        required = FALSE,
        description = stringr::str_c(
          "This is an optional field. This field accepts a single value for ",
          "the Building Name."
        )
      ),
      moveInDateFrom = list(
        type = "date",
        required = FALSE,
        description = stringr::str_c(
          "This is an optional field. This field accepts a single value for ",
          "the start Move-in Date."
        )
      ),
      moveInDateTo = list(
        type = "date",
        required = FALSE,
        description = stringr::str_c(
          "This is an optional field. This field accepts a single value for ",
          "the end Move-in Date."
        )
      ),
      leaseExpiringDateFrom = list(
        type = "date",
        required = FALSE,
        description = stringr::str_c(
          "This is an optional field. This field accepts a single value for ",
          "the Lease Expiring Date (start)."
        )
      ),
      leaseExpiringDateTo = list(
        type = "date",
        required = FALSE,
        description = stringr::str_c(
          "This is an optional field. This field accepts a single value for ",
          "the Lease Expiring Date (end)."
        )
      ),
      moveOutDateFrom = list(
        type = "date",
        required = FALSE,
        description = stringr::str_c(
          "This is an optional field. This field accepts a single value for ",
          "the Move-out Date (start)."
        )
      ),
      moveOutDateTo = list(
        type = "date",
        required = FALSE,
        description = stringr::str_c(
          "This is an optional field. This field accepts a single value for ",
          "the Move-out Date (end)."
        )
      ),
      includeOtherIncomeLeases = list(
        type = "boolean_string",
        required = FALSE,
        description = stringr::str_c(
          "This is an optional field. This field accepts a single value. If ",
          "provided, this will include leases with other income."
        )
      ),
      residentFriendlyMode = list(
        type = "boolean_string",
        required = FALSE,
        description = stringr::str_c(
          "This is an optional field. This field accepts a single value. If ",
          "provided, this will use Resident Friendly Mode."
        )
      ),
      includeLeaseHistory = list(
        type = "boolean_string",
        required = FALSE,
        description = stringr::str_c(
          "This is an optional field. This field accepts a single value. If ",
          "provided, lease history will be included."
        )
      ),
      includeArTransactions = list(
        type = "boolean_string",
        required = FALSE,
        description = stringr::str_c(
          "This is an optional field. This field accepts a single value. If ",
          "provided, it will return the AR Transactions associated with the ",
          "lease."
        )
      )
    ),
    "getLeaseDetails" = list(
      propertyId = list(
        type = "string",
        required = TRUE,
        description = stringr::str_c(
          "This is a required field. This field accepts a single value for ",
          "the Property ID."
        )
      ),
      leaseId = list(
        type = "integer",
        required = TRUE,
        description = stringr::str_c(
          "This is a required field. This field accepts a single value for ",
          "the Lease ID."
        )
      ),
      leaseStatusTypeIds = list(
        type = "integer",
        required = FALSE,
        multiple = TRUE,
        description = stringr::str_c(
          "This is an optional field. This field accepts comma separated ",
          "multiple values for Lease Status Type IDs."
        )
      ),
      includeAddOns = list(
        type = "boolean_string",
        required = FALSE,
        description = stringr::str_c(
          "This is an optional field. This field accepts a single value. If ",
          "provided, it will include lease add-ons."
        )
      ),
      includeCharge = list(
        type = "boolean_string",
        required = FALSE,
        description = stringr::str_c(
          "This is an optional field. This field accepts a single value. If ",
          "provided, it will include charges."
        )
      )
    ),
    "getLeaseDocuments" = list(
      propertyId = list(
        type = "integer",
        required = TRUE,
        description = stringr::str_c(
          "This is a required field. This field accepts a single value for ",
          "the Property ID."
        )
      ),
      leaseId = list(
        type = "integer",
        required = TRUE,
        description = stringr::str_c(
          "This is a required field. This field accepts a single value for ",
          "the Lease ID."
        )
      ),
      externalLeaseId = list(
        type = "string",
        required = FALSE,
        description = stringr::str_c(
          "This is an optional field. This field accepts a single value for ",
          "the external Lease ID."
        )
      ),
      documentIds = list(
        type = "integer",
        required = FALSE,
        multiple = TRUE,
        description = stringr::str_c(
          "This is an optional field. This field accepts comma separated ",
          "multiple values for Document IDs."
        )
      ),
      fileTypesCode = list(
        type = "string",
        required = FALSE,
        multiple = TRUE,
        description = stringr::str_c(
          "This is an optional field. This field accepts comma separated ",
          "multiple values for file types (system codes)."
        )
      ),
      addedOnFromDate = list(
        type = "date",
        required = FALSE,
        description = stringr::str_c(
          "This is an optional field. This field accepts a single value for ",
          "the Added On date (start)."
        )
      ),
      showDeletedFile = list(
        type = "boolean_string",
        required = FALSE,
        description = stringr::str_c(
          "This is an optional field. This field accepts a single value. If ",
          "provided, it will return deleted files."
        )
      )
    ),
    "getLeaseDocumentsList" = list(
      propertyId = list(
        type = "string",
        required = TRUE,
        description = stringr::str_c(
          "This is a required field. This field accepts a single value for ",
          "the Property ID."
        )
      ),
      leaseId = list(
        type = "integer",
        required = TRUE,
        description = stringr::str_c(
          "This is a required field. This field accepts a single value for ",
          "the Lease ID."
        )
      ),
      externalLeaseId = list(
        type = "string",
        required = FALSE,
        description = stringr::str_c(
          "This is an optional field. This field accepts a single value for ",
          "the external Lease ID."
        )
      ),
      fileTypesCode = list(
        type = "string",
        required = FALSE,
        multiple = TRUE,
        description = stringr::str_c(
          "This is an optional field. This field accepts comma separated ",
          "multiple values for file types (system codes)."
        )
      ),
      showDeletedFile = list(
        type = "boolean_string",
        required = FALSE,
        description = stringr::str_c(
          "This is an optional field. This field accepts a single value. If ",
          "provided, it will return deleted files."
        )
      ),
      leaseStatusTypeIds = list(
        type = "integer",
        required = FALSE,
        multiple = TRUE,
        description = stringr::str_c(
          "This is an optional field. This field accepts comma separated ",
          "multiple values for Lease Status Type IDs."
        )
      )
    )
  ),
  "properties" = list(
    "getProperties" = list(
      propertyIds = list(
        type = "string",
        required = FALSE,
        multiple = TRUE,
        description = stringr::str_c(
          "This is an optional field. This field accepts comma separated ",
          "multiple values (concatenated into a single, comma-separated string) ",
          "representing Property IDs."
        )
      ),
      propertyLookupCode = list(
        type = "string",
        required = FALSE,
        description = stringr::str_c(
          "This is an optional field. This field accepts a single value for ",
          "the Property Lookup Code."
        )
      ),
      showAllStatus = list(
        type = "boolean_string",
        required = FALSE,
        description = stringr::str_c(
          "This is an optional field. This field accepts a single value to ",
          "show all statuses."
        )
      )
    ),
    "getFloorPlans" = list(
      propertyId = list(
        type = "integer",
        required = TRUE,
        description = stringr::str_c(
          "This is a required field. This field accepts a single value for ",
          "the Property ID."
        )
      ),
      propertyFloorPlanIds = list(
        type = "string",
        required = FALSE,
        multiple = TRUE,
        description = stringr::str_c(
          "This is an optional field. This field accepts comma separated ",
          "multiple values for Property Floor Plan IDs."
        )
      ),
      usePropertyPreferences = list(
        type = "boolean_string",
        required = FALSE,
        description = stringr::str_c(
          "This is an optional field. This field accepts a single value. ",
          "Checks for the property settings manipulate floorplan data and ",
          "honor those settings."
        )
      ),
      includeDisabledFloorPlans = list(
        type = "boolean_string",
        required = FALSE,
        description = stringr::str_c(
          "This is an optional field. This field accepts a single value. If ",
          "provided value is 1, then disabled floorplans will be included along ",
          "with the enabled ones."
        )
      )
    ),
    "getPropertyAddOns" = list(
      propertyId = list(
        type = "integer",
        required = TRUE,
        description = stringr::str_c(
          "This is a required field. This field accepts a single value for ",
          "the Property ID."
        )
      ),
      addOnIds = list(
        type = "integer",
        required = FALSE,
        multiple = TRUE,
        description = stringr::str_c(
          "This is an optional field. This field accepts comma separated ",
          "multiple values for the Add-On IDs."
        )
      )
    ),
    "getPropertyAnnouncements" = list(
      propertyId = list(
        type = "integer",
        required = TRUE,
        description = stringr::str_c(
          "This is a required field. This field accepts a single value for ",
          "the Property ID."
        )
      ),
      announcementIds = list(
        type = "integer",
        required = FALSE,
        multiple = TRUE,
        description = stringr::str_c(
          "This is an optional field. This field accepts comma separated ",
          "multiple values for the Announcement IDs."
        )
      )
    ),
    "getPropertyRentableItems" = list(
      propertyId = list(
        type = "integer",
        required = TRUE,
        description = stringr::str_c(
          "This is a required field. This field accepts a single value for ",
          "the Property ID."
        )
      ),
      rentableItemIds = list(
        type = "integer",
        required = FALSE,
        multiple = TRUE,
        description = stringr::str_c(
          "This is an optional field. This field accepts comma separated ",
          "multiple values for the Rentable Item IDs."
        )
      )
    )
  ),
  "propertyunits" = list(
    "getPropertyUnits" = list(
      propertyIds = list(
        type = "string",
        required = TRUE,
        multiple = TRUE,
        description = stringr::str_c(
          "This is a required field. This field accepts comma separated ",
          "multiple values for Property IDs."
        )
      ),
      availableUnitsOnly = list(
        type = "boolean_string",
        required = FALSE,
        description = stringr::str_c(
          "This is an optional field. This field accepts a single value. If ",
          "provided value is 1, only available units will be returned."
        )
      ),
      usePropertyPreferences = list(
        type = "boolean_string",
        required = FALSE,
        description = stringr::str_c(
          "This is an optional field. This field accepts a single value. If ",
          "enabled, the Entrata setting 'Max Available Units per Floor Plan' ",
          "will be honored and the available units returned will be reduced. ",
          "Also requires using the availableUnitsOnly parameter as true (1)."
        )
      ),
      includeDisabledFloorPlans = list(
        type = "boolean_string",
        required = FALSE,
        description = stringr::str_c(
          "This is an optional field. This field accepts a single value. If ",
          "provided value is 1, disabled floorplans will be included along with ",
          "the enabled ones."
        )
      ),
      includeDisabledUnits = list(
        type = "boolean_string",
        required = FALSE,
        description = stringr::str_c(
          "This is an optional field. This field accepts a single value. If ",
          "provided value is 1, disabled units will be included along with the ",
          "enabled ones."
        )
      )
    ),
    "getUnitTypes" = list(
      propertyId = list(
        type = "integer",
        required = TRUE,
        multiple = FALSE,
        description = stringr::str_c(
          "This is a required field. This field accepts a single value for ",
          "the Property ID."
        )
      )
    )
  ),
  "queue" = list(
    "getResponse" = list(
      queueId = list(
        type = "string",
        required = TRUE,
        description = stringr::str_c(
          "This is a required field. This field accepts single value. This node ",
          "accepts the queueId. This should be a JWT token. This token should ",
          "get generated from the getReportData(r3) version."
        )
      ),
      serviceName = list(
        type = "string",
        required = TRUE,
        description = stringr::str_c(
          "This is a required field. This field accepts single value. This node ",
          "accepts the service name for which the apiToken is added."
        )
      )
    )
  ),
  "reports" = list(
    "getReportDependentFilter" = list(
      reportName = list(
        type = "string",
        required = TRUE,
        description = stringr::str_c(
          "This is a required field. This field accepts a single value for ",
          "the report name. Once passed, the web service will return values ",
          "of the dependent filter for that report."
        )
      ),
      dependentFilter = list(
        type = "string",
        required = TRUE,
        description = stringr::str_c(
          "This is a required field. This field accepts a single value for ",
          "the dependent filter. The values of these filters will be returned ",
          "based on the parent fields mentioned in the 'filters' section."
        )
      ),
      filters = list(
        type = "string",
        required = TRUE,
        description = stringr::str_c(
          "This is a required field. This field accepts a single value. ",
          "Parent filters are mentioned here, and based on these values, the ",
          "dependent filters will be returned."
        )
      )
    ),
    "getReportData" = list(
      reportName = list(
        type = "string",
        required = TRUE,
        description = stringr::str_c(
          "This is a required field. This field accepts a single value for ",
          "the report name. Once passed, the web service will return report ",
          "data for that report."
        )
      ),
      reportVersion = list(
        type = "string",
        required = TRUE,
        description = stringr::str_c(
          "This is an optional field. This field accepts a single value. ",
          "Will filter results as per the available report versions."
        )
      ),
      filters = list(
        type = "object",
        required = TRUE,
        description = stringr::str_c(
          "This is a required field. This field accepts a single value. ",
          "For each report, there are filter parameters required, such as ",
          "property ID, dates, etc. You can get filters for each report ",
          "using the getReportsInfo web service."
        )
      )
    ),
    "getReportInfo" = list(
      reportName = list(
        type = "string",
        required = TRUE,
        description = stringr::str_c(
          "This is a required field. This field accepts a single value for ",
          "the report name."
        )
      ),
      reportVersion = list(
        type = "string",
        required = FALSE,
        description = stringr::str_c(
          "This is an optional field. This field accepts a single value. ",
          "Will filter results based on the available report versions."
        )
      )
    ),
    "getReportList" = list(NULL)
  )
)

# parameters table --------------------------------------------------------

entrata_params_tbl <- entrata_method_params |>
  tibble::enframe(name = "endpoint", value = "methods") |>
  tidyr::unnest_longer(methods) |>
  dplyr::mutate(
    method = names(methods)
  ) |>
  tidyr::unnest_longer(methods, values_to = "parameters") |>
  dplyr::mutate(
    parameter = names(parameters)
  ) |>
  tidyr::unnest_wider(parameters, names_sep = "_") |>
  dplyr::mutate(
    multiple = dplyr::coalesce(parameters_multiple, FALSE)
  ) |>
  dplyr::select(
    endpoint,
    method,
    parameter,
    type = parameters_type,
    required = parameters_required,
    multiple,
    description = parameters_description
  ) |>
  dplyr::arrange(
    endpoint,
    method,
    dplyr::desc(required),
    parameter
  )
