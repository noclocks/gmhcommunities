customers_method_params <- list(
  "getCustomers" = list(
    "propertyId" = list(
      # propertyId is type "integer" but is also passed as a string in
      # the response body
      type = "string",
      required = TRUE,
      description = stringr::str_c(
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
)

financial_method_params <- list(
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
    "glBookTypeIds" = list(
      type = "integer",
      required = FALSE,
      multiple = TRUE,
      description = stringr::str_c(
        "This is an optional field. This field accepts comma separated ",
        "multiple values for GL Book Type IDs."
      )
    ),
    "budgetStatusTypeId" = list(
      type = "integer",
      required = FALSE,
      description = stringr::str_c(
        "This is an optional field. This field accepts a single value for ",
        "Budget Status Type ID."
      )
    ),
    "accountingMethod" = list(
      type = "string",
      required = TRUE,
      description = stringr::str_c(
        "This is a required field. This field accepts a single value for ",
        "the Accounting Method."
      )
    )
  ),
  "getBudgets" = list(
    "propertyId" = list(
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
)

leases_method_params <- list(
  "getLeases" = list(
    "propertyId" = list(
      type = "string",
      required = TRUE,
      description = stringr::str_c(
        "This is a required field. This field accepts a single value for ",
        "the Property ID."
      )
    ),
    "applicationId" = list(
      type = "integer",
      required = FALSE,
      description = stringr::str_c(
        "This is an optional field. This field accepts a single value for ",
        "the Application ID."
      )
    ),
    "customerId" = list(
      type = "integer",
      required = FALSE,
      description = stringr::str_c(
        "This is an optional field. This field accepts a single value for ",
        "the Customer ID."
      )
    ),
    "leaseStatusTypeIds" = list(
      type = "integer",
      required = FALSE,
      multiple = TRUE,
      description = stringr::str_c(
        "This is an optional field. This field accepts comma separated ",
        "multiple values for Lease Status Type IDs."
      )
    ),
    "leaseIds" = list(
      type = "integer",
      required = FALSE,
      multiple = TRUE,
      description = stringr::str_c(
        "This is an optional field. This field accepts comma separated ",
        "multiple values for Lease IDs."
      )
    ),
    "scheduledArCodeIds" = list(
      type = "integer",
      required = FALSE,
      multiple = TRUE,
      description = stringr::str_c(
        "This is an optional field. This field accepts comma separated ",
        "multiple values for Scheduled AR Code IDs."
      )
    ),
    "unitNumber" = list(
      type = "string",
      required = FALSE,
      description = stringr::str_c(
        "This is an optional field. This field accepts a single value for ",
        "the Unit Number."
      )
    ),
    "buildingName" = list(
      type = "string",
      required = FALSE,
      description = stringr::str_c(
        "This is an optional field. This field accepts a single value for ",
        "the Building Name."
      )
    ),
    "moveInDateFrom" = list(
      type = "date",
      required = FALSE,
      description = stringr::str_c(
        "This is an optional field. This field accepts a single value for ",
        "the start Move-in Date."
      )
    ),
    "moveInDateTo" = list(
      type = "date",
      required = FALSE,
      description = stringr::str_c(
        "This is an optional field. This field accepts a single value for ",
        "the end Move-in Date."
      )
    ),
    "leaseExpiringDateFrom" = list(
      type = "date",
      required = FALSE,
      description = stringr::str_c(
        "This is an optional field. This field accepts a single value for ",
        "the Lease Expiring Date (start)."
      )
    ),
    "leaseExpiringDateTo" = list(
      type = "date",
      required = FALSE,
      description = stringr::str_c(
        "This is an optional field. This field accepts a single value for ",
        "the Lease Expiring Date (end)."
      )
    ),
    "moveOutDateFrom" = list(
      type = "date",
      required = FALSE,
      description = stringr::str_c(
        "This is an optional field. This field accepts a single value for ",
        "the Move-out Date (start)."
      )
    ),
    "moveOutDateTo" = list(
      type = "date",
      required = FALSE,
      description = stringr::str_c(
        "This is an optional field. This field accepts a single value for ",
        "the Move-out Date (end)."
      )
    ),
    "includeOtherIncomeLeases" = list(
      type = "boolean_string",
      required = FALSE,
      description = stringr::str_c(
        "This is an optional field. This field accepts a single value. If ",
        "provided, this will include leases with other income."
      )
    ),
    "residentFriendlyMode" = list(
      type = "boolean_string",
      required = FALSE,
      description = stringr::str_c(
        "This is an optional field. This field accepts a single value. If ",
        "provided, this will use Resident Friendly Mode."
      )
    ),
    "includeLeaseHistory" = list(
      type = "boolean_string",
      required = FALSE,
      description = stringr::str_c(
        "This is an optional field. This field accepts a single value. If ",
        "provided, lease history will be included."
      )
    ),
    "includeArTransactions" = list(
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
    "propertyId" = list(
      type = "string",
      required = TRUE,
      description = stringr::str_c(
        "This is a required field. This field accepts a single value for ",
        "the Property ID."
      )
    ),
    "leaseId" = list(
      type = "integer",
      required = TRUE,
      description = stringr::str_c(
        "This is a required field. This field accepts a single value for ",
        "the Lease ID."
      )
    ),
    "leaseStatusTypeIds" = list(
      type = "integer",
      required = FALSE,
      multiple = TRUE,
      description = stringr::str_c(
        "This is an optional field. This field accepts comma separated ",
        "multiple values for Lease Status Type IDs."
      )
    ),
    "includeAddOns" = list(
      type = "boolean_string",
      required = FALSE,
      description = stringr::str_c(
        "This is an optional field. This field accepts a single value. If ",
        "provided, it will include lease add-ons."
      )
    ),
    "includeCharge" = list(
      type = "boolean_string",
      required = FALSE,
      description = stringr::str_c(
        "This is an optional field. This field accepts a single value. If ",
        "provided, it will include charges."
      )
    )
  ),
  "getLeaseDocuments" = list(
    "propertyId" = list(
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
