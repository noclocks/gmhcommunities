statuses <- c(
  32:34,
  41:44
)

base_request <- entrata_request() |>
  entrata_req_endpoint("reports") |>
  httr2::req_body_json(
    list(
      auth = list(type = "basic"),
      method = list(
        name = "getReportData",
        version = "r3",
        params = list(
          reportName = "pre_lease",
          reportVersion = "3.2",
          filters = list(
            property_group_ids = as.list(as.character(properties$property_id)),
            period = list(
              date = report_date,
              period_type = "date"
            ),
            summarize_by = "unit_type",
            group_by = "unit_type",
            consider_pre_leased_on = status,  # Single value for each request
            charge_code_detail = 0,
            space_options = "do_not_show",
            additional_units_shown = "available",
            combine_unit_spaces_with_same_lease = 0,
            consolidate_by = "no_consolidation",
            arrange_by_property = 0,
            subtotals = list("summary", "details"),
            yoy = 1
          )
        )
      )
    )
  )

httr2::req_perform_parallel()
