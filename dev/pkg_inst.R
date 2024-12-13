
#  ------------------------------------------------------------------------
#
# Title : inst/ folder setup
#    By : Jimmy Briggs
#  Date : 2024-12-03
#
#  ------------------------------------------------------------------------



# database ----------------------------------------------------------------

# schemas & tables
c(
  "auth",
  "entrata",
  "gmh",
  "survey",
  "logs"
) |>
  purrr::walk(
    ~ fs::dir_create(
      path = here::here("inst", "database", "schemas", .x),
      recurse = TRUE
    )
  )

c(
  "auth.tenants.sql",
  "auth.users.sql",
  "auth.apps.sql",
  "auth.roles.sql",
  "auth.permissions.sql",
  "auth.sessions.sql",
  "auth.user_roles.sql",
  "auth.user_apps.sql",
  "auth.role_permissions.sql",
  "auth.api_keys.sql",
  "auth.verification_tokens.sql",
  "auth.password_resets.sql",
  "auth.emails.sql",
  "auth.email_templates.sql"
) |>
  purrr::walk(
    ~ fs::file_create(
      path = here::here("inst", "database", "schemas", "auth", .x)
    )
  )

"entrata.properties.sql",
"entrata.property_units.sql",
"entrata.property_addresses.sql",
"entrata.space_options.sql",
"entrata.floor_plans.sql",
"entrata.residents.sql",
"entrata.leases.sql",
"entrata.lease_terms.sql",
"entrata.lease_renewals.sql",
"entrata.lease_charges.sql",
"entrata.lease_periods.sql",
"entrata.lease_intervals.sql",
"entrata.lease_interval_windows.sql",
"entrata.lease_interval_window_units.sql",
"entrata.rooms.sql",
"entrata.unit_types.sql",
"entrata.pricing_tiers.sql",
"entrata.charge_codes.sql",
"entrata.reports.sql",
"entrata.report_pre_lease_summary.sql",
"entrata.report_pre_lease_details.sql",
"entrata.report_weekly_perforance.sql",
"entrata.report_filter_parameters.sql",
"entrata.report_box_score.sql",
"entrata.api_metadata.sql",

"gmh.properties.sql",
"gmh.property_locations.sql",
"gmh.property_types.sql",
"gmh.property_profiles.sql",
"gmh.property_images.sql",
"gmh.portfolios.sql",
"gmh.portfolio_assignments.sql",
"gmh.investment_partners.sql",
"gmh.reports.sql",
"gmh.summary_reports.sql",
"gmh.detail_reports.sql",

"survey.surveys.sql",
"survey.questions.sql",
"survey.answers.sql",
"survey.responses.sql",
"survey.responses_answers.sql",



)
