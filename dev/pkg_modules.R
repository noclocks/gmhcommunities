
#  ------------------------------------------------------------------------
#
# Title : Package Shiny Modules
#    By : Jimmy Briggs
#  Date : 2024-12-06
#
#  ------------------------------------------------------------------------


# source ------------------------------------------------------------------

source("dev/R/use_template.R")

# layout modules ----------------------------------------------------------

use_module("title")
use_module("sidebar")
use_module("footer")
use_module("user_profile")
use_module("dashboard")

# utility modules ---------------------------------------------------------

use_module("excel_report")

# leasing & pre-lease -----------------------------------------------------

use_module("pre_lease")
use_module("pre_lease_summary")
use_module("pre_lease_details")

# entrata entities --------------------------------------------------------

use_module("properties")
use_module("property_units")
use_module("leases")
use_module("lease_terms")
use_module("lease_renewals")

# market survey modules ---------------------------------------------------

use_module("market_survey")
use_module("market_survey_overview")
use_module("market_survey_property_summary")
use_module("market_survey_leasing_summary")
use_module("market_survey_amenities")
use_module("market_survey_utilities")
use_module("market_survey_rents")
use_module("market_survey_fees")
use_module("market_survey_short_term_leases")
use_module("market_survey_parking")
use_module("market_survey_notes")
use_module("market_survey_hours")

# market survey insights modules ------------------------------------------
use_module("survey_insights")
use_module("survey_insights_comparison")
use_module("survey_insights_results")
use_module("survey_insights_history")
use_module("survey_insights_trends")
use_module("survey_insights_swot")
