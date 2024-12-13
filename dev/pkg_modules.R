
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

# utility modules ---------------------------------------------------------

use_module("excel_report")


# entrata entities --------------------------------------------------------

use_module("entrata")
use_module("entrata_properties")
use_module("entrata_property_units")
use_module("entrata_leases")
use_module("entrata_lease_terms")
use_module("entrata_lease_renewals")

# leasing & pre-lease -----------------------------------------------------

use_module("leasing")
use_module("prelease")

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
