
#  ------------------------------------------------------------------------
#
# Title : Market Survey Data Preparation
#    By : Jimmy Briggs
#  Date : 2024-12-03
#
#  ------------------------------------------------------------------------

# libraries ---------------------------------------------------------------
require(readxl)

# source ------------------------------------------------------------------

source("data-raw/R/market_survey.R")
source("data-raw/src/survey/excel_survey_sections.R")

# data --------------------------------------------------------------------

csv_path <- "data-raw/data/working/market_survey"

save_market_survey_csvs(
  data_by_property = market_survey_data_by_property,
  csv_dir = csv_path
)


# end ----------------------------------------------------------------------

cli::cli_alert_success(
  c(
    "Market Survey Data Preparation Successfully Completed."
  )
)
