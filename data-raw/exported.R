#  ------------------------------------------------------------------------
#
# Title : Exported Data
#  File : exported.R
#    By : Jimmy Briggs <jimmy.briggs@noclocks.dev>
#  Date : 2024-11-23
#
#  ------------------------------------------------------------------------


# source("data-raw/src/excel.R")
# usethis::use_data(unit_types, summary_tbls, details_tbls, overwrite = TRUE)
# readr::write_csv(unit_types, "data-raw/working/unit_types.csv")
# readr::write_csv(summary_tbls, "data-raw/working/summary_tbls.csv")
# readr::write_csv(details_tbls, "data-raw/working/details_tbls.csv")

source("data-raw/src/partners.R")
source("data-raw/src/portfolios.R")
source("data-raw/src/properties.R")

usethis::use_data(portfolio_assignments, overwrite = TRUE)
usethis::use_data(portfolios, overwrite = TRUE)
usethis::use_data(investment_partners, overwrite = TRUE)
usethis::use_data(unit_types, overwrite = TRUE)
usethis::use_data(details_tbls, overwrite = TRUE)
usethis::use_data(summary_tbls, overwrite = TRUE)
