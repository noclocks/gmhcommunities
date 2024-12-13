#  ------------------------------------------------------------------------
#
# Title : Package Development Script
#  File : pkg_devt.R
#    By : Jimmy Briggs <jimmy.briggs@noclocks.dev>
#  Date : 2024-11-19
#
#  ------------------------------------------------------------------------


# libraries --------------------------------------------------------------

require(devtools)
require(usethis)
require(pkgload)
require(pkgbuild)
require(attachment)
require(knitr)
require(rmarkdown)

# data-raw ---------------------------------------------------------------

c(
  "internal",
  "exported"
) |>
  purrr::walk(usethis::use_data_raw)


# functions --------------------------------------------------------------


# modules -----------------------------------------------------------------

use_module("market_property_summary")

