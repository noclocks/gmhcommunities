
#  ------------------------------------------------------------------------
#
# Title : Package Documentation
#    By : Jimmy Briggs
#  Date : 2024-12-10
#
#  ------------------------------------------------------------------------

require(usethis)
require(roxygen2)
require(pkgdown)
require(fs)

# logo --------------------------------------------------------------------

usethis::use_logo("inst/www/images/shared/logos/app-logo.svg")

# vignettes ---------------------------------------------------------------

usethis::use_vignette(name = "gmhcommunities", title = "GMH Communities")
usethis::use_vignette(name = "entrata", title = "Entrata API")
usethis::use_vignette(name = "pipeline", title = "Data Processing Pipeline")
usethis::use_vignette(name = "market_survey", title = "Leasing Market Survey")

fs::file_create("vignettes/compile.R")
fs::dir_create("vignettes/assets")

# articles ----------------------------------------------------------------

# usethis::use_article(name = "", title = "")

# pkgdown -----------------------------------------------------------------

usethis::use_pkgdown("_pkgdown.yml", "docs")
usethis::use_pkgdown_github_pages()
pkgdown::build_favicons(overwrite = TRUE)
pkgdown::build_site()

