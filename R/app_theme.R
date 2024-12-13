
#  ------------------------------------------------------------------------
#
# Title : Shiny App Theme
#    By : Jimmy Briggs
#  Date : 2024-12-13
#
#  ------------------------------------------------------------------------

#' App Theme
#'
#' @description
#' Custom Bootstrap theme for the Shiny app.
#'
#' @details
#' This function defines a custom Bootstrap theme for the Shiny app
#' using [bslib::bs_theme()].
#'
#' @return A Bootstrap theme object.
#'
#' @export
#'
#' @importFrom bslib bs_theme
app_theme <- function() {

  bslib::bs_theme(
    version = 5,
    primary = "#0e2b4c",
    "navbar-bg" = "#ffffff"
  )

}
