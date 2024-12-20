
#  ------------------------------------------------------------------------
#
# Title : Shiny App Title UI Function
#    By : Jimmy Briggs
#  Date : 2024-11-08
#
#  ------------------------------------------------------------------------

#' Shiny App Title
#'
#' @family App
#' @family UI
#'
#' @description
#' Shiny UI function for generating the app's title.
#'
#' @details
#' This function provides a simple user interface for displaying the title of the
#' application. The title is typically displayed in the top-left corner of the
#' application and may include a logo or other branding elements.
#'
#' @seealso [app_ui()]
#'
#' @returns [htmltools::tags$div()] containing the app title.
#'
#' @export
#'
#' @importFrom htmltools tags
#' @importFrom bsicons bs_icon
app_title_ui <- function(title = "GMH Data Hub Platform") {

  htmltools::tags$div(
    class = "app-header-title",
    style = "display: flex; align-items: center;",
    htmltools::tags$a(
      href = "https://gmhcommunities.com",
      htmltools::tags$img(
        src = "www/images/gmh/logos/gmh-logo.svg",
        alt = "GMH Communities",
        width = "200px",
        height = "auto",
        style = "margin-top: 5px; margin-bottom: 5px; margin-right: 15px;"
      ),
      htmltools::tags$div(
        style = "display: flex; align-items: center;",
        bsicons::bs_icon("building", size = "1.5rem"),
        htmltools::tags$span(
          style = "margin-left: 10px; font-size: 1.2rem;",
          title
        )
      )
    )
  )

}
