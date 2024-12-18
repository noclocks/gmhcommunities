
#  ------------------------------------------------------------------------
#
# Title : App Title Module
#    By : Jimmy Briggs
#  Date : 2024-11-08
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' Title Module
#'
#' @name mod_title
#'
#' @description
#' A module to display the title of the application.
#'
#' The module implements the following functions:
#' - `mod_title_ui()`: User interface for displaying the title.
#' - `mod_title_server()`: Server logic for the title module.
#'
#' @details
#' This module provides a simple user interface for displaying the title of the
#' application. The title is typically displayed in the top-left corner of the
#' application and may include a logo or other branding elements.
#'
#' @param id The ID of the module.
#' @param app_info An optional list containing information about the application.
#'   If left `NULL`, the function will use internally packaged app metadata.
#'
#' @section App Info:
#'
#' The `app_info` parameter is a list containing the following elements:
#'
#' - `name`: The name of the application.
#' - `version`: The version of the application.
#' - `logo`: The path or URL to the application logo.
#' - `symbol`: The path or URL to the application symbol or icon.
#' - `repo_url`: The URL to the application's GitHub repository.
#' - `docs_url`: The URL to the application's documentation.
#'
#' @return
#' - `mod_title_ui()`: A UI component for displaying the title.
#' - `mod_title_server()`: Server logic for the title module.
#'
#' @seealso [app_ui()]
NULL


# UI ----------------------------------------------------------------------

#' @rdname mod_title
#' @export
#' @importFrom shiny NS
#' @importFrom utils getFromNamespace
#' @importFrom rlang pkg_env
#' @importFrom htmltools tags
mod_title_ui <- function(
    id,
    app_info = NULL
) {

  ns <- shiny::NS(id)

  if (is.null(app_info)) { app_info <- gmhcommunities:::app_info }

  htmltools::tags$div(
    id = ns("title"),
    class = "navbar-brand",
    htmltools::tags$a(
      href = "#",
      htmltools::tags$img(
        src = app_info$logo,
        alt = app_info$name,
        height = 50,
        width = "auto"
      )
    )
  )

}


# server ------------------------------------------------------------------

#' @rdname mod_title
#' @export
#' @importFrom shiny moduleServer
#' @importFrom cli cat_rule
mod_title_server <- function(id, ...) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      cli::cat_rule("mod_title_server")

      # No server logic needed

    }
  )
}

# demo --------------------------------------------------------------------

#' @keywords internal
#' @noRd
#' @importFrom shiny shinyApp addResourcePath
#' @importFrom bslib page_navbar bs_theme
mod_title_demo <- function() {

  shiny::addResourcePath("www", pkg_sys("www"))

  ui <- bslib::page_navbar(
    header = htmltools::tags$head(
      add_external_resources()
    ),
    title = mod_title_ui("title"),
    theme = bslib::bs_theme(version = 5),
    window_title = "GMH DataHub",
    lang = "en"
  )

  server <- function(input, output, session) {
    mod_title_server("title")
  }

  shiny::shinyApp(ui, server)
}

