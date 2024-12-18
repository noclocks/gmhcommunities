
#  ------------------------------------------------------------------------
#
# Title : {{title}}
#    By : {{author}}
#  Date : {{date}}
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' {{title}}
#'
#' @name mod_{{name}}
#'
#' @description
#' A Shiny Module that ...
#'
#' - `mod_{{name}}_ui()`: The front-end UI for the module.
#' - `mod_{{name}}_server()`: The server logic for the module.
#' - `mod_{{name}}_demo()`: Function to demo the module in an isolated shiny app context.
#'
#' @param id The module's ID that will be namespaced.
#'
#' @return
#' - `mod_{{name}}_ui()`: UI HTML Output.
#' - `mod_{{name}}_server()`: List of reactive values returned from server logic.
#' - `mod_{{name}}_demo()`: `NULL` (runs a shiny app).
#'
#' @examples
#' if (interactive()) {
#'   mod_{{name}}_demo()
#' }
NULL


# UI ----------------------------------------------------------------------

#' @rdname mod_{{name}}
#' @export
#' @importFrom shiny NS
#' @importFrom htmltools tagList tags
#' @importFrom bslib card
mod_{{name}}_ui <- function(id) {

  ns <- shiny::NS(id)

  htmltools::tagList(
    bslib::card(

    )
  )
}


# server ------------------------------------------------------------------

#' @rdname mod_{{name}}
#' @export
#' @importFrom shiny moduleServer reactive
#' @importFrom cli cat_rule
mod_{{name}}_server <- function(id, pool = NULL, ...) {

  if (!is.null(pool)) check_db_conn(pool)

  cli::cat_rule("[Module]: mod_{{name}}_server()")

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      return(
        list(
          # x = shiny::reactive({ ... }),
        )
      )

    }
  )
}


# demo --------------------------------------------------------------------

#' @rdname mod_{{name}}
#' @export
#' @importFrom pkgload load_all
#' @importFrom bslib page_navbar nav_panel
#' @importFrom bsicons bs_icon
#' @importFrom shiny shinyApp
mod_{{name}}_demo <- function() {

  pkgload::load_all()

  ui <- bslib::page_navbar(
    title = "Demo: {{title}}",
    window_title = "Demo: {{title}}",
    theme = app_theme(),
    lang = "en",
    bslib::nav_spacer(),
    bslib::nav_panel(
      title = "{{title}}",
      value = "{{name}}",
      icon = bsicons::bs_icon("house"),
      mod_{{name}}_ui("demo")
    )
  )

  server <- function(input, output, session) {
    mod_{{name}}_server("demo")
  }

  shiny::shinyApp(ui, server)
}
