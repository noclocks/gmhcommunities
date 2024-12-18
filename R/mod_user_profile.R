
#  ------------------------------------------------------------------------
#
# Title : user_profile Shiny Module
#    By : Jimmy Briggs
#  Date : 2024-11-27
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' user_profile Shiny Module
#'
#' @name mod_user_profile
#'
#' @description
#' A Shiny Module for ...
#'
#' - `mod_user_profile_ui()`: User interface
#' - `mod_user_profile_server()`: Server logic
#'
#' @param id Module's namespace ID.
#'
#' @returns
#' - `mod_user_profile_ui()`: UI HTML Output.
#' - `mod_user_profile_server()`: Reactive values returned from server logic.
#'
#' @examplesIf interactive()
#' mod_user_profile_demo()
NULL


# UI ----------------------------------------------------------------------

#' @rdname mod_user_profile
#' @export
#' @importFrom bsicons bs_icon
#' @importFrom bslib accordion accordion_panel
#' @importFrom htmltools tags
#' @importFrom shiny NS textOutput
mod_user_profile_ui <- function(id) {

  ns <- shiny::NS(id)

  bslib::accordion(
    id = ns("accordion"),
    open = FALSE,
    bslib::accordion_panel(
      title = "User Profile",
      value = ns("user_profile"),
      icon = bsicons::bs_icon("person-circle"),
      htmltools::tags$div(
        class = "text-center",
        bsicons::bs_icon("person-circle", size = "2rem"),
        htmltools::tags$h5(
          shiny::textOutput(ns("user_full_name"), inline = TRUE)
        ),
        htmltools::tags$p(
          shiny::textOutput(ns("user_email"), inline = TRUE)
        ),
        htmltools::tags$p(
          shiny::textOutput(ns("user_role"), inline = TRUE)
        )
      )
    )
  )

}

# server ------------------------------------------------------------------

#' @rdname mod_user_profile
#' @export
#' @importFrom shiny moduleServer reactive renderText
#' @importFrom cli cat_rule
mod_user_profile_server <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      data <- shiny::reactive({
        list(
          user_full_name = "John Doe",
          user_email = "john.doe@gmhcommunities.com",
          user_role = "Administrator"
        )
      })
      output$user_full_name <- shiny::renderText({
        data()$user_full_name
      })
      output$user_email <- shiny::renderText({
        data()$user_email
      })
      output$user_role <- shiny::renderText({
        data()$user_role
      })
      return(data)
    }
  )
}



# demo --------------------------------------------------------------------

#' @rdname mod_user_profile
#' @export
#' @importFrom pkgload load_all
#' @importFrom bslib page_fluid bs_theme
#' @importFrom shiny shinyApp
mod_user_profile_demo <- function() {

  pkgload::load_all()

  ui <- bslib::page_fluid(
    title = "Demo",
    theme = bslib::bs_theme(version = 5),
    lang = "en",
    mod_user_profile_ui("demo")
  )

  server <- function(input, output, session) {
    mod_user_profile_server("demo")
  }

  shiny::shinyApp(ui, server)
}
