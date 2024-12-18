
#  ------------------------------------------------------------------------
#
# Title : Shiny App Layout Components - Title
#    By : Jimmy Briggs
#  Date : 2024-12-14
#
#  ------------------------------------------------------------------------

layout_title <- function(app_info = NULL) {

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
