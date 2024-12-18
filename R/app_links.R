
app_link <- function(
  name,
  url,
  icon = shiny::icon("world"),
  target = "_blank"
) {

  if (is.character(icon)) {
    icon <- shiny::icon(icon)
  }

  htmltools::tags$a(
    icon,
    name,
    href = url,
    target = target
  )

}

link_github <- app_link("GitHub", "https://github.com/noclocks/gmhdatahub", icon = shiny::icon("github"))
link_github
