
#  ------------------------------------------------------------------------
#
# Title : Shiny App UI
#    By : Jimmy Briggs
#  Date : 2024-11-08
#
#  ------------------------------------------------------------------------

# UI ----------------------------------------------------------------------

#' Core Shiny App User Interface
#'
#' @name app_ui
#'
#' @family App
#'
#' @description
#' This function generates the core user interface for the GMH DataHub Shiny app.
#'
#' @param req (Internal) The initial HTTP request object.
#'
#' @return [htmltools::tagList()] containing the core user interface elements.
#'
#' @seealso [app_server()]
#'
#' @export
#'
#' @importFrom bsicons bs_icon
#' @importFrom bslib page_navbar bs_theme nav_spacer nav_panel nav_menu nav_item
#' @importFrom htmltools tagList tags
#' @importFrom rlang pkg_env
#' @importFrom shiny icon textOutput actionLink
app_ui <- function(req) {

  force(req)

  htmltools::tagList(
    add_external_resources(),
    bslib::page_fluid(
      # theme
      theme = bslib::bs_theme(version = 5),
      bslib::page_navbar(
        id = "nav",
        lang = "en",
        window_title = "GMH DataHub",
        position = "fixed-top",
        # header
        # header = htmltools::tagList(),
        # layout modules
        title = mod_title_ui("app"),
        footer = mod_footer_ui("app"),
        sidebar = mod_sidebar_ui("app"),
        # navigation - page modules
        bslib::nav_spacer(),
        # dashboard
        bslib::nav_panel(
          title = "Dashboard",
          value = "dashboard",
          icon = bsicons::bs_icon("speedometer"),
          # mod_dashboard_ui("app")
          bslib::card(
            "DASHBOARD"
          )
        ),
        # data (menu)
        bslib::nav_menu(
          title = "Data",
          value = "data",
          icon = bsicons::bs_icon("database"),
          # properties
          bslib::nav_panel(
            title = "Properties",
            value = "properties",
            icon = bsicons::bs_icon("building")#,
            # mod_properties_ui("app")
          ),
          bslib::nav_panel(
            title = "Leasing",
            value = "leasing",
            icon = bsicons::bs_icon("file-check")#,
            # mod_leasing_ui("app")
          )
        ),
        # market survey
        bslib::nav_panel(
          title = "Market Survey",
          value = "market_survey",
          icon = bsicons::bs_icon("file-earmark-bar-graph")#,
          # mod_market_survey_ui("app")
        ),
        # survey insights
        bslib::nav_panel(
          title = "Survey Insights",
          value = "survey_insights",
          icon = bsicons::bs_icon("lightbulb")#,
          # mod_survey_insights_ui("app")
        ),
        bslib::nav_spacer(),
        bslib::nav_menu(
          title = "Admin",
          align = "right",
          bslib::nav_panel(
            title = "Settings",
            value = "settings",
            icon = shiny::icon("cogs")#,
            # mod_settings_ui("app")
          ),
          bslib::nav_panel(
            title = "Logs",
            value = "logs",
            icon = shiny::icon("clipboard-list")#,
            # mod_logs_ui("app")
          )
        ),
        bslib::nav_menu(
          title = "Links",
          align = "right",
          bslib::nav_item(
            htmltools::tags$a(
              shiny::icon("github"), "GitHub",
              href = app_info$repo_url,
              target = "_blank"
            )
          ),
          bslib::nav_item(
            htmltools::tags$a(
              shiny::icon("book"), "Documentation",
              href = app_info$docs_url,
              target = "_blank"
            )
          ),
          bslib::nav_item(
            htmltools::tags$a(
              shiny::icon("envelope"), "Contact",
              href = "#",
              target = "_blank"
            )
          )
        ),
        bslib::nav_menu(
          title = "Logout",
          align = "right",
          icon = bsicons::bs_icon("box-arrow-right"),
          shiny::textOutput("signed_in_as"),
          bslib::nav_item(
            shiny::actionLink(
              inputId = "auth_logout",
              label = "Logout",
              icon = shiny::icon("sign-out-alt"),
              style = "display: inline-flex; align-items: center; padding: 2.5px 50px; width: -webkit-fill-available;"
            )
          )
        )
      )
    )
  )

}

# demo --------------------------------------------------------------------

app_ui_demo <- function() {

  shiny::shinyApp(
    ui = app_ui,
    server = function(input, output, session) {

    }
  )

}


