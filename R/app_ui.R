
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
#' @returns [htmltools::tagList()] containing the core user interface elements.
#'
#' @seealso [app_server()]
#'
#' @export
#'
#' @importFrom bsicons bs_icon
#' @importFrom bslib page_fluid bs_theme page_navbar nav_spacer nav_panel nav_menu nav_item
#' @importFrom htmltools tagList tags
#' @importFrom shiny icon textOutput actionLink
app_ui <- function(req) {

  force(req)

  htmltools::tagList(
    add_external_resources(),
    bslib::page_navbar(
      id = "nav",
      lang = "en",
      window_title = "GMH DataHub",
      position = "fixed-top",
      theme = app_theme(),
      title = app_title_ui(),
      # footer = app_footer_ui(),
      # sidebar = mod_sidebar_ui("app"),
      # navigation - page modules
      bslib::nav_spacer(),
      # home page
      bslib::nav_panel(
        title = "Home",
        value = "home",
        icon = bsicons::bs_icon("house"),
        mod_home_ui("home")
      ),
      # dashboard
      bslib::nav_panel(
        title = "Dashboard",
        value = "dashboard",
        icon = bsicons::bs_icon("speedometer2"),
        mod_dashboard_ui("app")
      ),
      # data (menu)
      bslib::nav_menu(
        title = "Data",
        value = "data",
        icon = bsicons::bs_icon("database"),
        # pre_lease
        bslib::nav_panel(
          title = "Pre Lease",
          value = "pre_lease",
          icon = bsicons::bs_icon("file-check"),
          mod_pre_lease_ui("app")
        ),
        # properties
        bslib::nav_panel(
          title = "Properties",
          value = "properties",
          icon = bsicons::bs_icon("building"),
          mod_properties_ui("app")
        )
      ),
      # market survey
      bslib::nav_panel(
        title = "Market Survey",
        value = "market_survey",
        icon = bsicons::bs_icon("file-earmark-bar-graph"),
        mod_market_survey_ui("app")
      ),
      # survey insights
      bslib::nav_panel(
        title = "Survey Insights",
        value = "survey_insights",
        icon = bsicons::bs_icon("lightbulb"),
        mod_survey_insights_ui("app")
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
        icon = bsicons::bs_icon("link-45deg"),
        bslib::nav_item(
          bslib::nav_item(
            htmltools::tags$a(
              shiny::icon("book"), "Documentation",
              href = app_info$docs_url,
              target = "_blank"
            )
          ),
          htmltools::tags$a(
            shiny::icon("github"), "GitHub",
            href = app_info$repo_url,
            target = "_blank"
          )
        )
      ),
      bslib::nav_menu(
        title = "Contact",
        align = "right",
        icon = bsicons::bs_icon("envelope"),
        bslib::nav_item(
          htmltools::tags$a(
            shiny::icon("envelope"),
            "Email Support",
            href = "mailto:support@noclocks.dev",
            target = "_blank"
          )
        )
      ),
      bslib::nav_menu(
        title = "User",
        align = "right",
        icon = bsicons::bs_icon("person-circle"),
        bslib::nav_item(
          htmltools::tags$a(
            shiny::icon("user"),
            shiny::textOutput("signed_in_as"),
            href = "#"
          )
        ),
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

}

# demo --------------------------------------------------------------------

#' @importFrom shiny shinyApp
app_ui_demo <- function() {

  shiny::shinyApp(
    ui = app_ui,
    server = app_server
  )

}
