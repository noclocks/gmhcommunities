
#  ------------------------------------------------------------------------
#
# Title : App Server
#    By : Jimmy Briggs
#  Date : 2024-12-06
#
#  ------------------------------------------------------------------------


#' App Server
#'
#' @param input,output,session Default shiny server arguments.
#'
#' @returns Shiny Server
#'
#' @export
#'
#' @family App
#'
#' @importFrom shiny observe observeEvent reactive
app_server <- function(input, output, session) {

  # initialize session$userData for authentication
  session$userData$user <- function() {
    list(
      id = NULL,
      name = NULL,
      email = NULL,
      role = NULL
    )
  }
  session$userData$cookie <- NULL

  # initialize database connection pool
  pool <- db_connect()
  session$userData$pool <- pool

  # modules
  mod_home_data <- mod_home_server("home", pool = pool)
  mod_dashboard_data <- mod_dashboard_server("dashboard", pool = pool)

}


# shiny::observeEvent(input$hashed_cookie, {
#   hashed_cookie <- input$hashed_cookie
#   global_user <- NULL
#   query_list <- shiny::getQueryString()
#   page <- query_list$page
#   session_started <- TRUE
#   if (is.character(hashed_cookie) && nchar(hashed_cookie) == 32L) {
#     tryCatch({
#       app_session <- list(
#
#       )
#     }, error = function(e) {
#       global_user <- NULL
#     })
# })

# shiny::observeEvent(input$guide, {
#   rintrojs::introjs(
#     session,
#     events = list(
#       "oncomplete" = I('alert("That is all! I hope you enjoyed the tour! :)")'),
#       onbeforechange = rintrojs::readCallback("switchTabs")
#     )
#   )
# })
#
# output$excel_report <- shiny::downloadHandler(
#   filename = function() {
#     paste("report-", Sys.Date(), ".xlsx", sep = "")
#   },
#   content = function(file) {
#     openxlsx::write.xlsx(
#       x = mtcars,
#       file = file
#     )
#   }
# )
