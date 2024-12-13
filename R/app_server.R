
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
#' @return Shiny Server
#'
#' @export
#'
#' @family App
#'
#' @importFrom shiny observe observeEvent reactive
app_server <- function(input, output, session) {

  # initialize session$userData for authentication
  # session$userData$user <- function() NULL
  # session$userData$cookie <- NULL
  #
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

}
