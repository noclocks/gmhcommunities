
#  ------------------------------------------------------------------------
#
# Title : General Utilities
#    By : Jimmy Briggs
#  Date : 2024-12-04
#
#  ------------------------------------------------------------------------


# operators ---------------------------------------------------------------

#' NULL Coalescing Operator
#'
#' @description
#' This function is a wrapper for the NULL coalescing operator:
#' If x if `NULL`, return y, else return x.
#'
#' @param x, y Values to compare.
#'
#' @keywords internal
"%||%" <- function(x, y) {
  if (is.null(x)) y else x
}



# modify_list -------------------------------------------------------------

#' Modify a List
#'
#' @description
#' This function contains the same functionality as [utils::modifyList()] but
#' is more explicit in its purpose.
#'
#' @param old,new lists
#'
#' @return updated list
#'
#' @seealso [utils::modifyList()]
#'
#' @export
modify_list <- function(old, new) {
  for (nm in names(new)) old[[nm]] <- new[[nm]]
  old
}


# pkg_sys -----------------------------------------------------------------

#' Package System File
#'
#' @description
#' This function is a wrapper for [base::system.file()] that allows for
#' easier access to package files.
#'
#' @param ... Path to file.
#'
#' @return Path to file.
#'
#' @seealso [base::system.file()]
#'
#' @export
pkg_sys <- function(...) {
  system.file(..., package = "gmhcommunities")
}


# messages ----------------------------------------------------------------

#' Messages
#'
#' @description
#' This function is a wrapper for [cli::cli_alert()] to provide consistent
#' console messaging.
#'
#' @param msg Message to display.
#' @param type Type of message: "info", "warning", or "error".
#'
#' @seealso [cli::cli_alert()]
#'
#' @importFrom cli cli_alert_info cli_alert_warning cli_alert_danger
cli_msg <- function(msg, type = "info", .log = FALSE) {
  switch(
    type,
    "info" = cli::cli_alert_info(msg),
    "success" = cli::cli_alert_success(msg),
    "warning" = cli::cli_alert_warning(msg),
    "error" = cli::cli_alert_danger(msg)
  )
}

# try_catch_alert ---------------------------------------------------------

#' try_catch <- function(expr, error_value = NULL, log_file = NULL, ...) {
#'
#'   requireNamespace("cli", quietly = TRUE)
#'   if (!is.null(log_file)) {
#'     requireNamespace("logger", quietly = TRUE)
#'     logger::log_appender(logger::appender_file(log_file))
#'   }
#'
#'   start_time <- Sys.time()
#'
#'   result <- tryCatch({
#'     cli::cli_alert_info("Starting execution...")
#'     logger::log_info("Starting execution...")
#'
#'     value <- eval(expr)
#'
#'     cli::cli_alert_sc
#'   })
#'
#' }
#'
#' #' Try-Catch-Alert
#' #'
#' #' @description
#' #' [base::tryCatch()] with an alert message via [shinyalert::shinyalert()].
#' #'
#' #' @param expr An expression to evaluate.
#' #' @param input_id The input ID of the text.
#' #' @inheritParams shinyalert::shinyalert
#' #'
#' #' @importFrom shiny updateTextInput
#' #' @importFrom shinyalert shinyalert
#' try_catch_alert <- function(expr, input_id, type = "error") {
#'
#'   success_alert_id <- paste0("shinyalert_success_", input_id)
#'   warning_alert_id <- paste0("shinyalert_warning_", input_id)
#'   error_alert_id <- paste0("shinyalert_error_", input_id)
#'
#'   tryCatch({
#'
#'     expr
#'
#'     shinyalert::shinyalert(
#'       title = "Success!",
#'       type = "success",
#'       inputId = success_alert_id
#'     )
#'
#'   },
#'   warning = function(w) {
#'     shinyalert::shinyalert(
#'       title = "Warning!",
#'       text = w$message,
#'       type = "error",
#'       inputId = warning_alert_id
#'     )
#'   },
#'   error = function(e) {
#'     shinyalert::shinyalert(
#'       title = "Error!",
#'       text = e$message,
#'       type = "error",
#'       inputId = error_alert_id
#'     )
#'   },
#'   finally = {
#'     shiny::updateTextInput(
#'       session = shiny::getDefaultReactiveDomain(),
#'       inputId = input_id,
#'       value = ""
#'     )
#'   })
#'
#' }

