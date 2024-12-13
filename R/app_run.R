
#  ------------------------------------------------------------------------
#
# Title : Run Shiny App
#    By : Jimmy Briggs
#  Date : 2024-12-04
#
#  ------------------------------------------------------------------------

#' Run Shiny App
#'
#' @param ui Shiny App UI
#' @param server Shiny App Server
#'
#' @return [shiny::shinyApp()]
#'
#' @export
#'
#' @family App
#'
#' @importFrom shiny shinyApp
run_app <- function(
    ui = app_ui,
    server = app_server,
    cache = app_cache()
) {

  if (!is.null(cache)) {
    shiny::shinyOptions(
      cache = cache
    )
  }

  shiny::shinyApp(
    ui = ui,
    server = server
  )

}


# cache -------------------------------------------------------------------

#' Setup Shiny App Cache

app_cache <- function(type = c("memory", "disk")) {

  rlang::arg_match(type)

  switch(
    type,
    "memory" = cachem::cache_mem(
      logfile = "cache.log",
    ),
    "disk" = cachem::cache_disk(
      dir = "cache",
      read_fn = qs2::qs_read,
      write_fn = qs2::qs_save,
      extension = ".qs",
      logfile = "cache.log"
    )
  )

}
