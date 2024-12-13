
#  ------------------------------------------------------------------------
#
# Title : Shiny App Assets
#    By : Jimmy Briggs
#  Date : 2024-11-29
#
#  ------------------------------------------------------------------------

# topic -----------------------------------------------------------

#' App Assets
#'
#' @name app_assets
#'
#' @description
#' Various functions for including, bundling, and managing Shiny App static assets:
#'
#' - `app_styles()`: Add CSS styles to the Shiny application.
#' - `compile_styles()`: Compile SCSS styles into CSS for the Shiny application.
#' - `app_favicon()`: Add a favicon to the Shiny application.
#' - `add_resource_path()`: Add a resource path to the Shiny application.
#' - `add_external_resources()`: Add external resources to the Shiny application.
#'
#' @family App
#'
#' @param path Path to the asset file, resource directory, favicon, etc.
#'   Defaults are provided for each function.
#' @param prefix Prefix for the resource path. Defaults to `"www"`.
#' @param overwrite Logical. Should the resource path be overwritten if it already exists?
#'   Defaults to `TRUE`.
#' @param scss_path Path to the SCSS file. Defaults to `"www/styles/scss/index.scss"`.
#' @param css_path Path to the CSS file. Defaults to `"www/styles/css/styles.min.css"`.
#' @param minify Logical. Should the CSS be minified? Defaults to `TRUE`.
#'
#' @return
#' - `app_styles()`: HTML `<head>` tag with a link to the CSS file.
#' - `compile_styles()`: Compiled CSS file.
#' - `app_favicon()`: HTML `<head>` tag with a link to the favicon.
#' - `add_resource_path()`: `NULL`, used for its side effect of adding a resource path.
#' - `add_external_resources()`: HTML `<head>` tags with links to external resources.
NULL

# styles -----------------------------------------------------------

#' @rdname app_assets
#' @export
#' @importFrom htmltools tags
app_styles <- function(
  path = "www/styles.min.css"
) {
  htmltools::tags$head(
    htmltools::tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = path
    )
  )
}

#' @rdname app_assets
#' @export
#' @importFrom sass sass sass_file sass_options_get sass_options_set
#' @importFrom cli cli_abort cli_bullets cli_alert_info cli_alert_success
compile_styles <- function(
    input_scss = "www/styles/scss/index.scss",
    output_css = "www/styles/css/styles.min.css",
    minify = TRUE,
    ...
) {

  if (!dir.exists(scss_path)) {
    cli::cli_abort("The specified path does not exist: {.path {input_scss}}.")
  }

  if (!dir.exists(dirname(output_css))) {
    fs::dir_create(dirname(output_css))
  }

  if (minify) {
    default_opts <- sass::sass_options_get()
    sass::sass_options_set(output_style = "compressed")
    on.exit(sass::sass_options_set(default_opts))
  }

  output <- sass::sass(
    input = sass::sass_file(input_scss),
    options = sass::sass_options_get(),
    output = output_css,
    write_attachments = FALSE,
    cache = NULL,
    cache_key_extra = NULL
  )

  cli::cli_bullets(
    c(
      "v" = "Successfully compiled SASS styles.",
      "i" = "Input: {.file {input_scss}}",
      "i" = "Output: {.file {output_css}}",
      "i" = "Access compiled styles via {.code {app_styles()}}."
    )
  )

  return(invisible(output))

}


# scripts -----------------------------------------------------------------

#' @rdname app_assets
#' @export
app_js <- function(
  path = "www/scripts/js"
) {

  js_scripts <- fs::dir_ls(path, type = "file", glob = "*.js")

  htmltools::tags$head(
    htmltools::tags$script(
      src = js_scripts
    )
  )

}

# favicon -----------------------------------------------------------

#' @rdname app_assets
#' @export
#' @importFrom mime guess_type
#' @importFrom htmltools tags
app_favicon <- function(
  path = "www/favicon.ico"
) {

  requireNamespace("mime", quietly = TRUE)

  favicon_mime_type <- mime::guess_type(
    path,
    mime_extra = c("ico" = "image/x-icon")
  )

  htmltools::tags$head(
    htmltools::tags$link(
      rel = "icon",
      type = favicon_mime_type,
      href = path
    )
  )

}


# pre-loader --------------------------------------------------------------

app_preloader_ui <- function() {
  list(
    html = waiter::spin_3k(),
    color = "#0E2B4C"
  )
}


# resource path -----------------------------------------------------------

#' @rdname app_assets
#' @export
#' @importFrom shiny addResourcePath
#' @importFrom cli cli_abort cli_alert_info cli_alert_success
add_resource_path <- function(prefix = "www", path = pkg_sys("www"), overwrite = TRUE) {
  if (!dir.exists(path)) {
    cli::cli_abort("The specified path does not exist: {.path {path}}.")
  }

  existing_paths <- shiny::resourcePaths()

  if (prefix %in% names(existing_paths)) {
    if (!overwrite) {
      cli::cli_alert_info("Resource path '{.field {prefix}}' already exists and {code overwrite=FALSE}. Skipping...")
      return(invisible(NULL))
    }
    cli::cli_alert_info("Overwriting existing resource path: {.field {prefix}} -> {.path {path}}")
    shiny::removeResourcePath(prefix)
  }

  shiny::addResourcePath(prefix, path)
  cli::cli_alert_success("Added resource path: {.field {prefix}} -> {.path {path}}")
  return(invisible(NULL))
}


# external resources ------------------------------------------------------

#' @rdname app_assets
#' @export
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyWidgets useSweetAlert
#' @importFrom waiter use_waiter
#' @importFrom htmltools tags
add_external_resources <- function() {

  add_resource_path("www", pkg_sys("www"))

  htmltools::tags$head(
    # packages
    shinyjs::useShinyjs(),
    shinyWidgets::useSweetAlert(),
    waiter::use_waiter(),
    rintrojs::introjsUI(),
    # favicon
    app_favicon(),
    # htmltools::tags$link(rel = "icon", type = "image/x-icon", href = "favicon.ico"),
    # css
    # htmltools::tags$link(rel = "stylesheet", type = "text/css", href = "www/styles.min.css"),
    # js
    # htmltools::tags$script(src = "www/scripts.min.js")
  )

}

# bundle ------------------------------------------------------------------


# bundle_assets <- function(
#     path = pkg_sys("www"),
#     ...
# ) {
#
#   # see what folders are available at path
#   path_dirs <- basename(fs::dir_ls(path, type = "dir"))
#
#   # check for styles folder
#   if ("styles" %in% path_dirs) {
#     cli::cli_alert_info("Found {.field styles} folder: {.path {pkg_sys('www', 'styles')}}")
#     # check for scss folder
#     if ("scss" %in% path_dirs) {
#       # check for index.scss file
#       if ("index.scss" %in% fs::dir_ls(fs::path(path, "styles", "scss"))) {
#         # compile styles
#         compile_styles(
#           input_scss = fs::path(path, "styles", "scss", "index.scss"),
#           output_css = fs::path(path, "styles", "css", "styles.min.css")
#         )
#       } else {
#         cli::cli_alert_info("No 'index.scss' file found.")
#       }
#     } else {
#       cli::cli_alert_info("No 'scss' folder found.")
#     }
#   } else {
#     cli::cli_alert_info("No 'styles' folder found.")
#   }
#
#   # check for scripts folder
#   if ("scripts" %in% path_dirs) {
#     cli::cli_alert_info("Found {.field scripts} folder: {.path {pkg_sys('www', 'scripts')}}")
#   } else {
#     cli::cli_alert_info("No 'scripts' folder found.")
#   }
