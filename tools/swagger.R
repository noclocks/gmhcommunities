require(swagger)

swagger_viewer <- function(apispec, viewer = c("rstudio", "browser")) {

  if (!fs::is_file(apispec) && !fs::path_ext(apispec) %in% c(".json", ".yaml", ".yml")) {
    cli::cli_abort(
      "{.arg apispec} must be a valid file path to an OpenAPI specification file..."
    )
  }

  viewer <- match.arg(viewer)

  temp_dir <- fs::path_temp("swagger")
  fs::dir_create(temp_dir)
  fs::dir_copy(swagger::swagger_path(), temp_dir, overwrite = TRUE)
  index_html <- xfun::read_utf8(fs::path(temp_dir, "index.html"))
  index_html_updated <- gsub(
    "https://petstore.swagger.io/v2/swagger.json",
    fs::path_abs(apispec),
    index_html
  )
  xfun::write_utf8(index_html_updated, fs::path(temp_dir, "index.html"))

  if (viewer == "browser") {
    browseURL(fs::path(temp_dir, "index.html"))
  } else {
    rstudioapi::viewer(fs::path(temp_dir, "index.html"))
  }

}
