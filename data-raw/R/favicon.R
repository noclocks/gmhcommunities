build_favicons <- function(
    image_path,
    output_dir = fs::path(getwd(), "inst", "favicon"),
    overwrite = FALSE,
    api_key = config::get("real_favicon_generator_api_key"),
    ios6_and_prior_icons = FALSE,
    ios7_and_later_icons = TRUE,
    precomposed_icons = FALSE,
    declare_only_default_icon = TRUE
) {

  rlang::check_installed("openssl")
  rlang::check_installed("httr2")

  cli::cli_rule("Building Favicons")

  if (!file.exists(image_path)) {
    cli::cli_abort("Image file not found at {.path {image_path}}")
  }

  if (!fs::is_dir(output_dir)) {
    cli::cli_abort("`output_dir` is not a directory: {.path {output_dir}}")
  }

  if (!fs::dir_exists(output_dir)) {
    fs::dir_create(output_dir)
  }

  if (fs::dir_exists(output_dir) && !overwrite) {
    cli::cli_inform(c(
      "Favicons already exist in {.path {output_dir}}",
      "i" = "Set {.code overwrite = TRUE} to re-create."
    ))
    return(invisible())
  }

  cli::cli_inform(
    c(
      i = "Building favicons with {.url https://realfavicongenerator.net}..."
    )
  )

  logo <- readBin(
    image_path,
    what = "raw",
    n = fs::file_info(image_path)$size
  )

  json_request <- list(
    favicon_generation = list(
      api_key = api_key,
      master_picture = list(
        type = "inline",
        content = openssl::base64_encode(logo)
      ),
      favicon_design = list(
        desktop_browser = list(),
        ios = list(
          picture_aspect = "no_change",
          assets = list(
            ios6_and_prior_icons = ios6_and_prior_icons,
            ios7_and_later_icons = ios7_and_later_icons,
            precomposed_icons = precomposed_icons,
            declare_only_default_icon = declare_only_default_icon
          )
        )
      )
    )
  )

  req <- httr2::request("https://realfavicongenerator.net/api/favicon") |>
    httr2::req_body_json(json_request)

  withCallingHandlers({
    resp <- httr2::req_perform(req)
  },
  error = function(e) {
    cli::cli_abort("API request failed.", parent = e)
  })

  content <- httr2::resp_body_json(resp)

  result <- content$favicon_generation_result

  if (!identical(result$result$status, "success")) {
    cli::cli_abort("API request failed.", .internal = TRUE)
  }

  tmp <- withr::local_tempfile()
  req <- httr2::request(result$favicon$package_url)
  resp <- httr2::req_perform(req, tmp)

  withCallingHandlers({
    paths <- utils::unzip(
      tmp,
      exdir = fs::path(output_dir)
    )
  }, warning = function(e) {
    cli::cli_abort(
      "Your logo file couldn't be processed and may be corrupt.",
      parent = e
    )
  }, error = function(e) {
    cli::cli_abort(
      "Your logo file couldn't be processed and may be corrupt.",
      parent = e
    )
  }
  )

  cli::cli_inform(
    c(
      v = "Added {.path {sort(fs::path_file(paths))}}."
    )
  )

  invisible()
}
