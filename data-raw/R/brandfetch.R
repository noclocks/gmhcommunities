
get_brandfetch_api_key <- function() {

  config::get("brandfetch_api_key") %||%
    config::get("tools")$brandfetch_api_key %||%
    Sys.getenv("BRANDFETCH_API_KEY") %||%
    getOption("brandfetch_api_key") %||%
    NULL

}

safe_pluck <- function(x, ...) {
  if (purrr::pluck_exists(x, ...)) {
    purrr::pluck(x, ...)
  } else {
    NA_character_
  }
}

#' Fetch a Brand using the Brandfetch API
#'
#' @description
#' This function fetches a brand using the
#' [Brandfetch Brand API](https://docs.brandfetch.com/reference/brand-api).
#'
#' @param domain The domain of the brand to fetch
#' @param brandfetch_api_key The API key for the Brandfetch API
#' @param ... Additional arguments
#'
#' @return A tibble with the brand information
#'
#' @export
#'
#' @example examples/ex_brand_fetch.R
#'
#' @importFrom httr2 request req_url_path_append req_method req_auth_bearer_token req_headers req_perform
#' @importFrom tibblify tibblify tspec_object tib_chr tib_lgl tib_dbl tib_unspecified tib_df tib_row
#' @importFrom purrr pluck
#' @importFrom tidyr unnest
#' @importFrom rlang abort
#' @importFrom config get
#' @importFrom tibble tibble
#' @importFrom httr2 resp_body_json
#' @importFrom dplyr pull
fetch_brand <- function(
    domain,
    brandfetch_api_key = get_brandfetch_api_key(),
    ...
) {

  base_url <- "https://api.brandfetch.io/v2/brands"

  if (is.null(brandfetch_api_key)) {
    brandfetch_api_key <- config::get("brandfetch_api_key")
  }

  if (is.null(brandfetch_api_key)) {
    rlang::abort("No Brandfetch API key found")
  }

  req <- httr2::request(
    base_url = base_url
  ) |>
    httr2::req_url_path_append(
      domain
    ) |>
    httr2::req_method("GET") |>
    httr2::req_auth_bearer_token(brandfetch_api_key) |>
    httr2::req_headers(
      `Accept` = "application/json",
      `Content-Type` = "application/json"
    )

  res <- req |> httr2::req_perform()

  if (res$status_code != 200) {
    rlang::abort("Brandfetch API request failed")
  }


  # extract content ---------------------------------------------------------
  content <- res |>
    httr2::resp_body_json()

  init_spec <- tibblify::tspec_row(
    tibblify::tib_chr("id"),
    tibblify::tib_chr("name"),
    tibblify::tib_chr("domain"),
    tag_line = tibblify::tib_chr("description"),
    description = tibblify::tib_chr("longDescription"),
    quality_score = tibblify::tib_dbl("qualityScore")
  )

  brand_init <- tibblify::tibblify(content, init_spec)

  # links -------------------------------------------------------------------
  link_names <- content$links |> purrr::map_chr(purrr::pluck, "name")
  link_urls <- content$links |> purrr::map_chr(purrr::pluck, "url")

  links <- tibble::tibble(
    name = link_names,
    url = link_urls
  )

  # logos -------------------------------------------------------------------
  logo_themes <- c()
  logo_types <- c()
  logo_urls <- c()
  logo_backgrounds <- c()
  logo_exts <- c()
  logo_heights <- c()
  logo_widths <- c()
  logo_sizes <- c()

  logo_content <- content$logos |>
    jsonlite::toJSON(auto_unbox = TRUE) |>
    jsonlite::fromJSON(flatten = TRUE)

  logo_formats <- content$logos |>
    purrr::map(purrr::pluck, "formats")

  for (i in seq_along(content$logos)) {

    theme <- content$logos[[i]]$theme
    if (is.null(theme)) { theme <- NA_character_ }

    type <- content$logos[[i]]$type

    formats <- content$logos[[i]]$formats

    for (j in seq_along(formats)) {

      url <- formats[[j]]$src
      bg <- formats[[j]]$background
      ext <- formats[[j]]$format
      height <- formats[[j]]$height
      width <- formats[[j]]$width
      size <- formats[[j]]$size

      if (is.null(bg)) { bg <- NA_character_ }
      if (ext == "svg") {
        height <- NA_integer_
        width <- NA_integer_
      }

      logo_themes <- c(logo_themes, theme)
      logo_types <- c(logo_types, type)
      logo_urls <- c(logo_urls, url)
      logo_backgrounds <- c(logo_backgrounds, bg)
      logo_exts <- c(logo_exts, ext)
      logo_heights <- c(logo_heights, height)
      logo_widths <- c(logo_widths, width)
      logo_sizes <- c(logo_sizes, size)

    }

  }

  logos <- tibble::tibble(
    theme = logo_themes,
    type = logo_types,
    src = logo_urls,
    background = logo_backgrounds,
    ext = logo_exts,
    height = logo_heights,
    width = logo_widths,
    size = logo_sizes
  )

  # colors ------------------------------------------------------------------
  colors_hex <- content$colors |> purrr::map_chr(purrr::pluck, "hex")
  colors_type <- content$colors |> purrr::map_chr(purrr::pluck, "type")
  colors_brightness <- content$colors |> purrr::map_int(purrr::pluck, "brightness")

  hex2rgb <- function(hex) {
    hold <- grDevices::col2rgb(hex)
    r <- hold[1]
    g <- hold[2]
    b <- hold[3]
    out <- glue::glue(
      "rgb({r}, {g}, {b})"
    )
    out
  }

  colors <- tibble::tibble(
    hex = colors_hex,
    type = colors_type,
    brightness = colors_brightness
  ) |>
    dplyr::mutate(
      rgb = purrr::map_chr(hex, hex2rgb)
    )

  # fonts -------------------------------------------------------------------
  fonts_name <- content$fonts |> purrr::map_chr(safe_pluck, "name")
  fonts_type <- content$fonts |> purrr::map_chr(safe_pluck, "type")
  fonts_origin <- content$fonts |> purrr::map_chr(safe_pluck, "origin")
  fonts_origin_id <- content$fonts |> purrr::map_chr(safe_pluck, "originId")

  fonts <- tibble::tibble(
    name = fonts_name,
    type = fonts_type,
    origin = fonts_origin,
    origin_id = fonts_origin_id
  ) |>
    dplyr::mutate(
      gfonts_url = paste0(
        "https://fonts.google.com/specimen/",
        name
      ),
      import_css = paste0(
        "@import url(",
        stringr::str_c(
          "https://fonts.googleapis.com/css2?family=",
          name
        ),
        ");"
      )
    )

  # company -----------------------------------------------------------------
  company <- content$company |> purrr::compact()

  industries <- company$industries

  industry_names <- industries |> purrr::map_chr(purrr::pluck, "name")
  industry_emojis <- industries |> purrr::map_chr(purrr::pluck, "emoji")
  industry_slugs <- industries |> purrr::map_chr(purrr::pluck, "slug")
  industry_scores <- industries |> purrr::map_dbl(purrr::pluck, "score")

  company_industries <- tibble::tibble(
    name = industry_names,
    emoji = industry_emojis,
    slug = industry_slugs,
    score = industry_scores
  ) |>
    dplyr::arrange(
      desc(score)
    )


  # brand -------------------------------------------------------------------

  brand <- brand_init |>
    dplyr::mutate(
      links = list(links),
      logos = list(logos),
      colors = list(colors),
      fonts = list(fonts),
      company = list(company_industries)
    )

  return(brand)

}


download_brand_logos <- function(
    brand,
    path = "inst/extdata/brand",
    ...) {
  if (!fs::dir_exists(path)) {
    fs::dir_create(path)
  }

  brand_logos <- brand$logos |>
    dplyr::mutate(
      file = purrr::pmap_chr(
        list(
          brand_name = brand$name,
          type = type,
          format = format,
          height = height,
          width = width
        ),
        get_logo_file_name
      )
    )

  brand_logos |>
    purrr::pwalk(
      download_logo,
      src = brand$logos$src,
      name = brand$name,
      path = path,
      ...
    )

  return(
    invisible(TRUE)
  )
}

download_logo <- function(
    src,
    file,
    name,
    path = "inst/extdata/brand",
    type = c("icon", "logo"),
    format = c("png", "svg", "jpeg"),
    height,
    width,
    ...) {
  type <- match.arg(type)
  format <- match.arg(format)
  height <- as.integer(height)
  width <- as.integer(width)
  src <- src |> stringr::str_replace_all(" ", "%20")
  brand_name_clean <- stringr::str_to_lower(name) |> stringr::str_replace_all(" ", "_")
  size <- paste0(as.character(height), "x", as.character(width))

  if (!fs::dir_exists(path)) {
    fs::dir_create(path)
  }

  file_path <- fs::path(path, file)

  download.file(
    src,
    destfile = file_path,
    method = "curl"
  )

  return(
    invisible(TRUE)
  )
}

get_logo_file_name <- function(
    brand_name,
    type,
    theme,
    format,
    height = NA,
    width = NA,
    ...) {
  brand_name_clean <- stringr::str_to_lower(brand_name) |> stringr::str_replace_all(" ", "_")
  size <- ""
  if (all(
    !is.na(height),
    !is.na(width),
    format != "svg"
  )) {
    size <- paste0("-", as.character(height), "x", as.character(width))
  }

  paste0(
    brand_name_clean,
    "-",
    type,
    "-",
    theme,
    size,
    ".",
    format
  )
}
