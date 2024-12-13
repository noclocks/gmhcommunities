setup_brand_fonts <- function(
    brand_fonts,
    font_output_dir = fs::path(getwd(), "data-raw/brand/fonts"),
    css_output_dir = fs::path(getwd(), "data-raw/brand/css"),
    ...) {
  font_names <- brand_fonts$name |>
    unique() |>
    tolower()
  font_origins <- brand_fonts$origin |> unique()
  font_origin_ids <- brand_fonts$originId |>
    unique() |>
    tolower()

  params <- list(
    font_name = font_names,
    font_origin = font_origins,
    font_origin_id = font_origin_ids
  )

  purrr::pwalk(
    params,
    download_font_assets,
    font_output_dir = font_output_dir,
    css_output_dir = css_output_dir,
  )
}

download_font_assets <- function(
    font_name,
    font_origin,
    font_origin_id,
    font_output_dir = fs::path(getwd(), "data-raw/brand/fonts"),
    css_output_dir = fs::path(getwd(), "data-raw/brand/css"),
    ...) {
  if (!fs::dir_exists(font_output_dir)) fs::dir_create(font_output_dir, recursive = TRUE)
  if (!fs::dir_exists(css_output_dir)) fs::dir_create(css_output_dir, recursive = TRUE)

  font_name <- tolower(font_name)
  font_origin <- font_origin
  font_origin_id <- tolower(font_origin_id)

  font <- dplyr::filter(
    google_fonts,
    id == font_origin_id
  )

  font_info <- gfonts::get_font_info(
    id = font_origin_id
  )

  font_variants <- font_info$variants$fontStyle |> unique()

  gfonts::download_font(
    id = font_origin_id,
    output_dir = font_output_dir,
    variants = font_variants
  )

  gfonts::generate_css(
    id = font_origin_id,
    variants = font_variants,
    subsets = NULL,
    output = fs::path(css_output_dir, paste0(font_name, ".css")),
    font_dir = fs::path_rel(font_output_dir, css_output_dir),
    prefer_local_source = FALSE,
    browser_support = "modern"
  )

  out <- list(
    font = font,
    font_info = font_info,
    font_variants = font_variants,
    font_files = fs::dir_ls(font_output_dir, glob = glue::glue("*{font_name}*")),
    css_file = fs::path(css_output_dir, paste0(font_name, ".css"))
  )

  return(invisible(out))
}
