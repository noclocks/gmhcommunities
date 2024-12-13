download_brand_logos <- function(
  brand,
  path = "inst/extdata/brand",
  ...
) {

if (!fs::dir_exists(path)) {
  fs::dir_create(path)
}

brand_logos <- brand$logos |>
  dplyr::mutate(
    file = purrr::map(
      list(
        brand_name = brand$name,
        type = type,
        theme = theme,
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
  ...
) {

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
  ...
) {

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
