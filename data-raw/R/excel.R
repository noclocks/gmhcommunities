#  ------------------------------------------------------------------------
#
# Title : title
#  File : excel.R
#    By : Jimmy Briggs <jimmy.briggs@noclocks.dev>
#  Date : 2024-11-23
#
#  ------------------------------------------------------------------------

#' Excel Utility Functions
#'
#' @description
#' Functions for working with and munging data originating from Excel.
#'
#' @name excel
NULL

get_xl_sheets <- function(xl_file) {
  sheet_names <- readxl::excel_sheets(xl_file)
  summary_sheet <- sheet_names[1]
  report_params_sheet <- sheet_names[length(sheet_names)]
  property_names <- setdiff(
    sheet_names,
    c(summary_sheet, report_params_sheet)
  )
  list(
    "summary" = summary_sheet,
    "report_params" = report_params_sheet,
    "properties" = property_names
  )
}

read_xl_global_summary_tbl <- function(xl_file, ...) {

  global_summary_tbl_col_specs <- list(
    "property_name" = "text",
    "total_beds" = "numeric",
    "model_beds" = "numeric",
    "current_occupancy" = "numeric",
    "total_new" = "numeric",
    "total_renewals" = "numeric",
    "total_leases" = "numeric",
    "prelease_pct" = "numeric",
    "prior_total_new" = "numeric",
    "prior_total_renewals" = "numeric",
    "prior_total_leases" = "numeric",
    "prior_prelease_pct" = "numeric",
    "yoy_variance_num" = "numeric",
    "yoy_variance_pct" = "numeric",
    "weekly_new_leases" = "numeric",
    "weekly_new_renewals" = "numeric",
    "weekly_total_leases" = "numeric",
    "weekly_pct_gained" = "numeric",
    "weekly_velocity_beds_left" = "numeric",
    "weekly_velocity_leased_this_week" = "numeric",
    "weekly_velocity_90_pct" = "numeric",
    "weekly_velocity_95_pct" = "numeric",
    "weekly_velocity_100_pct" = "numeric"
  )

  global_summary_tbl_metadata <- list(
    leasing_week = readxl::read_excel(
      xl_file,
      sheet = "Summary",
      range = "B3",
      col_names = FALSE,
      col_types = "text"
    ) |> dplyr::pull(1) |> as.integer(),
    leasing_season_ending = readxl::read_excel(
      xl_file,
      sheet = "Summary",
      range = "AB3",
      col_names = FALSE,
      col_types = "date"
    ) |> dplyr::pull(1) |> as.Date(),
    weeks_left_to_lease = readxl::read_excel(
      xl_file,
      sheet = "Summary",
      range = "AB4",
      col_names = FALSE,
      col_types = "numeric"
    ) |> dplyr::pull(1) |> as.integer(),
    report_date = readxl::read_excel(
      xl_file,
      sheet = "Summary",
      range = "A1",
      col_names = FALSE,
      col_types = "date"
    ) |> dplyr::pull(1) |> as.Date()
  )

  global_summary_tbl_totals <- list(
    readxl::read_excel(
      xl_file,
      sheet = "Summary",
      range = "A9:D9",
      col_names = names(global_summary_tbl_col_specs)[c(1:4)],
      col_types = as.vector(unlist(unname(global_summary_tbl_col_specs[c(1:4)])))
    ),
    readxl::read_excel(
      xl_file,
      sheet = "Summary",
      range = "F9:I9",
      col_names = names(global_summary_tbl_col_specs)[c(5:8)],
      col_types = as.vector(unlist(unname(global_summary_tbl_col_specs[c(5:8)])))
    ),
    readxl::read_excel(
      xl_file,
      sheet = "Summary",
      range = "K9:N9",
      col_names = names(global_summary_tbl_col_specs)[c(9:12)],
      col_types = as.vector(unlist(unname(global_summary_tbl_col_specs[c(9:12)])))
    ),
    readxl::read_excel(
      xl_file,
      sheet = "Summary",
      range = "P9:Q9",
      col_names = names(global_summary_tbl_col_specs)[c(13:14)],
      col_types = as.vector(unlist(unname(global_summary_tbl_col_specs[c(13:14)])))
    ),
    readxl::read_excel(
      xl_file,
      sheet = "Summary",
      range = "S9:V9",
      col_names = names(global_summary_tbl_col_specs)[c(15:18)],
      col_types = as.vector(unlist(unname(global_summary_tbl_col_specs[c(15:18)])))
    ),
    readxl::read_excel(
      xl_file,
      sheet = "Summary",
      range = "X9:AB9",
      col_names = names(global_summary_tbl_col_specs)[c(19:23)],
      col_types = as.vector(unlist(unname(global_summary_tbl_col_specs[c(19:23)])))
    )
  ) |>
    dplyr::bind_cols()

  global_summary_tbl <- list(
    readxl::read_excel(
      xl_file,
      sheet = "Summary",
      range = "A11:D20",
      col_names = names(global_summary_tbl_col_specs)[c(1:4)],
      col_types = as.vector(unlist(unname(global_summary_tbl_col_specs[c(1:4)])))
    ),
    readxl::read_excel(
      xl_file,
      sheet = "Summary",
      range = "F11:I20",
      col_names = names(global_summary_tbl_col_specs)[c(5:8)],
      col_types = as.vector(unlist(unname(global_summary_tbl_col_specs[c(5:8)])))
    ),
    readxl::read_excel(
      xl_file,
      sheet = "Summary",
      range = "K11:N20",
      col_names = names(global_summary_tbl_col_specs)[c(9:12)],
      col_types = as.vector(unlist(unname(global_summary_tbl_col_specs[c(9:12)])))
    ),
    readxl::read_excel(
      xl_file,
      sheet = "Summary",
      range = "P11:Q20",
      col_names = names(global_summary_tbl_col_specs)[c(13:14)],
      col_types = as.vector(unlist(unname(global_summary_tbl_col_specs[c(13:14)])))
    ),
    readxl::read_excel(
      xl_file,
      sheet = "Summary",
      range = "S11:V20",
      col_names = names(global_summary_tbl_col_specs)[c(15:18)],
      col_types = as.vector(unlist(unname(global_summary_tbl_col_specs[c(15:18)])))
    ),
    readxl::read_excel(
      xl_file,
      sheet = "Summary",
      range = "X11:AB20",
      col_names = names(global_summary_tbl_col_specs)[c(19:23)],
      col_types = as.vector(unlist(unname(global_summary_tbl_col_specs[c(19:23)])))
    )
  ) |>
    dplyr::bind_cols()

  return(
    list(
      "metadata" = global_summary_tbl_metadata,
      "totals" = global_summary_tbl_totals,
      "data" = global_summary_tbl
    )
  )

}


#' Create Excel Ranges
#'
#' @description
#' Helper function to create ranges for Excel sheets based on filter patterns
#' and column offsets.
#'
#' @param cells `data.frame` of raw Excel cells created via [tidyxl::xlsx_cells()].
#'   Must contain (at least) the columns `sheet`, `row`, `col`, `data_type`
#'   and `character`.
#' @param filter_pattern `character` pattern to filter cells by.
#' @param start_offset `integer` offset to start the range from.
#' @param stop_col `integer` or `character` column to stop the range at.
#'
#' @returns `data.frame` of ranges for each sheet.
#' @export
#'
#' @importFrom dplyr group_by slice ungroup mutate select filter
#' @importFrom stringr str_detect
#' @importFrom purrr map
#' @importFrom tidyr unite
#' @importFrom readxl excel_sheets read_excel
create_ranges <- function(
    cells,
    filter_pattern,
    start_offset,
    stop_col) {
  # validate inputs
  stopifnot(exprs = {
    is.data.frame(cells)
    all(
      c("sheet", "row", "col", "data_type", "character") %in% colnames(cells)
    )
    is.character(filter_pattern)
    is.integer(start_offset)
    is.character(stop_col) || is.integer(stop_col)
  }) |> try()

  # create ranges
  cells |>
    dplyr::filter(
      stringr::str_detect(.data$character, .env$filter_pattern)
    ) |>
    dplyr::group_by(.data$sheet) |>
    dplyr::slice(1) |> # get first row of each sheet
    dplyr::mutate(
      start_cell = paste0("A", .env$start_offset),
      stop_cell = paste0(.env$stop_col, .data$row - 1)
    ) |>
    dplyr::ungroup() %>%
    dplyr::mutate(range = paste0(.data$start_cell, ":", .data$stop_cell)) |>
    dplyr::select(.data$sheet, .data$range)
}

# summary table ranges
# --------------------
# - searches for 'Total/Average:' in the 'character' column
#   to find the final row in the summary table range for each
#   sheet in the excel file representing a property unit
# - slices the first instance of 'Total/Average:' for each sheet
#   to ensure using the top summary table
# - establishes "A9" as the starting cell for each summary table
# - sets the stop cell as the row before the 'Total/Average:' row
# - creates a 'range' column with the range of the summary table
get_xl_summary_tbl_ranges <- function(
    xl_cells,
    start_cell = "A9",
    stop_cell_col = "O",
    stop_cell_row_offset = -1
) {

  xl_cells |>
    dplyr::select(sheet, address, row, col, character) |>
    dplyr::filter(
      stringr::str_detect(character, "Total/Average:")
    ) |>
    dplyr::group_by(sheet) |>
    dplyr::slice(1) |>
    dplyr::mutate(
      start_cell = start_cell,
      stop_cell = paste0(stop_cell_col, row + stop_cell_row_offset)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(
      sheet,
      start_cell,
      stop_cell
    ) |>
    dplyr::mutate(
      range = paste0(start_cell, ":", stop_cell)
    )
}

# prior_summary_tbl_ranges <- get_xl_summary_tbl_ranges(prior_xl_cells)
# current_summary_tbl_ranges <- get_xl_summary_tbl_ranges(current_xl_cells)

# details table ranges ----------------------------------------------------


# details table ranges
# --------------------
# - filters the cells to only include column A and non-blanks
# - find the rows where the first column contains "Details"
# - sets the start cell as 4 rows below the "Details" row
# - find the rows where the first column contains "Total/Average:"
# - sets the stop cell as the row before the 'Total/Average:' row
# - creates a 'range' column with the range of the details table

get_details_tbl_ranges <- function(
    xl_cells,
    start_cell_offset = 4,
    stop_cell_col = "O",
    stop_cell_row_offset = -1
) {

  xl_cells |>
    dplyr::select(sheet, address, row, col, character) |>
    dplyr::filter(
      col == 1
    ) |>
    dplyr::filter(
      stringr::str_detect(character, "Details")
    ) |>
    dplyr::mutate(
      start_cell = paste0("A", row + start_cell_offset)
    ) |>
    dplyr::select(sheet, start_cell) |>
    dplyr::inner_join(
      xl_cells |>
        dplyr::filter(
          stringr::str_detect(character, "Total/Average:")
        ) |>
        dplyr::group_by(sheet) |>
        dplyr::slice(2) |>
        dplyr::mutate(
          stop_cell = paste0(stop_cell_col, row + stop_cell_row_offset)
        ) |>
        dplyr::ungroup() |>
        dplyr::select(sheet, stop_cell),
      by = "sheet"
    ) |>
    dplyr::mutate(
      range = paste0(start_cell, ":", stop_cell)
    )
}

# prior_details_tbl_ranges <- get_details_tbl_ranges(prior_xl_cells)
# current_details_tbl_ranges <- get_details_tbl_ranges(current_xl_cells, stop_cell_col = "Q", stop_cell_row_offset = -1)

# read excel ranges -------------------------------------------------------

read_excel_ranges <- function(file, sheets, ranges, col_specs) {

  col_names <- names(col_specs)
  col_types <- purrr::map_chr(col_specs, ~ .x)

  purrr::map_dfr(
    sheets,
    ~ readxl::read_excel(
      file,
      sheet = .x,
      range = ranges[ranges$sheet == .x, "range"][[1]],
      col_types = col_types,
      col_names = col_names
    ) |>
      dplyr::mutate(unit_name = .x) |>
      dplyr::select(unit_name, tidyselect::everything())
  )
}
