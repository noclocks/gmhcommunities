
#  ------------------------------------------------------------------------
#
# Title : Excel Specs
#    By : Jimmy Briggs
#  Date : 2024-12-01
#
#  ------------------------------------------------------------------------


# files -------------------------------------------------------------------

xl_data_dir <- fs::path("data-raw/data/original")
xl_raw_files <- fs::dir_ls(xl_data_dir, regexp = "~", invert = TRUE, recurse = TRUE)

agc_leasing_summary_report_430_file <- xl_raw_files[1]
pre_lease_06_file <- xl_raw_files[3]
pre_lease_08_file <- xl_raw_files[4]




# tables ------------------------------------------------------------------

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

global_summary_tbl_xl_cells <- tidyxl::xlsx_cells(
  agc_leasing_summary_report_430_file,
  sheets = "Summary",
  include_blank_cells = FALSE
) |>
  dplyr::select(
    sheet,
    address,
    row,
    col,
    data_type,
    character
  )

property_summary_tbl_col_specs <- list(
  "unit_type" = "text",
  "excluded_units" = "numeric",
  "rentable_units" = "numeric",
  "avg_scheduled_charges" = "numeric",
  "occupied_current" = "numeric",
  "new_lease_2023" = "numeric",
  "new_lease_2024" = "numeric",
  "renewal_2023" = "numeric",
  "renewal_2024" = "numeric",
  "total_2023" = "numeric",
  "total_2024" = "numeric",
  "pct_2023" = "numeric",
  "pct_2024" = "numeric",
  "variance" = "numeric",
  "projected_availability" = "numeric"
)

property_details_tbl_col_specs <- list(
  "building_unit" = "text",
  "unit_type" = "text",
  "unit_status" = "text",
  "resident" = "text",
  "lease_status" = "text",
  "lease_term_name" = "text",
  "lease_term_duration" = "numeric",
  "lease_term_start_date" = "date",
  "lease_term_end_date" = "date",
  "completed_date" = "date",
  "approved_date" = "date",
  "deposit_charged" = "numeric",
  "budgeted_rent" = "numeric",
  # "ledger" = "text",
  # "charge_code" = "text",
  "scheduled_charges" = "numeric",
  "posted_charges" = "numeric"
)

global_summary_xl_cells <- tidyxl::xlsx_cells(
  agc_leasing_summary_report_430_file,
  sheets = "Summary",
  include_blank_cells = FALSE
) |>
  dplyr::select(
    sheet,
    address,
    row,
    col,
    data_type,
    character
  )

property_xl_cells <- tidyxl::xlsx_cells(
  agc_leasing_summary_report_430_file,
  sheets = get_xl_sheets(agc_leasing_summary_report_430_file)$properties,
  include_blank_cells = FALSE
) |>
  dplyr::select(
    sheet,
    address,
    row,
    col,
    data_type,
    character
  )

report_params_xl_cells <- tidyxl::xlsx_cells(
  agc_leasing_summary_report_430_file,
  sheets = get_xl_sheets(agc_leasing_summary_report_430_file)$report_params,
  include_blank_cells = FALSE
) |>
  dplyr::select(
    sheet,
    address,
    row,
    col,
    data_type,
    character
  )

# ranges ------------------------------------------------------------------

# global summary table:
# A1: Report Date
# B3: Leasing Week
# AB3: Leasing Season Ending
# AB4: Weeks Left to Lease

# no colnames
# Start Cell: A11

global_summary_tbl_metadata <- list(
  leasing_week = readxl::read_excel(
    agc_leasing_summary_report_430_file,
    sheet = "Summary",
    range = "B3",
    col_names = FALSE,
    col_types = "text"
  ) |> dplyr::pull(1) |> as.integer(),
  leasing_season_ending = readxl::read_excel(
    agc_leasing_summary_report_430_file,
    sheet = "Summary",
    range = "AB3",
    col_names = FALSE,
    col_types = "date"
  ) |> dplyr::pull(1) |> as.Date(),
  weeks_left_to_lease = readxl::read_excel(
    agc_leasing_summary_report_430_file,
    sheet = "Summary",
    range = "AB4",
    col_names = FALSE,
    col_types = "numeric"
  ) |> dplyr::pull(1) |> as.integer(),
  report_date = readxl::read_excel(
    agc_leasing_summary_report_430_file,
    sheet = "Summary",
    range = "A1",
    col_names = FALSE,
    col_types = "date"
  ) |> dplyr::pull(1) |> as.Date()
)

global_summary_tbl_totals <- list(
  readxl::read_excel(
    agc_leasing_summary_report_430_file,
    sheet = "Summary",
    range = "A9:D9",
    col_names = names(global_summary_tbl_col_specs)[c(1:4)],
    col_types = as.vector(unlist(unname(global_summary_tbl_col_specs[c(1:4)])))
  ),
  readxl::read_excel(
    agc_leasing_summary_report_430_file,
    sheet = "Summary",
    range = "F9:I9",
    col_names = names(global_summary_tbl_col_specs)[c(5:8)],
    col_types = as.vector(unlist(unname(global_summary_tbl_col_specs[c(5:8)])))
  ),
  readxl::read_excel(
    agc_leasing_summary_report_430_file,
    sheet = "Summary",
    range = "K9:N9",
    col_names = names(global_summary_tbl_col_specs)[c(9:12)],
    col_types = as.vector(unlist(unname(global_summary_tbl_col_specs[c(9:12)])))
  ),
  readxl::read_excel(
    agc_leasing_summary_report_430_file,
    sheet = "Summary",
    range = "P9:Q9",
    col_names = names(global_summary_tbl_col_specs)[c(13:14)],
    col_types = as.vector(unlist(unname(global_summary_tbl_col_specs[c(13:14)])))
  ),
  readxl::read_excel(
    agc_leasing_summary_report_430_file,
    sheet = "Summary",
    range = "S9:V9",
    col_names = names(global_summary_tbl_col_specs)[c(15:18)],
    col_types = as.vector(unlist(unname(global_summary_tbl_col_specs[c(15:18)])))
  ),
  readxl::read_excel(
    agc_leasing_summary_report_430_file,
    sheet = "Summary",
    range = "X9:AB9",
    col_names = names(global_summary_tbl_col_specs)[c(19:23)],
    col_types = as.vector(unlist(unname(global_summary_tbl_col_specs[c(19:23)])))
  )
) |>
  dplyr::bind_cols()

global_summary_tbl <- list(
  readxl::read_excel(
    agc_leasing_summary_report_430_file,
    sheet = "Summary",
    range = "A11:D20",
    col_names = names(global_summary_tbl_col_specs)[c(1:4)],
    col_types = as.vector(unlist(unname(global_summary_tbl_col_specs[c(1:4)])))
  ),
  readxl::read_excel(
    agc_leasing_summary_report_430_file,
    sheet = "Summary",
    range = "F11:I20",
    col_names = names(global_summary_tbl_col_specs)[c(5:8)],
    col_types = as.vector(unlist(unname(global_summary_tbl_col_specs[c(5:8)])))
  ),
  readxl::read_excel(
    agc_leasing_summary_report_430_file,
    sheet = "Summary",
    range = "K11:N20",
    col_names = names(global_summary_tbl_col_specs)[c(9:12)],
    col_types = as.vector(unlist(unname(global_summary_tbl_col_specs[c(9:12)])))
  ),
  readxl::read_excel(
    agc_leasing_summary_report_430_file,
    sheet = "Summary",
    range = "P11:Q20",
    col_names = names(global_summary_tbl_col_specs)[c(13:14)],
    col_types = as.vector(unlist(unname(global_summary_tbl_col_specs[c(13:14)])))
  ),
  readxl::read_excel(
    agc_leasing_summary_report_430_file,
    sheet = "Summary",
    range = "S11:V20",
    col_names = names(global_summary_tbl_col_specs)[c(15:18)],
    col_types = as.vector(unlist(unname(global_summary_tbl_col_specs[c(15:18)])))
  ),
  readxl::read_excel(
    agc_leasing_summary_report_430_file,
    sheet = "Summary",
    range = "X11:AB20",
    col_names = names(global_summary_tbl_col_specs)[c(19:23)],
    col_types = as.vector(unlist(unname(global_summary_tbl_col_specs[c(19:23)])))
  )
) |>
  dplyr::bind_cols()



global_summary_tbl_lst <- read_xl_global_summary_tbl(agc_leasing_summary_report_430_file)



get_global_summary_tbl_ranges <- function(xl_cells) {
  xl_cells |>
    dplyr::select(sheet, address, row, col, character) |>
    dplyr::filter(
      stringr::str_detect(character, "Total/Average:")
    ) |>
    dplyr::group_by(sheet) |>
    dplyr::slice(1) |>
    dplyr::mutate(
      start_cell = "A9",
      stop_cell = paste0("A", row - 1)
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

get_unit_type_xl_ranges <- function(xl_cells) {
  xl_cells |>
    dplyr::select(sheet, address, row, col, character) |>
    dplyr::filter(
      stringr::str_detect(character, "Total/Average:")
    ) |>
    dplyr::group_by(sheet) |>
    dplyr::slice(1) |>
    dplyr::mutate(
      start_cell = "A9",
      stop_cell = paste0("A", row - 1)
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

get_property_summary_tbl_xl_ranges <- function(
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

# details table ranges
# --------------------
# - filters the cells to only include column A and non-blanks
# - find the rows where the first column contains "Details"
# - sets the start cell as 4 rows below the "Details" row
# - find the rows where the first column contains "Total/Average:"
# - sets the stop cell as the row before the 'Total/Average:' row
# - creates a 'range' column with the range of the details table

get_property_details_tbl_ranges <- function(
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

global_summary_tbl_ranges <- get_global_summary_tbl_ranges(global_summary_tbl_xl_cells)

property_summary_tbl_ranges <- get_property_summary_tbl_xl_ranges(property_xl_cells)
property_details_tbl_ranges <- get_property_details_tbl_ranges(property_xl_cells)

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
      dplyr::mutate(property_name = .x) |>
      dplyr::select(property_name, tidyselect::everything())
  )
}

global_summary_tbl <- read_excel_ranges(
  agc_leasing_summary_report_430_file,
  "Summary",
  global_summary_tbl_ranges,
  global_summary_tbl_col_specs
)

property_summary_tbl <- read_excel_ranges(
  agc_leasing_summary_report_430_file,
  get_xl_sheets(agc_leasing_summary_report_430_file)$properties,
  property_summary_tbl_ranges,
  property_summary_tbl_col_specs
)

property_details_tbl <- read_excel_ranges(
  agc_leasing_summary_report_430_file,
  get_xl_sheets(agc_leasing_summary_report_430_file)$properties,
  property_details_tbl_ranges,
  property_details_tbl_col_specs
)

report_params_tbl <- list(
  readxl::read_excel(
    agc_leasing_summary_report_430_file,
    get_xl_sheets(agc_leasing_summary_report_430_file)$report_params,
    "A4:B18",
    col_names = c("key", "value"),
    col_types = c("text", "text")
  ),
  readxl::read_excel(
    agc_leasing_summary_report_430_file,
    get_xl_sheets(agc_leasing_summary_report_430_file)$report_params,
    "A20:B21",
    col_names = c("key", "value"),
    col_types = c("text", "text")
  )
) |>
  dplyr::bind_rows()

readr::write_csv(global_summary_tbl, "data-raw/data/working/agc_leasing_summary_report_430_global_summary_tbl.csv")
readr::write_csv(property_summary_tbl, "data-raw/data/working/agc_leasing_summary_report_430_property_summary_tbl.csv")
readr::write_csv(property_details_tbl, "data-raw/data/working/agc_leasing_summary_report_430_property_details_tbl.csv")
readr::write_csv(report_params_tbl, "data-raw/data/working/agc_leasing_summary_report_430_report_params_tbl.csv")

agc_wb_data <- list(
  global_summary_tbl = global_summary_tbl,
  property_summary_tbl = property_summary_tbl,
  property_details_tbl = property_details_tbl,
  report_params_tbl = report_params_tbl
)

qs2::qs_save(agc_wb_data, "data-raw/data/working/agc_leasing_summary_report_430_wb_data.qs")

agc_wb_working <- "data-raw/data/working/agc_leasing_summary_report_430.xlsx"

openxlsx::write.xlsx(
  agc_wb_data,
  agc_wb_working,
  asTable = TRUE
)



# global summary table ----------------------------------------------------



# unit_type ranges  -------------------------------------------------------



unit_type_ranges <-

  xl_cells |>
  dplyr::select(sheet, address, row, col, character) |>
  dplyr::filter(
    stringr::str_detect(character, "Total/Average:")
  ) |>
  dplyr::group_by(sheet) |>
  dplyr::slice(1) |>
  dplyr::mutate(
    start_cell = "A9",
    stop_cell = paste0("A", row - 1)
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
