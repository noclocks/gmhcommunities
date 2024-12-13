
#  ------------------------------------------------------------------------
#
# Title : Excel Dataprep
#    By : Jimmy Briggs
#  Date : 2024-12-01
#
#  ------------------------------------------------------------------------


# libraries ---------------------------------------------------------------

require(readxl)
require(openxlsx)
require(writexl)
require(readr)
require(vroom)
require(cellranger)
require(janitor)
require(tidyxl)
require(unpivotr)
require(dplyr)
require(tidyr)
require(purrr)
require(stringr)
require(lubridate)
require(fs)


# files -------------------------------------------------------------------

xl_data_dir <- fs::path("data-raw/data/original")
xl_raw_files <- fs::dir_ls(xl_data_dir, regexp = "~", invert = TRUE, recurse = TRUE)

agc_leasing_summary_report_430_file <- xl_raw_files[1]
xl_data_file <- agc_leasing_summary_report_430_file
sheet_names <- readxl::excel_sheets(xl_data_file)
summary_sheet_name <- "Summary"
params_sheet_name <- "Report Parameters"
property_unit_names <- setdiff(sheet_names, c(summary_sheet_name, params_sheet_name))
unit_names <- sheet_names |> setdiff(c("Report Parameters"))

# col specs ---------------------------------------------------------------

summary_tbl_col_specs <- c(
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

details_tbl_col_specs <- c(
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
  # "ledger" = "numeric",
  # "charge_code" = "text",
  "scheduled_charges" = "numeric",
  "posted_charges" = "numeric"
)

# range specifications ----------------------------------------------------

# get cells
xl_cells <- tidyxl::xlsx_cells(
  xl_data_file,
  sheets = property_unit_names,
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

# unit_type ranges  -------------------------------------------------------

unit_type_ranges <- xl_cells |>
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


# summary table ranges ----------------------------------------------------

summary_tbl_ranges <- xl_cells |>
  dplyr::select(sheet, address, row, col, character) |>
  dplyr::filter(
    stringr::str_detect(character, "Total/Average:")
  ) |>
  dplyr::group_by(sheet) |>
  dplyr::slice(1) |>
  dplyr::mutate(
    start_cell = "A9",
    stop_cell = paste0("O", row - 1)
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



# details table ranges ----------------------------------------------------

# for details table, we need to find the start and stop cells for each unit
# - first, select only the rows where the first column (A) is not blank
# - then, find the rows where the first column contains "Details"
# - the start cells will be 4 rows below the "Details" rows in the current
# - the start cells will be
# - then, find the rows where the first column contains "Total/Average:"

details_tbl_ranges_init <- xl_cells |>
  dplyr::select(sheet, address, row, col, character) |>
  dplyr::filter(
    col == 1
  )

details_tbl_ranges_start_cells <- details_tbl_ranges_init |>
  dplyr::filter(
    stringr::str_detect(character, "Details")
  ) |>
  dplyr::mutate(
    start_cell = paste0("A", row + 4)
  )

details_tbl_ranges_stop_cells <- details_tbl_ranges_init |>
  dplyr::filter(
    stringr::str_detect(character, "Total/Average:")
  ) |>
  dplyr::group_by(sheet) |>
  dplyr::slice(2) |>
  dplyr::mutate(
    stop_cell = paste0("O", row + -1)
  ) |>
  dplyr::ungroup()

details_tbl_ranges <- details_tbl_ranges_start_cells |>
  dplyr::select(sheet, start_cell) |>
  dplyr::inner_join(
    details_tbl_ranges_stop_cells |>
      dplyr::select(sheet, stop_cell),
    by = "sheet"
  ) |>
  dplyr::mutate(
    range = paste0(start_cell, ":", stop_cell)
  )


# report params -----------------------------------------------------------

report_params_tbl_ranges <- xl_cells |>
  dplyr::select(sheet, address, row, col, character) |>
  dplyr::filter(
    stringr::str_detect(character, "Total/Average:")
  ) |>
  dplyr::group_by(sheet) |>
  dplyr::slice(1) |>
  dplyr::mutate(
    start_cell = "A9",
    stop_cell = paste0("O", row - 1)
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

# read in data from range specs -------------------------------------------

# unit types by unit ------------------------------------------------------

unit_types <- purrr::map_dfr(
  unit_type_ranges$sheet,
  ~readxl::read_excel(
    xl_data_file,
    sheet = .x,
    range = unit_type_ranges$range[unit_type_ranges$sheet == .x],
    col_types = "text",
    col_names = "unit_type"
  ) |>
    dplyr::mutate(unit_name = .x) |>
    dplyr::select(unit_name, dplyr::everything())
)

# summary table by unit ---------------------------------------------------

summary_tbls <- purrr::map_dfr(
  summary_tbl_ranges$sheet,
  ~readxl::read_excel(
    xl_data_file,
    sheet = .x,
    range = summary_tbl_ranges$range[summary_tbl_ranges$sheet == .x],
    col_types = summary_tbl_col_specs,
    col_names = names(summary_tbl_col_specs)
  ) |>
    dplyr::mutate(property_name = .x) |>
    dplyr::select(
      property_name,
      dplyr::everything()
    )
)


# details table by unit ---------------------------------------------------

details_tbls <- purrr::map_dfr(
  details_tbl_ranges$sheet,
  ~readxl::read_excel(
    xl_data_file,
    sheet = .x,
    range = details_tbl_ranges$range[details_tbl_ranges$sheet == .x],
    col_types = details_tbl_col_specs,
    col_names = names(details_tbl_col_specs)
  ) |>
    dplyr::filter(
      !is.na(resident),
      # !stringr::str_detect(building_unit, "Unit Type:"),
      # !stringr::str_detect(charge_code, "Charge Total:")
    ) |>
    dplyr::mutate(unit_name = .x) |>
    dplyr::select(unit_name, dplyr::everything())
)

# define ranges

create_ranges <- function(cells, filter_pattern, start_offset, stop_col) {
  cells |>
    dplyr::filter(stringr::str_detect(character, filter_pattern)) |>
    dplyr::group_by(sheet) |>
    dplyr::slice(1) |>
    dplyr::mutate(
      start_cell = paste0("A", start_offset),
      stop_cell = paste0(stop_col, row - 1)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(range = paste0(start_cell, ":", stop_cell)) |>
    dplyr::select(sheet, range)
}

read_excel_ranges <- function(file, sheets, ranges, col_types, col_names) {
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

details_tbl_ranges_init <- xl_cells |> dplyr::filter(col == 1)

details_tbl_ranges_start_cells <- details_tbl_ranges_init |>
  dplyr::filter(stringr::str_detect(character, "Details")) |>
  dplyr::mutate(start_cell = paste0("A", row + 4))

details_tbl_ranges_stop_cells <- details_tbl_ranges_init |>
  dplyr::filter(stringr::str_detect(character, "Total/Average:")) |>
  dplyr::group_by(sheet) |>
  dplyr::slice(2) |>
  dplyr::mutate(stop_cell = paste0("Q", row + 2)) |>
  dplyr::ungroup()

details_tbl_ranges <- details_tbl_ranges_start_cells |>
  dplyr::select(sheet, start_cell) |>
  dplyr::inner_join(
    details_tbl_ranges_stop_cells |>
      dplyr::select(sheet, stop_cell),
    by = "sheet"
  ) |>
  dplyr::mutate(range = paste0(start_cell, ":", stop_cell))


unit_type_ranges_ <- create_ranges(xl_cells, "Total/Average:", 9, "A")
summary_tbl_ranges_ <- create_ranges(xl_cells, "Total/Average:", 9, "O")


#  ------------------------------------------------------------------------
#
# Title : Excel Data Prep
#    By : Jimmy Briggs
#  Date : 2024-06-13
#
#  ------------------------------------------------------------------------


# libs --------------------------------------------------------------------

library(cellranger)
library(readxl)
library(openxlsx)
library(readr)
library(vroom)
library(qs)
library(janitor)
library(tidyxl)
library(unpivotr)
library(openxlsx)
library(tidyr)
library(dplyr)
library(lubridate)
library(fs)


# copy to inst/extdata ----------------------------------------------------

# fs::dir_create("inst/extdata", showWarnings = FALSE)

xl_file_current <- fs::path("data-raw/original/Pre-Lease - 2024-06-05T144159.276.xlsx")
xl_file_current_pkg <- fs::path("inst/extdata/2024-06-05_PreLease.xlsx")
# fs::file_copy(xl_file_current, xl_file_current_pkg, overwrite = TRUE)

xl_file_prior <- fs::path("data-raw/original/AGC Leasing Summary Report-Draft 2024-04-30.xlsx")
xl_file_prior_pkg <- fs::path("inst/extdata/2024-04-30_AGC_LeasingSummaryReport_DRAFT.xlsx")
# fs::file_copy(xl_file_prior, xl_file_prior_pkg, overwrite = TRUE)

# variables ---------------------------------------------------------------

xl_data_file <- fs::path("data-raw/original/Pre-Lease - 2024-06-05T144159.276.xlsx")
sheet_names <- readxl::excel_sheets(xl_data_file)
unit_names <- sheet_names |> setdiff(c("Report Parameters"))

# col specs ---------------------------------------------------------------

summary_tbl_col_specs <- c(
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

details_tbl_col_specs <- c(
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
  # "ledger" = "numeric",
  # "charge_code" = "text",
  "scheduled_charges" = "numeric",
  "posted_charges" = "numeric"
)

# range specifications ----------------------------------------------------

# read in cells
xl_cells <- tidyxl::xlsx_cells(
  xl_data_file,
  sheets = unit_names,
  include_blank_cells = FALSE
)


# unit_type ranges  -------------------------------------------------------

unit_type_ranges <- xl_cells |>
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


# summary table ranges ----------------------------------------------------

summary_tbl_ranges <- xl_cells |>
  dplyr::select(sheet, address, row, col, character) |>
  dplyr::filter(
    stringr::str_detect(character, "Total/Average:")
  ) |>
  dplyr::group_by(sheet) |>
  dplyr::slice(1) |>
  dplyr::mutate(
    start_cell = "A9",
    stop_cell = paste0("O", row - 1)
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



# details table ranges ----------------------------------------------------

# for details table, we need to find the start and stop cells for each unit
# - first, select only the rows where the first column (A) is not blank
# - then, find the rows where the first column contains "Details"
# - the start cells will be 4 rows below the "Details" rows in the current
# - the start cells will be
# - then, find the rows where the first column contains "Total/Average:"

details_tbl_ranges_init <- xl_cells |>
  dplyr::select(sheet, address, row, col, character) |>
  dplyr::filter(
    col == 1
  )

details_tbl_ranges_start_cells <- details_tbl_ranges_init |>
  dplyr::filter(
    stringr::str_detect(character, "Details")
  ) |>
  dplyr::mutate(
    start_cell = paste0("A", row + 4)
  )

details_tbl_ranges_stop_cells <- details_tbl_ranges_init |>
  dplyr::filter(
    stringr::str_detect(character, "Total/Average:")
  ) |>
  dplyr::group_by(sheet) |>
  dplyr::slice(2) |>
  dplyr::mutate(
    stop_cell = paste0("O", row + -1)
  ) |>
  dplyr::ungroup()

details_tbl_ranges <- details_tbl_ranges_start_cells |>
  dplyr::select(sheet, start_cell) |>
  dplyr::inner_join(
    details_tbl_ranges_stop_cells |>
      dplyr::select(sheet, stop_cell),
    by = "sheet"
  ) |>
  dplyr::mutate(
    range = paste0(start_cell, ":", stop_cell)
  )


# read in data from range specs -------------------------------------------

# unit types by unit ------------------------------------------------------

unit_types <- purrr::map_dfr(
  unit_type_ranges$sheet,
  ~readxl::read_excel(
    xl_data_file,
    sheet = .x,
    range = unit_type_ranges$range[unit_type_ranges$sheet == .x],
    col_types = "text",
    col_names = "unit_type"
  ) |>
    dplyr::mutate(unit_name = .x) |>
    dplyr::select(unit_name, dplyr::everything())
)

# summary table by unit ---------------------------------------------------

summary_tbls <- purrr::map_dfr(
  summary_tbl_ranges$sheet,
  ~readxl::read_excel(
    xl_data_file,
    sheet = .x,
    range = summary_tbl_ranges$range[summary_tbl_ranges$sheet == .x],
    col_types = summary_tbl_col_specs,
    col_names = names(summary_tbl_col_specs)
  ) |>
    dplyr::mutate(unit_name = .x)
)


# details table by unit ---------------------------------------------------

details_tbls <- purrr::map_dfr(
  details_tbl_ranges$sheet,
  ~readxl::read_excel(
    xl_data_file,
    sheet = .x,
    range = details_tbl_ranges$range[details_tbl_ranges$sheet == .x],
    col_types = details_tbl_col_specs,
    col_names = names(details_tbl_col_specs)
  ) |>
    dplyr::filter(
      !is.na(resident),
      # !stringr::str_detect(building_unit, "Unit Type:"),
      # !stringr::str_detect(charge_code, "Charge Total:")
    ) |>
    dplyr::mutate(unit_name = .x) |>
    dplyr::select(unit_name, dplyr::everything())
)
