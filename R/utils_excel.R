


read_excel_binary <- function(path) {
  readBin(path, "raw", file.info(path)$size)
}











# output$download_excel <- shiny::downloadHandler(
#   filename = function() {
#     paste("GMH_University_Housing_25-26_Prelease_Summary_", format(Sys.Date(), "%Y-%m-%d"), ".xlsx", sep = "")
#   },
#   content = function(
#     file
#   ) {
#
#     colnames <- c(
#       "Property",
#       'Investment Partner',
#       'Total Beds',
#       'Model Beds',
#       'Current Occupancy',
#       'Total New',
#       'Total Renewals',
#       'Total Leases',
#       '25-26 Prelease %',
#       'Prior New',
#       'Prior Renewals',
#       'Prior Total',
#       'Prior Prelease %',
#       'Year-Over-Year Variance Number',
#       'Year-Over-Year Variance %',
#       'Pre Leasing Activity New',
#       'Pre Leasing Activity Renewals',
#       'Pre Leasing Activity Total',
#       'Pre Leasing Activity % Gained',
#       'Weekly Velocity Beds Left to Lease',
#       'Weekly Velocity 90%',
#       'Weekly Velocity 95%',
#       'Weekly Velocity 100%'
#     )
#
#     data <- summary_table_out() |>
#       dplyr::filter(X1 != "Total/Average") |>
#       # Join w/ `investment_partners` table
#       dplyr::left_join(investment_partners_rv(), by = dplyr::join_by(X1 == property_name)) |>
#       dplyr::select(X1, investment_partner, dplyr::everything()) |>
#       rlang::set_names(colnames)
#
#     comma_cols <- c(
#       'Total Beds',
#       'Model Beds',
#       'Total New',
#       'Total Renewals',
#       'Total Leases',
#       'Prior New',
#       'Prior Renewals',
#       'Prior Total',
#       'Year-Over-Year Variance Number',
#       'Pre Leasing Activity New',
#       'Pre Leasing Activity Renewals',
#       'Pre Leasing Activity Total',
#       'Weekly Velocity Beds Left to Lease',
#       'Weekly Velocity 90%',
#       'Weekly Velocity 95%',
#       'Weekly Velocity 100%'
#     )
#
#     percent_cols <- c(
#       'Current Occupancy',
#       '25-26 Prelease %',
#       'Prior Prelease %',
#       'Year-Over-Year Variance %',
#       'Pre Leasing Activity % Gained'
#     )
#
#     # Format numeric columns
#     for (col in comma_cols) {
#       class(data[[col]]) <- c("comma", class(data[[col]]))
#     }
#
#     # Format percentage columns
#     for (col in percent_cols) {
#       class(data[[col]]) <- c("percentage", class(data[[col]]))
#     }
#
#     # Set options for `openxlsx`
#     options("openxlsx.minWidth" = 12)
#     options("openxlsx.maxWidth" = 35)
#
#     options("openxlsx.creator" = "No Clocks, LLC")
#     # options("openxlsx.borders" = "TopBottomLeftRight")
#     # options("openxlsx.borderColour" = "black")
#     # options("openxlsx.borderStyle" = "thin")
#
#     options("openxlsx.dateFormat" = "yyyy-mm-dd")
#     options("openxlsx.datetimeFormat" = "yyyy-mm-dd hh:mm:ss")
#     options("openxlsx.numFmt" = "GENERAL") #"#,##0_);[Red](#,##0)")
#
#     options("openxlsx.header" = NULL)
#     options("openxlsx.headerStyle" = NULL)
#     options("openxlsx.firstColumn" = NULL)
#     options("openxlsx.firstFotter" = NULL)
#     options("openxlsx.firstHeader" = NULL)
#     options("openxlsx.footer" = NULL)
#     options("openxlsx.evenHeader" = NULL)
#     options("openxlsx.evenFooter" = NULL)
#     options("openxlsx.gridLines" = TRUE)
#     options("openxlsx.keepNA" = TRUE)
#     options("openxlsx.na.string" = "-")
#     options("openxlsx.orientation" = "landscape")
#     options("openxlsx.showGridLines" = TRUE)
#     options("openxlsx.tabColour" = "black")
#
#
#     # Create a new workbook
#     wb <- openxlsx::createWorkbook()
#
#     # Add a worksheet
#     openxlsx::addWorksheet(wb, "Prelease Summary")
#
#     # Define styles
#     title_style <- openxlsx::createStyle(
#       fontSize = 12,
#       textDecoration = "bold",
#       fgFill = "#0D1B2D",
#       fontColour = "white",
#       halign = "center",
#       valign = "center",
#       border = "TopBottomLeftRight"
#     )
#
#     subtitle_style <- openxlsx::createStyle(
#       fontSize = 12,
#       textDecoration = "bold",
#       fgFill = "#0D1B2D",
#       fontColour = "white",
#       halign = "center",
#       valign = "center",
#       border = "TopBottomLeftRight"
#     )
#
#     tbl_pre_header_style <- openxlsx::createStyle(
#       fontSize = 11,
#       fontColour = "white",
#       numFmt = "text",
#       border = c("top", "bottom", "left", "right"),
#       borderColour = "black",
#       borderStyle = "thin",
#       bgFill = "#0D1B2D",
#       fgFill = "#0D1B2D",
#       halign = "center",
#       valign = "center",
#       textDecoration = "bold",
#       wrapText = FALSE
#     )
#
#     tbl_header_style <- openxlsx::createStyle(
#       fontSize = 11,
#       fontColour = "white",
#       numFmt = "text",
#       border = c("top", "bottom", "left", "right"),
#       borderColour = "black",
#       borderStyle = "thin",
#       bgFill = "#0D1B2D",
#       fgFill = "#0D1B2D",
#       halign = "center",
#       valign = "center",
#       textDecoration = "bold",
#       wrapText = TRUE
#     )
#
#     tbl_body_style <- openxlsx::createStyle(
#       fontSize = 11,
#       halign = "center",
#       valign = "center",
#       border = "TopBottomLeftRight"
#     )
#
#     tbl_comma_fmt <- openxlsx::createStyle(
#       numFmt = "#,##0_);[Red](#,##0)",
#       halign = "center",
#       valign = "center",
#       border = "TopBottomLeftRight"
#     )
#
#     tbl_percent_fmt <- openxlsx::createStyle(
#       numFmt = "0.0%;[Red](0.0%)",
#       halign = "center",
#       valign = "center",
#       border = "TopBottomLeftRight"
#     )
#
#     # Write the title
#     openxlsx::writeData(
#       wb,
#       "Prelease Summary",
#       "GMH University Housing 25-26 Prelease Summary",
#       startCol = 1,
#       startRow = 1
#     )
#     openxlsx::mergeCells(
#       wb,
#       "Prelease Summary",
#       cols = 1:ncol(data),
#       rows = 1
#     )
#     openxlsx::addStyle(
#       wb,
#       "Prelease Summary",
#       title_style,
#       rows = 1,
#       cols = 1,
#       gridExpand = TRUE
#     )
#
#     # Write the date
#     openxlsx::writeData(
#       wb,
#       "Prelease Summary",
#       paste("Date:", format(Sys.Date(), "%Y-%m-%d")),
#       startCol = 1,
#       startRow = 2
#     )
#     openxlsx::mergeCells(
#       wb,
#       "Prelease Summary",
#       cols = 1:ncol(data),
#       rows = 2
#     )
#     openxlsx::addStyle(
#       wb,
#       "Prelease Summary",
#       subtitle_style,
#       rows = 2,
#       cols = 1,
#       gridExpand = TRUE
#     )
#
#     # Top-Level Headers
#     openxlsx::writeData(
#       wb,
#       "Prelease Summary",
#       "24-25 Prior Year Same Store",
#       startCol = 10,
#       startRow = 4
#     )
#     openxlsx::mergeCells(
#       wb,
#       "Prelease Summary",
#       cols = 10:13,
#       rows = 4
#     )
#     openxlsx::addStyle(
#       wb,
#       "Prelease Summary",
#       tbl_pre_header_style,
#       rows = 4,
#       cols = 10:13,
#       gridExpand = TRUE
#     )
#
#     openxlsx::writeData(
#       wb,
#       "Prelease Summary",
#       "Year-Over-Year Variance",
#       startCol = 14,
#       startRow = 4
#     )
#     openxlsx::mergeCells(
#       wb,
#       "Prelease Summary",
#       cols = 14:15,
#       rows = 4
#     )
#     openxlsx::addStyle(
#       wb,
#       "Prelease Summary",
#       tbl_pre_header_style,
#       rows = 4,
#       cols = 14:15,
#       gridExpand = TRUE
#     )
#
#     openxlsx::writeData(
#       wb,
#       "Prelease Summary",
#       "Preleasing Activity - Prior Seven Days:",
#       startCol = 16,
#       startRow = 4
#     )
#     openxlsx::mergeCells(
#       wb,
#       "Prelease Summary",
#       cols = 16:19,
#       rows = 4
#     )
#     openxlsx::addStyle(
#       wb,
#       "Prelease Summary",
#       tbl_pre_header_style,
#       rows = 4,
#       cols = 16:19,
#       gridExpand = TRUE
#     )
#
#     openxlsx::writeData(
#       wb,
#       "Prelease Summary",
#       "Weekly Velocity Needed:",
#       startCol = 20,
#       startRow = 4
#     )
#     openxlsx::mergeCells(
#       wb,
#       "Prelease Summary",
#       cols = 20:23,
#       rows = 4
#     )
#     openxlsx::addStyle(
#       wb,
#       "Prelease Summary",
#       tbl_pre_header_style,
#       rows = 4,
#       cols = 20:23,
#       gridExpand = TRUE
#     )
#
#     # Rename the columns and format data
#     data <- data |> rlang::set_names(
#       c(
#         "Property",
#         'Investment Partner',
#         'Total Beds',
#         'Model Beds',
#         'Current Occupancy',
#         'Total New',
#         'Total Renewals',
#         'Total Leases',
#         '25-26 Prelease %',
#         'Prior New',
#         'Prior Renewals',
#         'Prior Total',
#         'Prior Prelease %',
#         'Variance #',
#         'Variance %',
#         'New',
#         'Renewals',
#         'Total',
#         '% Gained',
#         'Beds Left to Lease',
#         '90%',
#         '95%',
#         '100%'
#       )
#     ) #|>
#     # dplyr::mutate(
#     #   `Total Beds` = prettyNum(`Total Beds`, big.mark = ','),
#     #   `Model Beds` = prettyNum(`Model Beds`, big.mark = ','),
#     #   `Current Occupancy` = paste0(`Current Occupancy`, '%'),
#     #   `Total New` = prettyNum(`Total New`, big.mark = ','),
#     #   `Total Renewals` = prettyNum(`Total Renewals`, big.mark = ','),
#     #   `Total Leases` = prettyNum(`Total Leases`, big.mark = ','),
#     #   `25-26 Prelease %` = paste0(`25-26 Prelease %`, '%'),
#     #   `Prior New` = prettyNum(`Prior New`, big.mark = ','),
#     #   `Prior Renewals` = prettyNum(`Prior Renewals`, big.mark = ','),
#     #   `Prior Total` = prettyNum(`Prior Total`, big.mark = ','),
#     #   `Prior Prelease %` = paste0(`Prior Prelease %`, '%'),
#     #   `Variance #` = prettyNum(`Variance #`, big.mark = ','),
#     #   `Variance %` = paste0(`Variance %`, '%'),
#     #   `New` = prettyNum(`New`, big.mark = ','),
#     #   `Renewals` = prettyNum(`Renewals`, big.mark = ','),
#     #   `Total` = prettyNum(`Total`, big.mark = ','),
#     #   `% Gained` = paste0(`% Gained`, '%'),
#     #   `Beds Left to Lease` = prettyNum(`Beds Left to Lease`, big.mark = ','),
#     #   `90%` = paste0(`90%`, '%'),
#     #   `95%` = paste0(`95%`, '%'),
#     #   `100%` = paste0(`100%`, '%')
#     # )
#
#     # Write the data
#     openxlsx::writeData(
#       wb,
#       "Prelease Summary",
#       data,
#       startCol = 1,
#       startRow = 5,
#       headerStyle = tbl_header_style
#     )
#     openxlsx::addStyle(
#       wb,
#       "Prelease Summary",
#       tbl_body_style,
#       rows = 6:(nrow(data) + 5),
#       cols = 1:ncol(data),
#       gridExpand = TRUE
#     )
#     openxlsx::setColWidths(
#       wb,
#       "Prelease Summary",
#       cols = 1:22,
#       widths = "auto",
#       ignoreMergedCells = TRUE
#     )
#
#     # add style to columns
#
#     comma_cols <- c(
#       3:4,
#       6:8,
#       10:12,
#       14,
#       16:18,
#       20:23
#     )
#
#     percent_cols <- c(
#       5,
#       9,
#       13,
#       15,
#       19
#     )
#
#     openxlsx::addStyle(
#       wb,
#       "Prelease Summary",
#       tbl_comma_fmt,
#       rows = 6:(nrow(data) + 5),
#       cols = comma_cols,
#       gridExpand = TRUE
#     )
#
#     openxlsx::addStyle(
#       wb,
#       "Prelease Summary",
#       tbl_percent_fmt,
#       rows = 6:(nrow(data) + 5),
#       cols = percent_cols,
#       gridExpand = TRUE
#     )
#
#     totals_data <- summary_table_out() |>
#       dplyr::filter(X1 == "Total/Average") |>
#       # Create empty column & entry for `Investment Partner`
#       dplyr::mutate(investment_partner = '') |>
#       dplyr::select(X1, investment_partner, dplyr::everything()) |>
#       rlang::set_names(colnames)
#
#     openxlsx::writeData(
#       wb,
#       "Prelease Summary",
#       totals_data,
#       startCol = 1,
#       startRow = nrow(data) + 7,
#       colNames = FALSE
#     )
#
#     openxlsx::addStyle(
#       wb,
#       "Prelease Summary",
#       tbl_body_style,
#       rows = nrow(data) + 7,
#       cols = 1:ncol(totals_data),
#       gridExpand = TRUE
#     )
#
#     openxlsx::addStyle(
#       wb,
#       "Prelease Summary",
#       tbl_comma_fmt,
#       rows = nrow(data) + 7,
#       cols = comma_cols
#     )
#
#     openxlsx::addStyle(
#       wb,
#       "Prelease Summary",
#       tbl_percent_fmt,
#       rows = nrow(data) + 7,
#       cols = percent_cols
#     )
#
#     openxlsx::addStyle(
#       wb,
#       "Prelease Summary",
#       tbl_header_style,
#       rows = nrow(data) + 7,
#       cols = 1
#     )
#
#     # Save the workbook
#     openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
#   }
# )
