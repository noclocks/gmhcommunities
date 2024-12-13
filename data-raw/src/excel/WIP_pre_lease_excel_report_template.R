library(openxlsx)
library(dplyr)
library(readr)

# Read the CSV files
global_summary <- read_csv("agc_leasing_summary_report_430_global_summary_tbl.csv")
property_summary <- read_csv("agc_leasing_summary_report_430_property_summary_tbl.csv")
property_details <- read_csv("agc_leasing_summary_report_430_property_details_tbl.csv")
report_params <- read_csv("agc_leasing_summary_report_430_report_params_tbl.csv")

# Create a new workbook
wb <- createWorkbook()

# Helper function to apply corporate styling
apply_corporate_style <- function(wb, sheet, range) {
  addStyle(wb, sheet, style = createStyle(
    fontSize = 11, fontName = "Arial",
    border = "TopBottom", borderColour = "#4F81BD",
    fgFill = "#DCE6F1", halign = "center", valign = "center",
    textDecoration = "bold"
  ), rows = 1, cols = 1:ncol(range), gridExpand = TRUE)
}

# Cover Page
addWorksheet(wb, "Cover Page")
writeData(wb, "Cover Page", "Corporate Pre-Lease Property Report", startRow = 5, startCol = 2)
mergeCells(wb, "Cover Page", rows = 5:7, cols = 2:5)
addStyle(wb, "Cover Page", style = createStyle(fontSize = 24, fontName = "Arial", textDecoration = "bold"),
         rows = 5, cols = 2)

# Index Tab
addWorksheet(wb, "Index")
writeData(wb, "Index", "Table of Contents", startRow = 2, startCol = 2)
writeData(wb, "Index", c("Tab Name", "Description"), startRow = 4, startCol = 2)
writeData(wb, "Index", c("Cover Page", "Title and Report Information"), startRow = 5, startCol = 2)
writeData(wb, "Index", c("Summary", "Global Summary Table"), startRow = 6, startCol = 2)
writeData(wb, "Index", c("Reconciliation", "Reconciliation Details"), startRow = 7, startCol = 2)
writeData(wb, "Index", c("Property Tabs", "Individual Property Data"), startRow = 8, startCol = 2)
writeData(wb, "Index", c("Report Parameters", "Report Parameters Used"), startRow = 9, startCol = 2)
apply_corporate_style(wb, "Index", global_summary)

# Reconciliation Tab
addWorksheet(wb, "Reconciliation")
writeData(wb, "Reconciliation", "Reconciliation Details", startRow = 2, startCol = 2)
# Add reconciliation data here when available

# Summary Tab
addWorksheet(wb, "Summary")
writeData(wb, "Summary", "Global Summary Table", startRow = 2, startCol = 2)
writeData(wb, "Summary", global_summary, startRow = 4, startCol = 2)
apply_corporate_style(wb, "Summary", global_summary)

# Property Tabs
property_names <- unique(property_summary$property_name)
for (property in property_names) {
  sheet_name <- substr(property, 1, 31)  # Excel sheet names are limited to 31 characters
  addWorksheet(wb, sheet_name)

  # Property Summary
  writeData(wb, sheet_name, paste("Summary for", property), startRow = 2, startCol = 2)
  property_data <- filter(property_summary, property_name == property)
  writeData(wb, sheet_name, property_data, startRow = 4, startCol = 2)
  apply_corporate_style(wb, sheet_name, property_data)

  # Property Details
  writeData(wb, sheet_name, "Property Details", startRow = nrow(property_data) + 7, startCol = 2)
  details_data <- filter(property_details, property_name == property)
  writeData(wb, sheet_name, details_data, startRow = nrow(property_data) + 9, startCol = 2)
  apply_corporate_style(wb, sheet_name, details_data)
}

# Report Parameters Tab
addWorksheet(wb, "Report Parameters")
writeData(wb, "Report Parameters", "Report Parameters", startRow = 2, startCol = 2)
writeData(wb, "Report Parameters", report_params, startRow = 4, startCol = 2)
apply_corporate_style(wb, "Report Parameters", report_params)

# Save the workbook
saveWorkbook(wb, "dev/Pre-Lease_Report_Template.xlsx", overwrite = TRUE)

# Open the workbook
openXL("dev/Pre-Lease_Report_Template.xlsx")
