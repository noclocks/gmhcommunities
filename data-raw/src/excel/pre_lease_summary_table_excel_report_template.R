
global_summary <- readr::read_csv("data-raw/data/working/agc_leasing/agc_leasing_summary_report_430_global_summary_tbl.csv")
property_summary <- readr::read_csv("data-raw/data/working/agc_leasing/agc_leasing_summary_report_430_property_summary_tbl.csv")
property_details <- readr::read_csv("data-raw/data/working/agc_leasing/agc_leasing_summary_report_430_property_details_tbl.csv")
report_params <- readr::read_csv("data-raw/data/working/agc_leasing/agc_leasing_summary_report_430_report_params_tbl.csv")

gmh_logo_image <- "inst/www/images/gmh/logos/communities/gmh-communities-logo.png"

pre_lease_wb <- openxlsx2::wb_workbook(
  creator = "No Clocks, LLC",
  title = "GMH Communities Pre-Lease Summary Report",
  subject = "Pre-Lease",
  category = "GMH Communities",
  keywords = c("GMH", "Pre-Lease", "Summary", "Report"),
  company = "GMH Communities",
  theme = NULL
)

xl_gmh_styles <- function(wb, sheet, range) {

  openxlsx2::wb_add_style(

  )

}



names(summary_table_data)

c(
  "Property Name",
  "Total Number of Beds",
  "Manual Model Beds",
  "Current Occupancy %",
  "Total New Leases",
  "Total Renewals",
  "Total New Leases & Renewals",
  "Current Pre-Lease %",
  "Current New Leases",
  "Current "
)



col_display_names <- snakecase::to_title_case(names(summary_table_data))


require(openxlsx)

options("openxlsx.minWidth" = 12)
options("openxlsx.maxWidth" = 35)
options("openxlsx.creator" = "No Clocks, LLC")
# options("openxlsx.borders" = "TopBottomLeftRight")
# options("openxlsx.borderColour" = "black")
# options("openxlsx.borderStyle" = "thin")
options("openxlsx.dateFormat" = "yyyy-mm-dd")
options("openxlsx.datetimeFormat" = "yyyy-mm-dd hh:mm:ss")
options("openxlsx.numFmt" = "GENERAL") #"#,##0_);[Red](#,##0)")
options("openxlsx.header" = NULL)
options("openxlsx.headerStyle" = NULL)
options("openxlsx.firstColumn" = NULL)
options("openxlsx.firstFotter" = NULL)
options("openxlsx.firstHeader" = NULL)
options("openxlsx.footer" = NULL)
options("openxlsx.evenHeader" = NULL)
options("openxlsx.evenFooter" = NULL)
options("openxlsx.gridLines" = TRUE)
options("openxlsx.keepNA" = TRUE)
options("openxlsx.na.string" = "-")
options("openxlsx.orientation" = "landscape")
options("openxlsx.showGridLines" = TRUE)
options("openxlsx.tabColour" = "black")



sheets <- c(
  "Cover",
  "Summary",
  "Detail",
  "Parameters"
)

purrr::walk(
  sheets,
  ~ openxlsx::addWorksheet(wb = pre_lease_wb, sheetName = .x)
)

openxlsx::insertImage(
  wb = pre_lease_wb,
  sheet = "Cover",
  file = gmh_logo_image,
  width = 5.00,
  height = 1.35,
  startRow = 1,
  startCol = 7
)

openxlsx::saveWorkbook(pre_lease_wb, "dev/pre_lease_summary_report.xlsx")

openxlsx::openXL(pre_lease_wb)

title_style <- openxlsx::createStyle(
  fontSize = 16,
  textDecoration = "bold",
  fgFill = "black",
  fontColour = "white",
  halign = "center",
  valign = "center",
  border = "TopBottomLeftRight"
)

subtitle_style <- openxlsx::createStyle(
  fontSize = 14,
  textDecoration = "bold",
  fgFill = "black",
  fontColour = "white",
  halign = "center",
  valign = "center",
  border = "TopBottomLeftRight"
)

table_header_style <- openxlsx::createStyle(
  fontSize = 12,
  fontColour = "white",
  numFmt = "text",
  border = c("top", "bottom", "left", "right"),
  borderColour = "black",
  borderStyle = "thin",
  bgFill = "black",
  fgFill = "black",
  halign = "center",
  valign = "center",
  textDecoration = "bold",
  wrapText = TRUE
)

table_body_style <- openxlsx::createStyle(
  fontSize = 12,
  halign = "center",
  valign = "center",
  border = "TopBottomLeftRight"
)

table_body_comma_format <- openxlsx::createStyle(
  fontSize = 12,
  numFmt = "#,##0_);[Red](#,##0)",
  halign = "center",
  valign = "center",
  border = "TopBottomLeftRight"
)

table_body_percent_format <- openxlsx::createStyle(
  fontSize = 12,
  numFmt = "0.0%;[Red](0.0%)",
  halign = "center",
  valign = "center",
  border = "TopBottomLeftRight"
)

current_pre_lease_period <- get_leasing_period() |> as.Date()
current_pre_lease_period_year_label <- paste0(
  substr(format(current_pre_lease_period[[1]], "%Y"), 3, 4),
  "-",
  substr(format(current_pre_lease_period[[2]], "%Y"), 3, 4)
)

prior_pre_lease_period <- get_leasing_period(lubridate::today() - lubridate::years(1)) |> as.Date()
prior__pre_lease_period_year_label <- paste0(
  substr(format(prior_pre_lease_period[[1]], "%Y"), 3, 4),
  "-",
  substr(format(prior_pre_lease_period[[2]], "%Y"), 3, 4)
)

paste0(prior__pre_lease_period_year_label, "Prior Year (Same Store)"


col_names <- c(
  "Property",
  "Investment Partner",
  "Total Beds",
  "Model Beds",
  "Current Occupancy %",
  "Total New",
  "Total Renewals",
  "Total Leases",
  paste0(current_pre_lease_period_year_label, " Pre-Lease %"),

)
