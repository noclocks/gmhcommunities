
#  ------------------------------------------------------------------------
#
# Title : Entrata Versions
#    By : Jimmy Briggs
#  Date : 2024-11-26
#
#  ------------------------------------------------------------------------


# r2 & r3 methods ---------------------------------------------------------

r2_methods <- c(
  "getLeases",
  "getLeaseDetails",
  "getReportInfo",
  "getDependentFilter"
)

r3_methods <- c(
  "getReportData"
)

# entrata_method_versions -------------------------------------------------

source("data-raw/src/entrata/methods.R")

assign_versions <- function(methods) {
  purrr::map(methods, ~ if (.x %in% r3_methods) {
    "r3"
  } else if (.x %in% r2_methods) {
    "r2"
  } else {
    "r1"
  })
}

entrata_method_versions <- purrr::imap(entrata_methods, ~ rlang::set_names(assign_versions(.x), .x))


# tibble ------------------------------------------------------------------

entrata_method_versions_tbl <- entrata_methods_tbl |>
  dplyr::mutate(
    version = dplyr::case_when(
      method %in% r2_methods ~ "r2",
      method %in% r3_methods ~ "r3",
      .default = "r1"
    )
  )
