
#  ------------------------------------------------------------------------
#
# Title : Formatting Utilities
#    By : Jimmy Briggs
#  Date : 2024-12-10
#
#  ------------------------------------------------------------------------

#' Extract Phone Number
#'
#' @description
#' Extracts the phone number from a string and formats it for a "tel://" href.
#'
#' @param input_string A string containing a phone number
#'
#' @returns A string formatted for a "tel://" href
#'
#' @export
extract_phone_number <- function(input_string) {
  phone_digits <- gsub("[^0-9]", "", input_string)
  paste0("tel://", phone_digits)
}

#' Formats a phone number
#'
#' @description
#' This function formats phone numbers, and by default uses the syntax:
#' `+1 (555) 555-5555`.
#'
#' @param phone Phone number to format
#'
#' @returns String of formatted phone number
#'
#' @export
format_phone_number <- function(phone) {
  phone <- gsub("[^0-9]", "", phone)
  if (nchar(phone) == 11) {
    phone <- substr(phone, 2, 11)
  }
  paste0(
    "+1 (",
    substr(phone, 1, 3),
    ") ",
    substr(phone, 4, 6),
    "-",
    substr(phone, 7, 10)
  )
}

format_date <- function(date, format = "%Y-%m-%d", tz = "UTC") {

  date_formatter <- scales::label_date(
    format = format,
    tz = tz
  )

  date_formatter(date)

}

format_time <- function(time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC") {

  time_formatter <- scales::label_time(
    format = format,
    tz = tz
  )

  time_formatter(time)

}

format_http_time <- function(time) {
  withr::local_locale(LC_TIME = "C")
  strftime(time, "%a, %d %b %Y %H:%M:%S", tz = "UTC", usetz = TRUE)
}

format_local_time <- function(time, tz = "UTC") {
  out <- as.POSIXct(x, tz = tz)
  attr(out, "tzone") <- NULL
  out
}

format_unix_time <- function(time, tz = "UTC") {
  as.integer(as.POSIXct(time, tz = tz))
}

format_last_updated_at <- function(timestamp) {
  format(timestamp, "%A %B %d, %Y at %I:%M:%S %p")
}

short_date_formatter <- scales::label_date(
  format = "%b %d"
)

time_formatter <- scales::label_time(
  format = "%H:%M:%S",
  tz = "UTC"
)

integer_formatter <- scales::label_number(
  accuracy = 1,
  prefix = "",
  big.mark = ",",
  decimal.mark = ".",
  trim = TRUE,
  style_negative = "parens"
)

format_integer <- function(x, ...) {
  integer_formatter(x)
}

number_formatter <- scales::label_number(
  accuracy = 0.01,
  prefix = "",
  big.mark = ",",
  decimal.mark = ".",
  trim = TRUE,
  style_negative = "parens"
)

format_number <- function(x, ...) {
  number_formatter(x)
}

percent_formatter <- scales::label_percent(
  accuracy = 0.01,
  prefix = "",
  suffix = "%",
  big.mark = ",",
  decimal.mark = ".",
  trim = TRUE,
  style_negative = "parens"
)

format_percent <- function(x, ...) {
  percent_formatter(x)
}

percent_incr_decr_formatter <- function(x) {

  p <- icon_text(
    ifelse(x > 0, "arrow-up", "arrow-down"),
    format_percent(x)
  )

  formattable::formatter(
    "span",
    style = x ~ formattable::style(
      font.weight = "bold",
      color = ifelse(x > 0, "lightgreen", ifelse(x < 0, "red", "black"))
    ),
    p
  )
}

format_percent_incr_decr <- function(x, ...) {
  percent_incr_decr_formatter(x)(x)
}

currency_formatter <- scales::label_currency(
  accuracy = 0.01,
  prefix = "$",
  big.mark = ",",
  decimal.mark = ".",
  trim = TRUE,
  style_negative = "parens"
)

format_currency <- function(x, ...) {
  currency_formatter(x)
}

bytes_formatter <- scales::label_bytes(
  accuracy = 0.01,
  big.mark = ",",
  decimal.mark = ".",
  trim = TRUE
)

format_bytes <- function(x, ...) {
  bytes_formatter(x)
}


