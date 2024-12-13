
#  ------------------------------------------------------------------------
#
# Title : Date Utilities
#    By : Jimmy Briggs
#  Date : 2024-12-01
#
#  ------------------------------------------------------------------------


# topic -------------------------------------------------------------------

#' Date Utilities
#'
#' @name utils_dates
#'
#' @description
#' Date utility functions.
#'
#' @details
#' The following functions are included:
#'
#' - `get_weeks_of_year()`: Get the (leasing) weeks of the year based off of
#'   the specified reporting cycle (i.e. what is the start of the week based from)
#'   and the current year.
#'
#' - `get_leasing_period_start_date()`: Get the start date of the leasing period
#'   based off of the specified date.
#' - `get_leasing_period_end_date()`: Get the end date of the leasing period
#'   based off of the specified date.
#'
#' - `get_weeks_left_to_lease()`: Get the number of weeks left in the leasing period
#'   based off of the specified date.
#'
#' - `get_weekly_period_start_date()`: Get the start date of the current week.
#' - `get_weekly_period_end_date()`: Get the end date of the current week.
#'
#' @param week_start The day of the week to start the week on. Default is `"Monday"`,
#'   but can also be one of `"Saturday"` or `"Sunday"`.
#' @param year The year to get the weeks of. Default is the current year.
#' @param as_of_date The date to use as the reference date. Default is today.
#' @param format The format to return the date in. Default is `"%m/%d/%Y"`
#'   which is the format required for most dates when using the Entrata API.
#'
#' @return
#' - `get_weeks_of_year()`: A list of weeks in the year based off of the specified
#'   week start and year.
#' - `get_leasing_period_start_date()`: The start date of the leasing period.
#' - `get_leasing_period_end_date()`: The end date of the leasing period.
#' - `get_weeks_left_to_lease()`: The number of weeks left in the leasing period.
#' - `get_weekly_period_start_date()`: The start date of the current week.
#' - `get_weekly_period_end_date()`: The end date of the current week.
#'
#' @family Utilities
#' @family Dates
#'
#' @seealso [entrata_properties()], [entrata_reports()], [entrata_config()]
NULL

# weekly ------------------------------------------------------------------

#' @rdname utils_dates
#' @export
#' @importFrom rlang arg_match
#' @importFrom lubridate year
get_weeks_of_year <- function(
    week_start = c("Monday", "Saturday", "Sunday"),
    year = lubridate::year(Sys.Date())
) {

  # validate week_start
  week_start <- rlang::arg_match(week_start, c("Monday", "Saturday", "Sunday"))

  # get all dates of the year
  all_dates <- seq(
    as.Date(
      paste0(
        year, "-01-01"
      )
    ),
    as.Date(
      paste0(
        year, "-12-31"
      )
    ),
    by = "day"
  )

  # get the dates of the week start
  week_starts <- dates[weekdays(dates) == week_start]

  # create list of weeks
  weeks <- lapply(week_starts, function(start) {
    end <- start + days(6)
    week_name <- paste("Week of", format(start, "%Y-%m-%d"))
    week_range <- c(format(start, "%Y-%m-%d"), format(end, "%Y-%m-%d"))
    setNames(list(week_range), week_name)
  })

  # merge and return
  do.call(c, weeks)

}

#' @rdname utils_dates
#' @export
get_weekly_period <- function(as_of_date = lubridate::today()) {

  if (is.null(as_of_date)) {
    as_of_date <- lubridate::today()
  }

  start <- get_weekly_period_start_date(as_of_date)
  end <- get_weekly_period_end_date(as_of_date)

  c(start, end)

}

#' @rdname utils_dates
#' @export
#' @importFrom lubridate floor_date today
get_weekly_period_start_date <- function(as_of_date = lubridate::today(), week_start = c("Monday", "Sunday")) {
  week_start <- rlang::arg_match(week_start, c("Monday", "Sunday"))
  hold <- lubridate::floor_date(as_of_date, "week")
  if (week_start == "Monday") {
    return(
      hold + lubridate::days(1)
    )
  } else {
    return(hold)
  }
}

#' @rdname utils_dates
#' @export
#' @importFrom lubridate ceiling_date today
get_weekly_period_end_date <- function(as_of_date = lubridate::today()) {
  get_weekly_period_start_date(as_of_date) + lubridate::days(6)
}


# leasing periods ---------------------------------------------------------

#' @rdname utils_dates
#' @export
get_leasing_period <- function(as_of_date = lubridate::today()) {

  start <- get_leasing_period_start_date(as_of_date, format = "%Y-%m-%d")
  end <- get_leasing_period_end_date(as_of_date, format = "%Y-%m-%d")

  c(start, end)

}

#' @rdname utils_dates
#' @export
#' @importFrom cli cli_abort
#' @importFrom lubridate is.Date today month make_date year
get_leasing_period_start_date <- function(
    as_of_date = lubridate::today(),
    format = "%m/%d/%Y"
) {

  if (!lubridate::is.Date(as_of_date)) {
    cli::cli_abort(
      "The 'date' argument must be a Date."
    )
  }

  if (as_of_date > lubridate::today()) {
    cli::cli_abort(
      "The 'date' argument must be a date in the past."
    )
  }

  if (!is.character(format)) {
    cli::cli_abort(
      "The 'format' argument must be a character string."
    )
  }

  if (lubridate::month(as_of_date) >= 9) {
    out <- lubridate::make_date(lubridate::year(as_of_date) + 1, 9, 1)
  } else {
    out <- lubridate::make_date(lubridate::year(as_of_date), 9, 1)
  }

  format(out, format) |>
    as.character()

}

#' @rdname utils_dates
#' @export
#' @importFrom cli cli_abort
#' @importFrom lubridate is.Date today month make_date year
get_leasing_period_end_date <- function(
    as_of_date = lubridate::today(),
    format = "%m/%d/%Y"
) {

  if (!lubridate::is.Date(as_of_date)) {
    cli::cli_abort(
      "The 'date' argument must be a Date."
    )
  }

  if (as_of_date > lubridate::today()) {
    cli::cli_abort(
      "The 'date' argument must be a date in the past."
    )
  }

  if (!is.character(format)) {
    cli::cli_abort(
      "The 'format' argument must be a character string."
    )
  }

  if (lubridate::month(as_of_date) >= 9) {
    out <- lubridate::make_date(lubridate::year(as_of_date) + 2, 8, 31)
  } else {
    out <- lubridate::make_date(lubridate::year(as_of_date) + 1, 8, 31)
  }

  format(out, format) |>
    as.character()

}

#' @rdname utils_dates
#' @export
#' @importFrom cli cli_abort
#' @importFrom lubridate is.Date today mdy today
get_weeks_left_to_lease <- function(as_of_date = lubridate::today()) {

  if (!lubridate::is.Date(as_of_date)) {
    cli::cli_abort(
      "The 'date' argument must be a Date."
    )
  }

  if (as_of_date > lubridate::today()) {
    cli::cli_abort(
      "The 'date' argument must be a date in the past."
    )
  }

  period_end_date <- get_leasing_period_end_date(as_of_date) |>
    lubridate::mdy()

  floor(as.numeric(period_end_date - as_of_date) / 7)

}

get_year <- function() {
  as.numeric(format(Sys.Date(), "%Y"))
}

#' End of Month
#'
#' @param as_of_date The date to use as the reference date. Default is today.
#'
#' @return The date of the last day of the month for the specified `as_of_date`.
#'
#' @export
#'
#' @importFrom lubridate is.Date ceiling_date as_date
end_of_month <- function(as_of_date) {
  if (!lubridate::is.Date(as_of_date)) as_of_date <- lubridate::as_date(as_of_date)
  lubridate::ceiling_date(as_of_date, unit = "month") - 1
}

#' Start of Month
#'
#' @param as_of_date The date to use as the reference date. Default is today.
#'
#' @return The date of the first day of the month for the specified `as_of_date`.
#'
#' @export
#'
#' @importFrom lubridate is.Date floor_date as_date
start_of_month <- function(as_of_date) {
  if (!lubridate::is.Date(as_of_date)) as_of_date <- lubridate::as_date(as_of_date)
  as.Date(format(as_of_date, "%Y-%m-01"))
}

#' Extract Date from String
#'
#' @param string The string to extract the date from.
#'
#' @return The extracted date.
#'
#' @export
#'
#' @importFrom lubridate mdy
#' @importFrom stringr str_extract_all
extract_date <- function(string) {
  paste0(
    unlist(
      stringr::str_extract_all(
        string,
        "[0-9]{1,2}[-./][0-9]{1,2}[-./][0-9]{2,4}"
      ),
      recursive = TRUE
    ),
    collapse = ""
  ) |>
    lubridate::mdy()
}

parse_http_date <- function(str) {
  check_string(str)
  withr::local_locale(LC_TIME = "C")
  # https://datatracker.ietf.org/doc/html/rfc7231#section-7.1.1.1
  out <- as.POSIXct(strptime(x, "%a, %d %b %Y %H:%M:%S", tz = "UTC"))
  attr(out, "tzone") <- NULL
  out
}

#' Elapsed Months
#'
#' @description
#' Derive the number of months elapsed between two dates.
#'
#' @param start_date Date representing the start of a period.
#' @param end_date Date representing the end of a period.
#'
#' @return The number of months elapsed between the two dates.
#'
#' @export
elapsed_months <- function(start_date, end_date) {
  end <- as.POSIXlt(end_date)
  start <- as.POSIXlt(start_date)
  12 * (end$year - start$year) + (end$mon - start$mon)
}


