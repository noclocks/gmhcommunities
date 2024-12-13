get_weeks_of_year <- function(year = lubridate::year(Sys.Date())) {

  dates <- seq(
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

  week_starts <- dates[weekdays(dates) == "Monday"]

  weeks <- lapply(week_starts, function(start) {
    end <- start + days(6)
    week_name <- paste("Week of", format(start, "%Y-%m-%d"))
    week_range <- c(format(start, "%Y-%m-%d"), format(end, "%Y-%m-%d"))
    setNames(list(week_range), week_name)
  })

  do.call(c, weeks)

}
