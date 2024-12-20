% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/app_inputs.R
\name{leasing_week_input}
\alias{leasing_week_input}
\title{Custom Leasing Week Period Date Range Input}
\usage{
leasing_week_input(
  id,
  label = icon_text("calendar-alt", "Select Leasing Week"),
  value = NULL,
  inline = FALSE,
  ...
)
}
\arguments{
\item{id}{ID to use for the input.}

\item{label}{Display label for the control, or \code{NULL} for no label.}

\item{value}{Initial value(s), dates as character string are accepted in \code{yyyy-mm-dd} format,
or Date/POSIXct object. Can be a single value or several values.}

\item{inline}{If \code{TRUE}, datepicker will always be visible.}

\item{...}{
  Arguments passed on to \code{\link[shinyWidgets:airDatepicker]{shinyWidgets::airDatepickerInput}}
  \describe{
    \item{\code{inputId}}{The \code{input} slot that will be used to access the value.}
    \item{\code{multiple}}{Select multiple dates. If \code{TRUE}, then one can select unlimited dates.
If \code{numeric} is passed, then amount of selected dates will be limited by it.}
    \item{\code{range}}{Select a date range.}
    \item{\code{timepicker}}{Add a timepicker below calendar to select time.}
    \item{\code{separator}}{Separator between dates when several are selected, default to \code{" - "}.}
    \item{\code{placeholder}}{A character string giving the user a hint as to what can be entered into the control.}
    \item{\code{dateFormat}}{Format to use to display date(s), default to \code{yyyy-MM-dd},
see \href{https://air-datepicker.com/docs?scrollTo=dateFormat}{online documentation} for possible values.}
    \item{\code{firstDay}}{Day index from which week will be started. Possible values are from 0 to 6, where
0 - Sunday and 6 - Saturday. By default value is taken from current localization,
but if it passed here then it will have higher priority.}
    \item{\code{minDate}}{The minimum allowed date. Either a Date object, or a string in \code{yyyy-mm-dd} format.}
    \item{\code{maxDate}}{The maximum allowed date. Either a Date object, or a string in \code{yyyy-mm-dd} format.}
    \item{\code{disabledDates}}{A vector of dates to disable, e.g. won't be able to select one of dates passed.}
    \item{\code{disabledDaysOfWeek}}{Day(s) of week to disable, numbers from 0 (Sunday) to 6 (Saturday).}
    \item{\code{highlightedDates}}{A vector of dates to highlight.}
    \item{\code{view}}{Starting view, one of \code{'days'} (default), \code{'months'} or \code{'years'}.}
    \item{\code{startView}}{Date shown in calendar when date picker is opened.}
    \item{\code{minView}}{Minimal view, one of \code{'days'} (default), \code{'months'} or \code{'years'}.}
    \item{\code{monthsField}}{Names for the months when view is 'months',
use \code{'monthsShort'} for abbreviations or \code{'months'} for full names.}
    \item{\code{clearButton}}{If \code{TRUE}, then button "Clear" will be visible.}
    \item{\code{todayButton}}{If \code{TRUE}, then button "Today" will be visible to set view to current date,
if a \code{Date} is used, it will set view to the given date and select it..}
    \item{\code{autoClose}}{If \code{TRUE}, then after date selection, datepicker will be closed.}
    \item{\code{timepickerOpts}}{Options for timepicker, see \code{\link[shinyWidgets:timepickerOptions]{timepickerOptions()}}.}
    \item{\code{position}}{Where calendar should appear, a two word string like
\code{'bottom left'} (default), or \code{'top right'}, \code{'left top'}.}
    \item{\code{update_on}}{When to send selected value to server: on \code{'change'}
or when calendar is \code{'close'}d.}
    \item{\code{onlyTimepicker}}{Display only the time picker.}
    \item{\code{toggleSelected}}{When \code{TRUE}, in range mode, it's not possible to select the same date as start and end.}
    \item{\code{addon}}{Display a calendar icon to \code{'right'} or the \code{'left'}
of the widget, or \code{'none'}. This icon act like an \code{\link[shiny:actionButton]{shiny::actionButton()}},
you can retrieve value server-side with \verb{input$<inputId>_button}.}
    \item{\code{addonAttributes}}{A \code{list()} of additional attributes to use for the addon tag, like \code{class} for example.}
    \item{\code{language}}{Language to use, can be one of
\code{ar}, \code{cs}, \code{da}, \code{de}, \code{en}, \code{es}, \code{fi}, \code{fr}, \code{hu}, \code{it}, \code{ja}, \code{ko}, \code{nl},
\code{pl}, \code{pt-BR}, \code{pt}, \code{ro}, \code{ru}, \code{si}, \code{sk}, \code{sv}, \code{th}, \code{tr}, \code{uk}, \code{zh}.}
    \item{\code{tz}}{The timezone.}
    \item{\code{readonly}}{If \code{TRUE}, datepicker will be readonly and the input field won't be editable.}
    \item{\code{onkeydown}}{Attribute passed to the input field.}
    \item{\code{width}}{The width of the input, e.g. \code{'400px'}, or \verb{100\%}.}
  }}
}
\value{
\code{\link[htmltools:tagList]{htmltools::tagList()}} containing the custom leasing week input.
}
\description{
This custom input is designed to allow the user to select a date range
representing a leasing week period. The input is a datepicker that
allows the user to select a single date. The input will then automatically
select the entire week period (Monday to Sunday) that the selected date
falls within.

The input is built leveraging the \code{\link[shinyWidgets:airDatepicker]{shinyWidgets::airDatepickerInput()}} function.
}
\examples{
leasing_week_input("leasing_week")
}
