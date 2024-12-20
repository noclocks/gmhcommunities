% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_market_survey.R
\name{mod_market_survey}
\alias{mod_market_survey}
\alias{mod_market_survey_ui}
\alias{mod_market_survey_server}
\alias{mod_market_survey_demo}
\title{Market Survey Shiny Module}
\usage{
mod_market_survey_ui(id)

mod_market_survey_server(
  id,
  pool = NULL,
  selected_property = NULL,
  selected_leasing_week = NULL
)

mod_market_survey_demo()
}
\arguments{
\item{id}{Module's namespace ID.}
}
\value{
\itemize{
\item \code{mod_market_survey_ui()}: UI HTML Output.
\item \code{mod_market_survey_server()}: Reactive values returned from server logic.
}
}
\description{
A Shiny Module for ...
\itemize{
\item \code{mod_market_survey_ui()}: User interface
\item \code{mod_market_survey_server()}: Server logic
}
}
\examples{
if (interactive()) {
  mod_market_survey_demo()
}
}
