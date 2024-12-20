% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db_config.R
\name{get_db_config}
\alias{get_db_config}
\title{Get Database Configuration}
\usage{
get_db_config(
  key = NULL,
  file = Sys.getenv("R_CONFIG_FILE", "config.yml"),
  config = Sys.getenv("R_CONFIG_ACTIVE", "default")
)
}
\arguments{
\item{key}{A character string representing the configuration key to retrieve.}

\item{file}{A character string representing the path to the configuration file.}

\item{config}{A character string representing the configuration to use from the
configuration file.}
}
\value{
A list, or vector (if \code{key} is specified), corresponding to the
contents of the \code{db} configuration key's values (i.e. \code{dbname}, \code{host},
\code{port}, \code{username}, and \code{password}).
}
\description{
This function retrieves the database configuration from the specified configuration
file and returns the configuration as a list. You can optionally specify a key
to retrieve a specific configuration value.
}
