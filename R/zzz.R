
#  ------------------------------------------------------------------------
#
# Title : zzz.R - Package .onLoad / .onAttach
#    By : Jimmy Briggs
#  Date : 2024-11-26
#
#  ------------------------------------------------------------------------


# options -----------------------------------------------------------------

options::define_options(
  # database
  "Stores a database pooled connection object.",
  db.conn = NULL,
  "Database user integer ID to use for query logging.",
  db.user_id = 1,
  # entrata
  "Entrata API Base URL",
  entrata.base_url = get_entrata_config("base_url"),
  "Entrata API Username",
  entrata.username = get_entrata_config("username"),
  "Entrata API Password",
  entrata.password = get_entrata_config("password"),
  "Entrata API User Agent",
  entrata.user_agent = NULL,
  "Entrata API Timeout",
  entrata.timeout = 30,
  "Entrata API Retry On Status Codes",
  entrata.retry_on = c(429, 500, 502, 503, 504),
  "Entrata API Retry Max Attempts",
  entrata.max_retries = 3,
  "Entrata Request ID",
  entrata.request_id = NULL,
  "Entrata Verbosity",
  entrata.verbosity = NULL,
  "Entrata Default Headers",
  entrata.default_headers = NULL,
  "Entrata API Proxy",
  entrata.proxy = NULL
)

#' @eval options::as_roxygen_docs()
NULL

# onLoad ------------------------------------------------------------------

.onLoad <- function(libname, pkgname) { # nocov start

  logger::log_threshold(logger::INFO, namespace = "gmhcommunities")

  logger::log_layout(
    logger::layout_glue_generator(
      "{level} [{format(time, \"%Y-%m-%d %H:%M:%S\")}] {msg}"
    ),
    namespace = "gmhcommunities"
  )

  logger::log_formatter(
    logger::formatter_glue,
    namespace = "gmhcommunities"
  )

  logger::log_appender(
    appender = logger::appender_stdout,
    namespace = "gmhcommunities"
  )

} # nocov end

.onAttach <- function(libname, pkgname) {
  msg <- "gmhcommunities R package developed by No Clocks, LLC (https://github.com/noclocks/gmhcommunities/)"
  packageStartupMessage(msg)
}
